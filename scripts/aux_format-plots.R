#==============================================================================
# Packages
#==============================================================================

load_packages <- function(package_list){
  for (p in package_list){
    suppressMessages(suppressWarnings(library(p, character.only = TRUE)))
  }
}

packages_default <- c("tidyverse", "cowplot", "patchwork", "RColorBrewer",
                      "ggpubr")

load_packages(packages_default)

#==============================================================================
# Auxiliary functions
#==============================================================================

adjust_lightness <- function(palette, lightnesses){
  # Adjust the lightnesses of a colour palette, while keeping hue and
  # saturation unchanged
  palette_hsl <- plotwidgets::col2hsl(palette)
  palette_hsl[3,] <- lightnesses
  return(plotwidgets::hsl2col(palette_hsl))
}

geomean <- function(x){
  # Simple geometric mean function
  exp(mean(log(x), na.rm = TRUE))
}

import_csv_matrix <- function(path){
  # Import a CSV matrix (e.g. of predictions or ranks, without headers) into R
  path %>% read_csv(col_names = FALSE, col_types = cols(), progress = TRUE) %>%
    as.matrix %>% unname
}

import_csv <- purrr::partial(read_csv, col_types = cols(), progress = TRUE)
import_tsv <- purrr::partial(read_tsv, col_types = cols(), progress = TRUE)

save_fig <- function(path, plot, width, aspect){
  cowplot::save_plot(path, plot, ncol = 1, nrow = 1, base_width = width,
                     base_height = width * aspect)
}

#==============================================================================
# Plotting theme information
#==============================================================================

# Import standard themes
source("scripts/aux_plot-theme.R")

# Bar chart with x-axis and external legend
theme_col <- theme_base + theme(
  axis.title.x.bottom = element_text(margin = lmargin(t=0.7), 
                                     vjust = 1),
  axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1,
                             margin = lmargin(t=0.3),
                             size = fontsize_base),
) # Can drop x-axis title by modifying axis.title.x.bottom

#==============================================================================
# Palettes
#==============================================================================

#------------------------------------------------------------------------------
# Primary palette (Set2)
#------------------------------------------------------------------------------

# Colour names
palette_primary_names <- c("green1", "orange", "blue", "pink", "green2",
                           "yellow", "beige", "grey")

# Raw colour codes
palette_primary <- setNames(brewer.pal(8, "Set2"), palette_primary_names)
palette_primary_pale   <- setNames(brewer.pal(8, "Pastel2"),
                                   palette_primary_names)
palette_primary_dark   <- setNames(brewer.pal(8, "Dark2"),
                                   palette_primary_names)

# Fix pastel grey (too dark to read text)
palette_primary_pale["grey"] <- theme_base$legend.box.background$fill

# Intermediate blue & grey values
blue_mid <- adjust_lightness(palette_primary["blue"], 0.8)
grey_mid <- adjust_lightness(palette_primary["grey"], 0.8)
palette_primary_mid <- setNames(c(blue_mid, grey_mid),
                                c("blue", "grey"))

# Grey intermediate between normal and dark
grey_darkmid <- adjust_lightness(palette_primary["grey"], 0.6)
palette_primary_darkmid <- setNames(c(grey_darkmid), c("grey"))

# Darker normal blue (to distinguish from mid-blue)
palette_primary["blue"] <- adjust_lightness(palette_primary["blue"], 0.62)

#------------------------------------------------------------------------------
# Secondary palette (Set1)
#------------------------------------------------------------------------------

# Colour names
palette_secondary_names <- c("red", "blue", "green", "purple", "orange",
                           "yellow", "brown", "pink", "grey")

# Preferred lightnesses
palette_secondary_lightness <- c(0.63, 0.63, 0.63, 0.63, 
                                 0.73, 0.45, 0.63, 0.73, 0.63)
palette_secondary_darkness <- c(0.53, 0.53, 0.53, 0.53,
                                0.63, 0.35, 0.53, 0.63, 0.53)

# Raw colour codes
palette_secondary <- brewer.pal(9, "Set1") %>%
  adjust_lightness(palette_secondary_lightness) %>%
  setNames(palette_secondary_names)
palette_secondary_dark <- palette_secondary %>% 
  adjust_lightness(palette_secondary_darkness) %>%
  setNames(palette_secondary_names)

#------------------------------------------------------------------------------
# Palettes for specific purposes
#------------------------------------------------------------------------------

# Colour by competition ranking (1, 2, 3, 4, Other)
palette_winners <- unname(c(palette_primary[c(1,2,4,3)], 
                            palette_primary_mid["blue"]))

# Colour by top-N accuracy metric (top-1, -5, -10, -20)
names_palette_accuracy <- c("red", "brown", "purple", "grey")
#names_palette_accuracy <- c("red", "green", "purple", "orange")
palette_accuracy <- unname(palette_secondary[names_palette_accuracy])

# Colour by X-metric (X99, X95, X90, X80)
names_palette_x <- c("orange", "pink", "blue", "green")
palette_x <- unname(palette_secondary[names_palette_x])
palette_x_dark <- unname(palette_secondary_dark[names_palette_x])

# Colour by data subset (all labs, known labs only, Unk Eng only)
palette_unkn <- unname(c(palette_secondary["blue"],
                         palette_secondary["pink"],
                         palette_primary_dark["grey"]))

#==============================================================================
# Common scales
#==============================================================================

#------------------------------------------------------------------------------
# X/Y
#------------------------------------------------------------------------------

# Remove minor breaks
scale_x_cont_nominor <- purrr::partial(scale_x_continuous, minor_breaks = NULL)
scale_y_cont_nominor <- purrr::partial(scale_y_continuous, minor_breaks = NULL)
scale_x_log_nominor <- purrr::partial(scale_x_log10, minor_breaks = NULL)
scale_y_log_nominor <- purrr::partial(scale_y_log10, minor_breaks = NULL)

# Proportions and percentages
scale_x_proportion <- purrr::partial(scale_x_cont_nominor,
                                     breaks = seq(0,1,0.2),
                                     limits = c(0,1),
                                     expand = c(0,0))
scale_x_percent <- purrr::partial(scale_x_proportion,
                                  labels = function(x) as.integer(x * 100))
scale_y_proportion <- purrr::partial(scale_y_cont_nominor,
                                     breaks = seq(0,1,0.2),
                                     limits = c(0,1),
                                     expand = c(0,0))
scale_y_percent <- purrr::partial(scale_y_proportion,
                                  labels = function(y) as.integer(y * 100))

# Competition rankings (either all, or just top 100)
scale_x_ranking <- purrr::partial(scale_x_cont_nominor,
                                  expand = c(0.02,0.02),
                                  name = "Prediction Track ranking")
scale_x_ranking_all <- purrr::partial(scale_x_ranking,
                                      breaks = c(1,seq(40,300,40), 299))
scale_x_ranking_all_trunc <- purrr::partial(scale_x_ranking,
                                            breaks = c(1,seq(40,300,40)))
scale_x_ranking_100 <- purrr::partial(scale_x_ranking,
                                      breaks = c(1,seq(20,100,20)),
                                      limits = c(0,100))

#------------------------------------------------------------------------------
# Colour/fill
#------------------------------------------------------------------------------

# Colour by competition ranking (1, 2, 3, 4, Other)
scale_colour_winners_raw <- purrr::partial(scale_colour_manual,
                                           values = palette_winners,
                                           guide = guide_legend(override.aes = list(size = 1.5)))
scale_fill_winners_raw <- purrr::partial(scale_fill_manual,
                                         values = palette_winners)
scale_colour_winners <- purrr::partial(scale_colour_winners_raw,
                                       name = "Prediction Track ranking:")
scale_fill_winners <- purrr::partial(scale_fill_winners_raw,
                                     name = "Prediction Track ranking:")

# Colour by top-N accuracy metric (top-1, -5, -10, -20)
scale_colour_accuracy_raw <- purrr::partial(scale_colour_manual,
                                            values = palette_accuracy)
scale_fill_accuracy_raw <- purrr::partial(scale_fill_manual,
                                          values = palette_accuracy)
scale_colour_accuracy <- purrr::partial(scale_colour_accuracy_raw,
                                        name = "Accuracy metric:")
scale_fill_accuracy <- purrr::partial(scale_fill_accuracy_raw,
                                      name = "Accuracy metric:")

# Colour by X-metric (X99, X95, X90, X80)
scale_colour_x <- purrr::partial(scale_colour_manual,
                                 name = "Metric:",
                                 values = palette_x)
scale_colour_x_dark <- purrr::partial(scale_colour_manual,
                                      name = "Metric:",
                                      values = palette_x_dark)
scale_fill_x <- purrr::partial(scale_fill_manual,
                               name = "Metric:",
                               values = palette_x)

# Colour by data subset (all labs, known labs only, Unk Eng only)
scale_colour_unkn <- purrr::partial(scale_colour_manual,
                                    name = "Data subset:",
                                    values = palette_unkn,
                                    guide = guide_legend())
scale_fill_unkn <- purrr::partial(scale_fill_manual,
                                  name = "Data subset:",
                                  values = palette_unkn,
                                  guide = guide_legend())

#------------------------------------------------------------------------------
# Shapes
#------------------------------------------------------------------------------

scale_shape_multi <- purrr::partial(scale_shape_manual,
                                    values = c(16,15,17,18,16))

#==============================================================================
# Common geoms
#==============================================================================
# Share default line thicknesses, etc., across scripts for consistency

# Lines
geom_line_thin <- purrr::partial(geom_line, size = 0.2)

# Bars
geom_col_mid <- purrr::partial(geom_col, width = 0.8)

# Points
geom_point_fixed <- purrr::partial(geom_point, shape = 16)
geom_point_winners <- purrr::partial(geom_point_fixed, size = 1.2)
geom_point_nonwinners <- purrr::partial(geom_point_fixed, size = 1)

# Spearman correlations
stat_spear <- purrr::partial(stat_cor, method = "spearman",
                             colour = "black", cor.coef.name = "rho", 
                             size = rescale_font(fontsize_base), family = font)

#==============================================================================
# Common parameters
#==============================================================================

#------------------------------------------------------------------------------
# Image widths
#------------------------------------------------------------------------------
# In order to get the text size right, images need to be exported at their
# actual size, so I'm specifying the main sizes here

page_width_in <- 6.67 # Width of document page (excluding margins)
fig_width_large_in <- page_width_in # Full-width figures (default)
fig_width_single_wide_in <- page_width_in * 0.75 # Wide single-panel figures
fig_width_single_narrow_in <- page_width_in * 0.6 # Wide single-panel figures

#------------------------------------------------------------------------------
# Basic competition parameters
#------------------------------------------------------------------------------

n_winners <- 4
n_teams <- 299
n_labs <- 1314
n_labs_unique <- 1313