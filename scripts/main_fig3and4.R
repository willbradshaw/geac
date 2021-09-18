#==============================================================================
# MAIN TEXT FIGURE 3 & 4: X-metrics
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Input paths
path_accuracy <- "data/team-top-n-accuracy.csv"
path_teams <- "data/team-stats.csv"
path_key <- "data/key-model-stats.csv"

# Output paths
out_path_fig3_main <- "figures/main_fig3.png"
out_path_fig4_main <- "figures/main_fig4.png"
out_path_x_all <- "figures/si_xmetrics-all-submissions.png"

# Parameters
example_team_rank <- 126 # Ranking of team to use for example plot
tag_margin_fig3 <- lmargin(t = 0.2, r = 0.2, b = -0.5, l = 0.3) # Margin of subfigure labels
tag_margin_fig4 <- lmargin(t = 0, r = 0.2, b = 0.5, l = 0.3)
source_names_raw <- c("BLAST (new)", "BLAST (Alley et al., original)",
                      "BLAST (Alley et al., corrected)", "Nielsen & Voigt (2018)", "Alley et al. (2020)",
                      "GEAC (1st place)", "GEAC (2nd place)", "GEAC (3rd place)",
                      "GEAC (4th place)", "GEAC Ensemble")
source_names_plot <- c(NA, NA, "BLAST", "Nielsen & Voigt (2018)",
                       "Alley et al. (2020)", "GEAC (1st place)", NA, NA, NA,
                       "GEAC Ensemble")

# Output parameters
out_width_main <- page_width_in
out_width_si   <- fig_width_single_wide_in
out_asp_main   <- 0.45
out_asp_si     <- 0.7

# Palettes
palette_nlabs <- unname(palette_primary_darkmid["grey"])
palette_example <- unname(palette_primary["green2"])

# Factor levels
x_levels <- c("X99", "X95", "X90", "X80")
win_levels <- c(1:n_winners, "Other")
outlier_levels <- c(TRUE, FALSE) # Fig. 4 outliers
source_levels <- source_names_plot[!is.na(source_names_plot)]

# Scales
scale_shape_x <- purrr::partial(scale_shape_multi,
                                name = "Metric:")

#------------------------------------------------------------------------------
# Read data
#------------------------------------------------------------------------------

# Import accuracy and team stats
data_accuracy <- import_csv_matrix(path_accuracy)
data_teams    <- import_csv(path_teams)

# Import, rename and filter key sources
tab_sources <- tibble(source = source_names_raw,
                      source_new = source_names_plot)
data_key <- import_csv(path_key) %>%
    inner_join(tab_sources, by = "source") %>%
    filter(!is.na(source_new)) %>% select(-source) %>%
    select(source = source_new, everything()) %>%
    mutate(source = factor(source, levels = source_levels)) %>%
    arrange(source)

#-----------------------------------------------------------------------------
# X95 overview plot (example team)
#------------------------------------------------------------------------------

# Get example data
tab_example <- filter(data_teams, 
                      prediction_track_ranking == example_team_rank)
which_accuracy <- which(data_accuracy[,10] == tab_example$accuracy_top10 &
                            data_accuracy[,5] == tab_example$accuracy_top5)
accuracies_example <- tibble(N = 1:ncol(data_accuracy),
                             accuracy = data_accuracy[which_accuracy,])

# Extract annotation data
annot_scale <- 1.15
tab_annot <- tibble(
    y = c(0.95, 0.99),
    x = c(tab_example$X95, tab_example$X99),
    colour = palette_x[c(2,1)],
    label = c("X95", "X99"),
    hjust = c(1,0)
) %>% mutate(x_scaled = x * c(1/annot_scale, annot_scale))

# Plot helper functions & parameters
geom_seg <- purrr::partial(geom_segment, size = 0.3, linetype = "dashed",
                           data = tab_annot)

#ggplot
g_example <- ggplot(accuracies_example, aes(x = N, y = accuracy))  +
    labs(x = "N", y = "Top-N Accuracy (%)", title = "") +
    geom_line_thin(color = palette_example[1]) +
    geom_point_fixed(color = palette_example[1], size = 1) +
    geom_seg(aes(xend = x, y = y, yend = y, colour = colour), x = -Inf) +
    geom_seg(aes(yend = y, x = x, xend = x, colour = colour), y = -Inf) +
    geom_text(aes(x = x_scaled, colour = colour, hjust = hjust, 
                  label = label), data = tab_annot, y = 0.51,
              size = rescale_font(fontsize_base), vjust = 0) +
    scale_y_cont_nominor(labels = function(x) as.integer(x*100),
                         breaks = seq(0,1,0.1), limits = c(0.5, 1),
                         expand = c(0,0.01)) +
    scale_x_log10(limits = c(NA,2500)) +
    scale_colour_identity() +
    theme_base + theme(legend.position = "none",
                       plot.tag = element_text(margin = tag_margin_fig3))

#-----------------------------------------------------------------------------
# XN scores plot (main figure)
#------------------------------------------------------------------------------

# Prepare dataset
tab_exclusion <- data_teams %>% rename(ranking = prediction_track_ranking) %>%
    select(ranking, starts_with("X")) %>%
    gather(metric, value, -ranking) %>%
    mutate(metric = factor(metric, levels = x_levels))

# Make plot
g_exclusion_100 <- ggplot(filter(tab_exclusion, metric %in% c("X99", "X95")),
                          aes(x = ranking, y = value, color = metric,
                              shape = metric)) +
    geom_point(size = 1) +
    geom_hline(yintercept = n_labs, colour = palette_nlabs[1],
               linetype = "dashed", size = 0.3) +
    scale_x_ranking_100() +
    scale_y_log10(name = "N", minor_breaks = NULL, limits = c(1, n_labs*1.1),
                  expand = c(0,0)) +
    scale_colour_x() + scale_shape_x() +
    theme_base + theme(plot.tag = element_text(margin = tag_margin_fig3))

#-----------------------------------------------------------------------------
# XN scores plot (SI)
#------------------------------------------------------------------------------

# Make plot
g_exclusion_all <- ggplot(tab_exclusion,
                          aes(x = ranking, y = value, color = metric,
                              shape = metric)) +
    geom_point(size = 1) +
    geom_hline(yintercept = n_labs, colour = palette_nlabs[1],
               linetype = "dashed", size = 0.3) +
    scale_x_ranking_all() +
    scale_y_log10(name = "N", limits = c(1, NA),
                  expand = c(0,0)) +
    scale_colour_x() + scale_shape_x() +
    theme_base + theme(plot.tag = element_text(margin = tag_margin_fig3),
                       aspect.ratio = 1/2)

#------------------------------------------------------------------------------
# Key model XN (summary, full in SI from another script)
#------------------------------------------------------------------------------

df <- data_key %>% select(source, X99, X95) %>% gather(metric, N, -source) %>%
    mutate(metric = factor(metric, levels = x_levels))

df_annot <- data_key %>% select(source, N=X99) %>%
    filter(grepl("GEAC", source)) %>% 
    mutate(metric="X99")

g_prior <- ggplot(df, aes(x = source, y = N, fill = metric)) +
    geom_col_mid(position = "dodge") +
    geom_hline(yintercept = n_labs, colour = palette_nlabs[1],
               linetype = "dashed", size = 0.3) +
    annotate("text", x = Inf, y = n_labs-40, label = "All labs",
             size = rescale_font(fontsize_base), hjust = 1, vjust = 1,
             colour = palette_nlabs[1]) +
    geom_text(aes(colour = metric, label = N), data = df_annot,
              size = fontsize_base_rescaled, angle = 45,
              hjust = 1, vjust = 1, nudge_y = 320, nudge_x = 0.05,
              show.legend = FALSE) +
    scale_y_cont_nominor(name = "N", limits = c(0,1400),
                         breaks = seq(0,2000,400), expand = c(0,0)) +
    scale_fill_x() + scale_colour_x_dark() +
    theme_col + theme(axis.title.x.bottom = element_blank(),
                      plot.tag = element_text(margin = tag_margin_fig3))

#------------------------------------------------------------------------------
# Linear exclusion plots (Fig. 4)
#------------------------------------------------------------------------------

# Define outlier values
outlier_min_100 <- list(X95 = n_labs-20, X99 = n_labs-20)

# Add winner annotations to exclusion data
tab_exclusion_linear <- tab_exclusion %>% 
    filter(metric %in% c("X99", "X95")) %>%
    mutate(winning_rank = ifelse(ranking <= n_winners, ranking, "Other"),
           winning_rank = factor(winning_rank, levels = win_levels))

# Split by metric
tab_el_split <- tab_exclusion_linear %>%
    group_by(metric) %>% group_split() %>%
    setNames(sapply(., function(x) first(x$metric))) %>%
    lapply(., function(x) select(x, -metric))

# Filter to top-100
tab_el_split_100 <- lapply(tab_el_split, function(x) filter(x, ranking <= 100))

# Assign outlier status
tab_els_outliers_100 <- lapply(names(tab_el_split_100), function(x)
    tab_el_split_100[[x]] %>% 
        mutate(outlier = value >= outlier_min_100[[x]],
               outlier = factor(outlier, levels = outlier_levels))) %>%
    setNames(names(tab_el_split_100))

# Define basic plotting function (without axis scales)
plot_exc_linear <- function(data, metric, outlier_pad = 5,
                            force_values = 0){
    # Split data by winner status
    data_winners <- data %>% filter(winning_rank != "Other")
    data_nonwinners <- data %>% filter(winning_rank == "Other")
    # Generate outlier padding data (to prevent zero-height facets)
    # Make plot
    a <- aes(x=ranking, y=value, colour=winning_rank)
    g <- ggplot(data = NULL, mapping = a) +
        geom_point_nonwinners(data = data_nonwinners) +
        geom_point_winners(data = data_winners) +
        facet_grid(outlier~., scales = "free_y", space = "free_y") +
        scale_colour_winners() + labs(y = metric) +
        theme_base + 
        theme(strip.text = element_blank(), aspect.ratio = NULL,
              plot.tag = element_text(margin = tag_margin_fig4))
    # Force particular values if needed
    if (length(force_values) > 0){
        data_force <- tibble(ranking = n_winners + 1, value = force_values,
                             outlier = factor(FALSE, levels = outlier_levels),
                             winning_rank = "Other")
        g <- g + geom_point_nonwinners(data = data_force, alpha = 0)
    }
    # Add outlier padding if needed
    if (sum(data$outlier == TRUE) > 0){
        data_outliers <- data %>% filter(outlier == TRUE)
        pad_values <- c(min(data_outliers$value)-outlier_pad,
                        max(data_outliers$value)+outlier_pad)
        data_outlier_pad <- tibble(ranking = n_winners + 1, value = pad_values,
                                   outlier = factor(TRUE, levels = outlier_levels),
                                   winning_rank = "Other")
        g <- g + geom_point_nonwinners(data = data_outlier_pad, alpha = 0)
    }
    return(g)
}

# Make top-100 plots
outlier_pad_100 <- list(X95 = 50/3, X99 = 50)
y_breaks_100 <- list(X95 = c(seq(0, n_labs-100, 100), n_labs),
                     X99 = c(seq(0, n_labs, 300), n_labs))
force_values_100 <- list(X95 = c(0,400), X99 = c(0,1200))
g_exc_linear_100 <- lapply(names(tab_els_outliers_100), function (x)
    plot_exc_linear(tab_els_outliers_100[[x]], x, outlier_pad_100[[x]],
                    force_values_100[[x]]) +
        scale_x_ranking_100() + 
        scale_y_cont_nominor(breaks = y_breaks_100[[x]],
                             expand = c(0,0))) %>%
    setNames(names(tab_els_outliers_100))

#-----------------------------------------------------------------------------
# Bring plots together
#------------------------------------------------------------------------------

# Fig. 3
fig3 <- wrap_plots(g_example, g_exclusion_100, g_prior + guides(fill = FALSE),
                   ncol = 3) +
    plot_annotation(tag_levels = 'a') + plot_layout(guides = "collect") &
    theme_legend & theme(legend.box.spacing = lines(-0.3),
                         plot.margin = lmargin(c(0, 0.1, 0.1, 0)))

# Fig. 4 main
fig4 <- wrap_plots(g_exc_linear_100[["X99"]], g_exc_linear_100[["X95"]],
                   ncol = 2) +
    plot_annotation(tag_levels = 'a') + plot_layout(guides = "collect") &
    theme_legend & theme(plot.margin = lmargin(c(0.2, 0.2, 0.1, 0)))

#-----------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_fig(out_path_fig3_main, fig3, out_width_main, out_asp_main)
save_fig(out_path_fig4_main, fig4, out_width_main, out_asp_main)
save_fig(out_path_x_all, g_exclusion_all, out_width_si, out_asp_si)
