#==============================================================================
# SI FIGURE: ENSEMBLE TOP-N ACCURACY
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Input paths
path_acc_teams <- "data/team-top-n-accuracy.csv"
path_acc_ens   <- "data/key-model-top-n-accuracy.csv"

# Output paths
out_path <- "figures/si_ensemble-top-n.png"

# Output parameters
output_width_in <- fig_width_single_wide_in
output_asp <- 0.7

# Define palettes, scales, themes
palette_model <- unname(palette_primary[c("yellow", "green1")])
scale_colour_model <- purrr::partial(scale_colour_manual,
                                     values = palette_model,
                                     name = "Model:")
theme_wide <- theme_base + theme(aspect.ratio = 1/2)

#------------------------------------------------------------------------------
# Import and process data
#------------------------------------------------------------------------------

# Import data
data_acc_ens   <- import_csv(path_acc_ens) %>%
  filter(grepl("Ensemble", source)) %>% select(-source)
data_acc_teams <- import_csv_matrix(path_acc_teams)

# Extract 1st-place accuracies
data_acc_geac_1st <- data_acc_teams[which.max(data_acc_teams[,10]),]

# Combine accuracies
data_acc_n <- data_acc_ens %>% rename(`GEAC Ensemble` = accuracy_top_n) %>%
  mutate(`GEAC (1st place)` = data_acc_geac_1st) %>%
  gather(model, accuracy_top_n, -n) %>%
  mutate(model = fct_inorder(model))

#------------------------------------------------------------------------------
# Make plot
#------------------------------------------------------------------------------

g_n <- ggplot(data_acc_n, aes(x=n, y=accuracy_top_n, colour = model)) +
  geom_line_thin() + geom_point_fixed() +
  scale_x_log10(name = "N") +
  scale_y_cont_nominor(name = "Top-N Accuracy (%)", limits = c(0.8, 1),
                       breaks = seq(0,1,0.05),
                       labels = function(y) y * 100) +
  scale_colour_model() + theme_wide

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_fig(out_path, g_n, output_width_in, output_asp)