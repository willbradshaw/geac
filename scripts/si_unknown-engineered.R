#==============================================================================
# SI FIGURE: UNKNOWN ENGINEERED
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Input paths
path_unkn <- "data/team-unknown-engineered-stats.csv"
path_freq <- "data/lab-counts.csv"

# Output paths
out_path <- "figures/si_unknown-engineered.png"

# Output parameters
output_width_in <- page_width_in
output_asp <- 0.8

# Define palettes, scales, themes
scale_shape_unkn <- purrr::partial(scale_shape_manual,
                                   name = "Data subset:",
                                   values = 16:18,
                                   guide = guide_legend())
theme_col_wide <- theme_col + theme(aspect.ratio = 1/2)
theme_wide <- theme_base + theme(aspect.ratio = 1/2)

#------------------------------------------------------------------------------
# Import and process data
#------------------------------------------------------------------------------

# Import Unk. Eng. data
data_unkn <- import_csv(path_unkn) %>%
  rename(ranking = prediction_track_ranking)

# Import lab count data & derive true Unk. Eng. frequency
data_freq <- import_csv(path_freq) %>% group_by(unknown_engineered) %>%
  summarise(n = sum(n_test)) %>% mutate(p = n/sum(n))
unk_eng_true_frequency <- data_freq %>% filter(unknown_engineered) %>%
  pull(p)

# Summarise Unk. Eng. data by decile
data_unkn_deciles <- data_unkn %>% group_by(ranking_decile, data_subset) %>%
  summarise(avg_accuracy_top10 = mean(accuracy_top10),
            avg_accuracy_top1  = mean(accuracy_top1),
            avg_unkn_top10 = mean(unk_eng_in_top10),
            avg_unkn_rank = geomean(unk_eng_avg_rank),
            rank_min = min(ranking),
            rank_max = max(ranking),
            label = paste(rank_min, rank_max, sep=" to "),
            label = fct_inorder(label),
            .groups = "drop")

#------------------------------------------------------------------------------
# Make plots
#------------------------------------------------------------------------------

aes_unkn_pt <- purrr::partial(aes, colour = data_subset, shape = data_subset,
                              x = ranking)
aes_unkn_dc <- purrr::partial(aes, fill = data_subset, x = label)

# Top-10 accuracy
g_unkn_acc_top10 <- ggplot(data_unkn, aes_unkn_pt(y=accuracy_top10)) +
  geom_line_thin() + geom_point(size = 1) + 
  scale_x_ranking_all_trunc() +
  scale_y_percent(name = "Top-10 accuracy (%)") + 
  scale_colour_unkn() + scale_shape_unkn() + theme_wide

# Top-1 accuracy
g_unkn_acc_top1 <- ggplot(data_unkn, aes_unkn_pt(y=accuracy_top1)) +
  geom_line_thin() + geom_point(size = 1) + 
  scale_x_ranking_all_trunc() +
  scale_y_percent(name = "Top-1 accuracy (%)") + 
  scale_colour_unkn() + scale_shape_unkn() + theme_wide

# Rate of Unknown Engineered in top-10
g_unkn_in_top10 <- ggplot(data_unkn_deciles, aes_unkn_dc(y=avg_unkn_top10)) +
  geom_col_mid(position = "dodge") + 
  scale_x_discrete(name = "Prediction Track ranking") +
  geom_hline(yintercept = unk_eng_true_frequency, colour = palette_secondary["red"],
             linetype = "dashed", size = 0.3) +
  scale_y_percent(name = "Mean frequency of\nUnk. Eng. in top 10 guesses") + 
  scale_fill_unkn() + theme_col_wide

# Average rank of unknown engineered
g_unkn_rank <- ggplot(data_unkn_deciles, aes_unkn_dc(y=avg_unkn_rank)) +
  geom_col_mid(position = "dodge") + 
  scale_x_discrete(name = "Prediction Track ranking") +
  scale_y_log10(name = "Geometric mean of\nUnk. Eng. ranking",
                       expand = c(0,0)) + 
  scale_fill_unkn() + theme_col_wide

g_unkn_out <- wrap_plots(g_unkn_in_top10 + guides(fill = FALSE),
                         g_unkn_rank + guides(fill = FALSE),
                         g_unkn_acc_top10, g_unkn_acc_top1, ncol = 2) +
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") &
  theme_legend

#-----------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_fig(out_path, g_unkn_out, output_width_in, output_asp)
