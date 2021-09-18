#==============================================================================
# SI FIGURE: CORRELATIONS BETWEEN ACCURACY AND X METRICS
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Input path
path_teams    <- "data/team-stats.csv"

# Output paths
out_path_acc <- "figures/si_accuracy-correlations.png"
out_path_x   <- "figures/si_xmetric-correlations.png"
out_path_mix_all <- "figures/si_mix-correlations-all.png"
out_path_mix_filtered <- "figures/si_mix-correlations-filtered.png"

# Output parameters
out_width <- page_width_in
out_asp   <- 1.05

# Themes & scales
theme_cor <- theme_base + theme(
  axis.title = element_blank(),
  strip.text = element_text(margin = lmargin(rep(0.2, 4))),
  aspect.ratio = 1,
  plot.margin = lmargin(0.3, 0.4, 0.4, 0.4),
)
scale_x_acc <- purrr::partial(scale_x_continuous, breaks = seq(0,1,0.2),
                              minor_breaks = NULL,
                              labels = function(x) round(x*100))
scale_y_acc <- purrr::partial(scale_y_continuous, breaks = seq(0,1,0.2),
                              minor_breaks = NULL,
                              labels = function(x) round(x*100))
scale_x_nlabs <- purrr::partial(scale_x_cont_nominor, limits = c(0,n_labs),
                                breaks = seq(0,2000,400),
                                expand = c(0,0))
scale_y_nlabs <- purrr::partial(scale_y_cont_nominor, limits = c(0,n_labs),
                                breaks = seq(0,2000,400),
                                expand = c(0,0))

#------------------------------------------------------------------------------
# Import data
#------------------------------------------------------------------------------

# Team data
data_teams <- suppressMessages(read_csv(path_teams))

#------------------------------------------------------------------------------
# Part 1: Intra-competition accuracy correlations
#------------------------------------------------------------------------------

# Types of accuracy metric
tab_types <- tibble(accuracy_type_base = data_teams %>% 
                      select(starts_with("accuracy")) %>% colnames) %>%
  mutate(accuracy_type_nice = paste(sub("accuracy_top", "Top-", 
                                        accuracy_type_base), "\nAccuracy (%)"),
         accuracy_type_nice_asc = fct_inorder(accuracy_type_nice),
         accuracy_type_nice_desc = fct_rev(accuracy_type_nice_asc)) %>%
  select(-accuracy_type_nice)

# Combinations of metrics to compare
tab_acc_base <- expand_grid(ranking = data_teams$prediction_track_ranking,
                            accuracy_type_1 = tab_types$accuracy_type_base,
                            accuracy_type_2 = tab_types$accuracy_type_base) %>%
  mutate(winning_rank = ifelse(ranking <= n_winners, ranking, "Other"))

# Add nice accuracy names
tab_acc_nice <- tab_acc_base %>%
  inner_join(tab_types,
             by = c("accuracy_type_1" = "accuracy_type_base")) %>% 
  rename(accuracy_nice_1 = accuracy_type_nice_desc) %>%
  select(-accuracy_type_nice_asc) %>%
  inner_join(tab_types,
             by = c("accuracy_type_2" = "accuracy_type_base")) %>% 
  rename(accuracy_nice_2 = accuracy_type_nice_asc) %>%
  select(-accuracy_type_nice_desc)
  
# Add accuracy values
tab_teams_melt <- data_teams %>% 
  select(ranking = prediction_track_ranking, starts_with("accuracy")) %>%
  gather(accuracy_type, accuracy, -ranking)
tab_acc <- tab_acc_nice %>%
  inner_join(tab_teams_melt,
             by = c("ranking" = "ranking", 
                    "accuracy_type_1" = "accuracy_type")) %>% 
  rename(accuracy_1 = accuracy) %>%
  inner_join(tab_teams_melt,
             by = c("ranking" = "ranking",
                    "accuracy_type_2" = "accuracy_type")) %>% 
  rename(accuracy_2 = accuracy)

# Split into winners/nonwinners
tab_acc_win <- tab_acc %>% filter(winning_rank != "Other")
tab_acc_nonwin <- tab_acc %>% filter(winning_rank == "Other")

# Make plots
aes_col <- aes(colour=winning_rank)
stat_spear_no_p <- purrr::partial(stat_spear, digits = 3,
                                  mapping = aes(label = ..r.label..))
stat_aspear <- purrr::partial(stat_spear_no_p, label.x = 0.05, label.y = 0.95,
                              vjust = 1, hjust = 0, data = tab_acc)

g_acc_cor <- ggplot(aes(x=accuracy_1, y=accuracy_2), data = NULL) +
  geom_abline(size = 0.3, colour = palette_primary["grey"]) +
  geom_point_nonwinners(data=tab_acc_nonwin, alpha=0.7, mapping=aes_col) +
  geom_point_winners(data=tab_acc_win, alpha=1, mapping=aes_col) +
  stat_aspear() +
  facet_grid(accuracy_nice_1 ~ accuracy_nice_2, switch = "both") +
  scale_x_acc() + scale_y_acc() + scale_colour_winners() +
  theme_cor

#------------------------------------------------------------------------------
# Part 2: Inter-X-metric correlations
#------------------------------------------------------------------------------

# Combinations of metrics to compare
x_levels <- c("X99", "X95", "X90", "X80")
x_types  <- fct_inorder(x_levels)
x_types_rev <- fct_inorder(rev(x_levels))
tab_x_base <- expand_grid(ranking = data_teams$prediction_track_ranking,
                            x_type_1 = x_types, x_type_2 = x_types_rev) %>%
  mutate(winning_rank = ifelse(ranking <= n_winners, ranking, "Other"))

# Add values
tab_teams_melt_x <- data_teams %>% 
  select(ranking = prediction_track_ranking, starts_with("X")) %>%
  gather(x_type, N, -ranking)
tab_x <- tab_x_base %>%
  inner_join(tab_teams_melt_x, 
             by = c("ranking" = "ranking", 
                    "x_type_1" = "x_type")) %>% 
  rename(N1 = N) %>%
  inner_join(tab_teams_melt_x, 
             by = c("ranking" = "ranking", 
                    "x_type_2" = "x_type")) %>% 
  rename(N2 = N) %>%
  mutate(x_type_1 = factor(x_type_1, levels = x_levels),
         x_type_2 = factor(x_type_2, levels = rev(x_levels)))
  
# Split into winners/nonwinners
tab_x_win <- tab_x %>% filter(winning_rank != "Other")
tab_x_nonwin <- tab_x %>% filter(winning_rank == "Other")

# Make plots
stat_xspear <- purrr::partial(stat_spear_no_p, label.x = n_labs*0.05, 
                              label.y = n_labs*0.95,
                              vjust = 1, hjust = 0, data = tab_x)

g_x_cor <- ggplot(aes(x=N1, y=N2), data = NULL) +
  geom_abline(size = 0.3, colour = palette_primary["grey"]) +
  geom_point_nonwinners(data=tab_x_nonwin, alpha=0.7, mapping=aes_col) +
  geom_point_winners(data=tab_x_win, alpha=1, mapping=aes_col) +
  stat_xspear() +
  facet_grid(x_type_1 ~ x_type_2, switch = "both") +
  scale_x_nlabs() + scale_y_nlabs() + scale_colour_winners() +
  theme_cor

#------------------------------------------------------------------------------
# Part 3: X-vs-accuracy correlations (all teams)
#------------------------------------------------------------------------------

# Combinations to compare
tab_mix_base <- expand_grid(ranking = data_teams$prediction_track_ranking,
                            accuracy_type = tab_types$accuracy_type_base,
                            x_type = x_types_rev) %>%
  mutate(winning_rank = ifelse(ranking <= n_winners, ranking, "Other"))

# Add nice accuracy names
tab_mix_nice <- tab_mix_base %>%
  inner_join(tab_types,
             by = c("accuracy_type" = "accuracy_type_base")) %>% 
  rename(accuracy_nice = accuracy_type_nice_asc) %>%
  select(-accuracy_type_nice_desc)

# Add values
tab_mix <- tab_mix_nice %>%
  inner_join(tab_teams_melt, by = c("ranking", "accuracy_type")) %>%
  inner_join(tab_teams_melt_x, by = c("ranking", "x_type")) %>%
  mutate(x_type = factor(x_type, levels = x_levels))

# Split into winners/nonwinners
tab_mix_win <- tab_mix %>% filter(winning_rank != "Other")
tab_mix_nonwin <- tab_mix %>% filter(winning_rank == "Other")

# Make plots
stat_mspear <- purrr::partial(stat_spear_no_p, label.x = 0.05, 
                              label.y = n_labs*0.95,
                              vjust = 1, hjust = 0, data = tab_mix)
g_mix_cor_all <- ggplot(aes(x=accuracy, y=N), data = NULL) +
  geom_point_nonwinners(data=tab_mix_nonwin, alpha=0.7, mapping=aes_col) +
  geom_point_winners(data=tab_mix_win, alpha=1, mapping=aes_col) +
  stat_mspear() +
  facet_grid(x_type ~ accuracy_nice, switch = "both") +
  scale_x_acc() + scale_y_nlabs() + scale_colour_winners() +
  theme_cor

#------------------------------------------------------------------------------
# Part 4: X-vs-accuracy correlations (filtered)
#------------------------------------------------------------------------------

# Filter data to remove outliers
tab_mixf <- tab_mix %>% filter(N < n_labs)
tab_mixf_win <- tab_mixf %>% filter(winning_rank != "Other")
tab_mixf_nonwin <- tab_mixf %>% filter(winning_rank == "Other")

# Make plots
stat_fspear <- purrr::partial(stat_spear_no_p, label.x = 0.05, 
                              label.y = n_labs*0.95,
                              vjust = 1, hjust = 0, data = tab_mixf)
g_mix_cor_filtered <- ggplot(aes(x=accuracy, y=N), data = NULL) +
  geom_point_nonwinners(data=tab_mixf_nonwin, alpha=0.7, mapping=aes_col) +
  geom_point_winners(data=tab_mixf_win, alpha=1, mapping=aes_col) +
  stat_fspear() +
  facet_grid(x_type ~ accuracy_nice, switch = "both") +
  scale_x_acc() + scale_y_nlabs() + scale_colour_winners() +
  theme_cor

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_fig(out_path_acc, g_acc_cor, out_width, out_asp)
save_fig(out_path_x, g_x_cor, out_width, out_asp)
save_fig(out_path_mix_all, g_mix_cor_all, out_width, out_asp)
save_fig(out_path_mix_filtered, g_mix_cor_filtered, out_width, out_asp)