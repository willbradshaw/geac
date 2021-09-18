#==============================================================================
# MAIN TEXT FIGURE 2: CORE COMPETITION METRICS
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Input path
path_countries <- "data/country-percentages.csv"
path_teams     <- "data/team-stats.csv"
path_accuracy  <- "data/team-top-n-accuracy.csv"

# Output paths
out_path_main <- "figures/main_fig2.png" # Main figure
out_path_si_nsub <- "figures/si_n-submissions.png" # SI figure on submission #s
out_path_si_acc_all <- "figures/si_accuracy-all-submissions.png"
out_path_si_f1_all <- "figures/si_f1-all-submissions.png"
out_path_si_pr_decile <- "figures/si_precision-recall-deciles.png"

# Output parameters
output_width_in <- page_width_in # Width of page
output_width_small <- fig_width_single_wide_in # Width for wide single-panel SI figures
output_asp_main <- 0.7 # Main-text figure
output_asp_si_nsub <- 0.55 # Two-panel n_submissions figure
output_asp_si_acc <- 0.7 # One-panel accuracy figure
output_asp_si_f1 <- 0.7 # One-panel F1 figure
output_asp_si_pr_decile <- 0.6

#------------------------------------------------------------------------------
# Define palettes, scales, themes
#------------------------------------------------------------------------------

# Palettes
palette_countries_part <- c(rbind(palette_primary_dark[c("blue", "orange", "green2")],
                                  palette_primary_pale[c("yellow", "pink", "blue")]),
                            palette_primary_dark["beige"],palette_primary_mid["grey"])

#------------------------------------------------------------------------------
# Import data
#------------------------------------------------------------------------------

# Country data (for pie chart)
data_countries <- suppressMessages(read_csv(path_countries))

# Team data
data_teams <- suppressMessages(read_csv(path_teams))

# Accuracy data
data_accuracy <- as.matrix(read.csv(path_accuracy, header = FALSE))

#------------------------------------------------------------------------------
# Make pie chart
#------------------------------------------------------------------------------

# Designate country display groups
europe_countries <- c("UK", "Netherlands", "Germany", "France", "Spain",
                      "Other Europe")
asia_countries   <- c("China", "Other Asia")
other_countries <- c("Other", "Other North America", "Other South America",
                     "Africa", "Oceania")
groups_out <- c("USA", "India", "Russia", "Other\nEurope", "Other\nAsia",
                "Canada", "Brazil", "Other")
data_countries_collapsed <- data_countries %>%
  mutate(in_europe = country %in% europe_countries,
         in_asia   = country %in% asia_countries,
         in_other  = country %in% other_countries,
         group = ifelse(in_europe, "Other\nEurope",
                        ifelse(in_asia, "Other\nAsia",
                               ifelse(in_other, "Other", country)))) %>%
  group_by(group) %>%
  summarise(percent_participants = sum(percent_participants),
            percent_site_visitors = sum(percent_site_visitors)) %>%
  rename(country = group) %>%
  arrange(match(country, groups_out)) %>%
  mutate(country = fct_inorder(country))

# Registered participant data
data_countries_part <- data_countries_collapsed %>%
  rename(percent = percent_participants) %>%
  mutate(label = country,
         csum = rev(cumsum(rev(percent))),
         pos = percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent/2, pos)) %>%
  select(-percent_site_visitors, -csum)

# Text alignment
hjust_countries_part <- c(0, 0, 0, 1.1, 1, 1, 0.7, 0.2)
vjust_countries_part <- c(0, 0.5, 1, 0.7, 0.8, 0.6, 0, 0)

# Make pie charts
g_countries_part <- ggplot(data_countries_part,
                           aes(x=1, y=percent, fill=fct_inorder(label))) +
  geom_col(width = 1) +
  coord_polar(theta = "y", direction = -1, clip = "off") +
  geom_text(aes(y = pos, label = label), x=1.8,
            size = rescale_font(fontsize_base),
            hjust = hjust_countries_part,
            vjust = vjust_countries_part, lineheight = 0.9) +
  geom_segment(aes(y=pos, yend=pos), x=1.5, xend=1.7,
               size = 0.2) +
  scale_y_continuous(name = "Participant countries",
                     breaks = data_countries_part$pos,
                     labels = data_countries_part$label) +
  scale_fill_manual(values = palette_countries_part) +
  guides(fill = FALSE) +
  theme_blank + theme(
    axis.title = NULL,
    axis.title.x = element_text(margin = lmargin(1.5,0,0,0),
                                face = "bold", hjust = 0.5,
                                vjust = 0.5),
    axis.title.y = element_blank(),
    aspect.ratio = 1,
    plot.margin = lmargin(r=2.5)
  )

#------------------------------------------------------------------------------
# Rank vs number of submissions (main plots)
#------------------------------------------------------------------------------

# Prepare dataset
data_prf <- data_teams %>% select(-starts_with("accuracy")) %>%
  mutate(ranking_pc = prediction_track_ranking/max(prediction_track_ranking),
         ranking_decile = ceiling(ranking_pc*10)) %>%
  mutate(winning_rank = ifelse(prediction_track_ranking <= n_winners,
                               prediction_track_ranking, "Other"),
         winning_rank = factor(winning_rank, levels = c(1:n_winners, "Other")),
         )
data_prf_winners    <- data_prf %>% filter(winning_rank != "Other")
data_prf_nonwinners <- data_prf %>% filter(winning_rank == "Other")

# Make complete plot
g_nsub_all <- ggplot(aes(x=prediction_track_ranking, y=n_entries,
                         colour=winning_rank), data = NULL) +
  geom_line_thin(colour = last(palette_winners), data = data_prf) +
  geom_point_nonwinners(data = data_prf_nonwinners) +
  geom_point_winners(data = data_prf_winners) +
  scale_x_ranking_all_trunc() +
  scale_y_cont_nominor(name = "# Submissions", limits = c(0, 150),
                       expand = c(0,0)) +
  scale_colour_winners() +
  theme_base

# Top 100 only
g_nsub_100 <- g_nsub_all + scale_x_ranking_100()

#------------------------------------------------------------------------------
# Rank vs number of submissions (decile plot)
#------------------------------------------------------------------------------

# Derive decile labels
data_prf_deciles_labelled <- data_prf %>%
  group_by(ranking_decile) %>%
  arrange(prediction_track_ranking) %>%
  mutate(decile_rank_min = min(prediction_track_ranking),
         decile_rank_max = max(prediction_track_ranking),
         decile_label = paste(decile_rank_min, 
                              decile_rank_max, sep=" to "),
         decile_label_long = paste("Rank", decile_label),
         decile_label = fct_inorder(decile_label),
         decile_label_long = fct_inorder(decile_label_long))

# Summarise by decile
data_prf_deciles_summ <- data_prf_deciles_labelled %>%
  summarise(avg_n_entries = mean(n_entries),
            decile_label = decile_label[1])

# Make decile plot
g_nsub_deciles <- ggplot(data_prf_deciles_summ, 
                         aes(x=decile_label, y=avg_n_entries)) +
  geom_col_mid(fill = last(palette_winners)) +
  scale_x_discrete(name = "Prediction Track ranking") +
  scale_y_cont_nominor(name = "Mean submissions per team",
                       expand = c(0,0), limits = c(0,50)) +
  theme_col

#------------------------------------------------------------------------------
# Accuracy vs rank
#------------------------------------------------------------------------------

# Prepare data for plotting
tab_rank_accuracy <- data_teams %>%
  select(prediction_track_ranking, starts_with("accuracy_")) %>%
  gather(type, accuracy, -prediction_track_ranking) %>%
  mutate(N = as.integer(sub("accuracy_top", "", type)),
         type = paste("Top", N),
         type = factor(type, levels = paste("Top", sort(unique(N)))),
         error = 1 - accuracy) %>%
  arrange(type == "Top 1", type == "Top 10", type == "Top 5")

# Make plot (all teams)
g_accuracy_rank_all <-  ggplot(tab_rank_accuracy, 
                               aes(x=prediction_track_ranking, y=accuracy,
                                   colour=type, shape=type)) +
  geom_line_thin() + geom_point(size = 1) +
  scale_x_ranking_all() + scale_y_percent(name = "Accuracy (%)") +
  scale_colour_accuracy() + scale_shape_multi(name = "Accuracy metric:") +
  theme_base + theme(aspect.ratio = 1/2)

# Top 100 only
g_accuracy_rank_100 <- g_accuracy_rank_all + scale_x_ranking_100() +
  theme(aspect.ratio = plot_aspect_ratio_base)

#------------------------------------------------------------------------------
# Macro precision, recall, F1 (all submissions)
#------------------------------------------------------------------------------

# Split labelled dataset by winner status (need for facet plot later)
data_prf_dl_winners    <- data_prf_deciles_labelled %>%
  filter(winning_rank != "Other")
data_prf_dl_nonwinners <- data_prf_deciles_labelled %>%
  filter(winning_rank == "Other")

# Make precision/recall plot
g_precision_recall <- ggplot(aes(x=macro_precision, y=macro_recall,
                                 colour=winning_rank), data = NULL) +
  geom_abline(size = 0.3, colour = palette_primary_mid["grey"],
              linetype = "dashed") +
  geom_point_nonwinners(data = data_prf_dl_nonwinners, alpha = 0.7) +
  geom_point_winners(data = data_prf_dl_winners, alpha = 1) +
  scale_x_proportion(name = "Precision") +
  scale_y_proportion(name = "Recall") +
  scale_colour_winners() +
  guides(alpha = FALSE) +
  theme_base

# Make macro-F1 plots
g_f1_all <- ggplot(aes(x=prediction_track_ranking, y=macro_f1,
                       colour=winning_rank), data = NULL) +
  geom_line_thin(data = data_prf, colour = last(palette_winners)) +
  geom_point_nonwinners(data = data_prf_nonwinners) +
  geom_point_winners(data = data_prf_winners) +
  scale_x_ranking_all() + scale_y_proportion(name = "F1 score") +
  scale_colour_winners() +
  theme_base + theme(aspect.ratio = 1/2)

g_f1_100 <- g_f1_all + scale_x_ranking_100() +
  theme(aspect.ratio = plot_aspect_ratio_base)

#------------------------------------------------------------------------------
# Precision/recall by decile
#------------------------------------------------------------------------------

# Make plot
g_precision_recall_deciles <- g_precision_recall +
  facet_wrap(~decile_label_long, ncol = 5) +
  theme_col + theme(aspect.ratio = 1,
                    strip.text = element_text(face = "plain"))

#------------------------------------------------------------------------------
# Winning team accuracy curves
#------------------------------------------------------------------------------

# Sort accuracy scores by top-10 accuracy, then filter for winners
acc_order <- order(data_accuracy[,10], decreasing = TRUE)
n_winners <- 4
n_labs <- ncol(data_accuracy)
data_accuracy_ordered <- data_accuracy[acc_order,]
data_accuracy_winners <- data_accuracy_ordered[1:n_winners,]
dimnames(data_accuracy_winners) <- list(rank = 1:n_winners, N = 1:n_labs)

# Convert into tibble
tab_accuracy_winners <- data_accuracy_winners %>% 
  as.table(responseName = "accuracy", stringsAsFactors = FALSE) %>% 
  as.data.frame(stringsAsFactors = FALSE, responseName = "accuracy") %>% 
  as_tibble %>% mutate(N = as.numeric(N))

# Plot accuracy
g_accuracy_winners <- ggplot(tab_accuracy_winners, 
                             aes(x=N, y=accuracy, colour = rank)) +
  geom_line_thin() + geom_point_winners() +
  scale_x_log10(name = "N") +
  scale_y_cont_nominor(name = "Top-N accuracy (%)",
                     labels = function(x) as.integer(x*100),
                     breaks = seq(0,1,0.1), limits = c(0.6, 1),
                     expand = c(0,0.01)) +
  scale_colour_winners() +
  theme_base

#------------------------------------------------------------------------------
# Collect all subfigures for main figure
#------------------------------------------------------------------------------

# Collate subfigures
g_acc_win_out <- g_accuracy_winners + guides(colour = FALSE)
g_pre_rec_out <- g_precision_recall + guides(colour = FALSE, alpha = FALSE)

# Assemble grid
g_figure <- (g_countries_part + g_nsub_100 + g_accuracy_rank_100) / 
  (g_acc_win_out + g_pre_rec_out + g_f1_100) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect", #ncol = 3, nrow = 2, 
              widths = c(1,1,1), heights = c(1,1)
              ) & 
  theme_legend & theme(
    legend.box = "vertical"
    )

#------------------------------------------------------------------------------
# Collate SI figures
#------------------------------------------------------------------------------

g_si_nsub <- wrap_plots(g_nsub_all, g_nsub_deciles, ncol = 2, widths = c(1,1),
                        guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme_legend

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_base <- purrr::partial(save_plot, ncol = 1, nrow = 1)
save_fig <- purrr::partial(save_base, base_width = output_width_in)

# Main figure
save_fig(out_path_main, g_figure, 
         base_height = output_width_in * output_asp_main)

# Submissions SI figure
save_fig(out_path_si_nsub, g_si_nsub,
          base_height = output_width_in * output_asp_si_nsub)

# All-submissions accuracy plot
save_base(out_path_si_acc_all, g_accuracy_rank_all,
          base_width = output_width_small,
          base_height = output_width_small * output_asp_si_acc)

# All-submissions F1 plot
save_base(out_path_si_f1_all, g_f1_all,
         base_width = output_width_small,
         base_height = output_width_small * output_asp_si_f1)

# Precision/recall by decile
save_fig(out_path_si_pr_decile, g_precision_recall_deciles,
         base_height = output_width_in * output_asp_si_pr_decile)
