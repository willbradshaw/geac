#==============================================================================
# SI FIGURE: CALIBRATION
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Input path
path_teams <- "data/team-stats.csv"

# Output parameters
out_path <- "figures/si_calibration.png"
out_width <- fig_width_single_wide_in
out_asp <- 1.25
out_path_decile <- "figures/si_calibration-deciles.png"
out_asp_decile <- 0.8

# Scales for decile plot
palette_win <- unname(palette_primary_dark[c("green2", "grey")])
palette_win_dec <- c(palette_win[1], rep("black", 10))
scale_fill_win <- purrr::partial(scale_fill_manual, values = palette_win,
                                 name = "Prizewinners?", labels = c("Yes", "No"))
scale_alpha_decile <- purrr::partial(scale_alpha_discrete, range = c(0.45,1),
                              name = "Metric:")

#------------------------------------------------------------------------------
# Import and prepare data
#------------------------------------------------------------------------------

data_teams <- import_csv(path_teams) %>% 
    rename(ranking=prediction_track_ranking) %>%
    mutate(winning_rank = ifelse(ranking <= n_winners, ranking, "Other"),
           winning_rank = fct_inorder(winning_rank)) %>%
    select(ranking, winning_rank, exp_calibration_error, max_calibration_error,
           ranking_decile)
data_win <- filter(data_teams, winning_rank != "Other")
data_nonwin <- filter(data_teams, winning_rank == "Other")

#------------------------------------------------------------------------------
# Per-team plots
#------------------------------------------------------------------------------

aes_cal <- purrr::partial(aes, x = ranking, colour = winning_rank)
stat_cspear <- purrr::partial(stat_spear, label.y = 0.98, vjust = 1, hjust = 0,
                              data = data_teams, label.x = 90)

g_mce <- ggplot(aes_cal(y=max_calibration_error), data = NULL) +
    geom_point_winners(data = data_win) +
    geom_point_nonwinners(data = data_nonwin) +
    stat_cspear() +
    scale_colour_winners() + scale_x_ranking_all() +
    scale_y_percent(name = "Maximum Calibration Error (%)") +
    theme_base + theme(aspect.ratio = 1/2)

g_ece <- ggplot(aes_cal(y=exp_calibration_error), data = NULL) +
    geom_point_winners(data = data_win) +
    geom_point_nonwinners(data = data_nonwin) +
    stat_cspear() +
    scale_colour_winners() + scale_x_ranking_all() +
    scale_y_percent(name = "Expected Calibration Error (%)") +
    theme_base + theme(aspect.ratio = 1/2)

g_out <- wrap_plots(g_ece, g_mce, ncol = 1) +
    plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") &
    theme_legend

#------------------------------------------------------------------------------
# Decile plot
#------------------------------------------------------------------------------

# Define data
data_deciles <- data_teams %>% group_by(ranking_decile) %>%
    summarise(rank_max = max(ranking), rank_min = min(ranking),
              ECE = mean(exp_calibration_error),
              MCE = mean(max_calibration_error)) %>%
    mutate(decile_label = paste(rank_min, rank_max, sep=" to "))
data_winners <- data_teams %>% filter(ranking <= n_winners) %>%
    summarise(ranking_decile = 0,
              decile_label = paste(1, n_winners, sep = " to "),
              rank_min = 1, rank_max = n_winners,
              ECE = mean(exp_calibration_error),
              MCE = mean(max_calibration_error))
data_deciles_plot <- bind_rows(data_winners, data_deciles) %>%
    mutate(decile_label = fct_inorder(decile_label),
           winner = ranking_decile == 0) %>%
    select(-rank_min, -rank_max) %>%
    gather(metric, value, -ranking_decile, -decile_label, -winner) %>%
    arrange(metric) %>% 
    mutate(`Metric:` = fct_inorder(metric),
           winner = factor(winner, levels = c(TRUE, FALSE)))

# Prepare plotting auxiliaries
aes_dec <- aes(x=decile_label, y=value, fill=winner, alpha=metric)

g_deciles <- ggplot(data_deciles_plot, aes_dec) +
    geom_col_mid(position = "dodge") +
    scale_x_discrete(name = "Innovation Track ranking") +
    scale_y_cont_nominor(name = "Mean prediction error (%)",
                         breaks = seq(0,1,0.2), expand = c(0,0),
                         labels = function(x) x*100,
                         limits = c(0,0.8)) +
    scale_fill_win() + scale_alpha_decile() +
    theme_col + theme(
        axis.text.x.bottom = element_text(colour = palette_win_dec),
        aspect.ratio = 1/2
    )
print(g_deciles)


#------------------------------------------------------------------------------
#save output
#------------------------------------------------------------------------------

save_fig(out_path, g_out, out_width, out_asp)
save_fig(out_path_decile, g_deciles, out_width, out_asp_decile)
