#==============================================================================
# SUPPLEMENTARY FIGURE: X99 VS X95
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Input paths (team data)
path_teams <- "data/team-stats.csv"

# Output paths
out_path_xrel <- "figures/si_x99-vs-x95.png"

# Output parameters
output_width_xrel <- fig_width_single_narrow_in
output_asp_xrel   <- 0.9

#------------------------------------------------------------------------------
# Define palettes and scales
#------------------------------------------------------------------------------

# Winners vs nonwinners
palette_win <- unname(palette_primary_dark[c("grey", "green2")])
palette_win_dec <- c(palette_win[2], rep("black", 10))
scale_fill_win <- purrr::partial(scale_fill_manual,
                               name = "Metric:",
                               values = palette_win)

#------------------------------------------------------------------------------
# Read data
#------------------------------------------------------------------------------

data_teams <- suppressMessages(read_csv(path_teams))

#------------------------------------------------------------------------------
# Add decile labels
#------------------------------------------------------------------------------

tab_teams_labelled <- data_teams %>% group_by(ranking_decile) %>%
  mutate(decile_rank_min = min(prediction_track_ranking),
         decile_rank_max = max(prediction_track_ranking),
         decile_label = paste(decile_rank_min, 
                              decile_rank_max, sep=" to "),
         decile_label_long = paste("Rank", decile_label),
         decile_label = fct_inorder(decile_label),
         decile_label_long = fct_inorder(decile_label_long))

#------------------------------------------------------------------------------
# X99 vs X95
#------------------------------------------------------------------------------

# Compute ratio
tab_teams_xrel <- mutate(tab_teams_labelled,
                         xrel = X99/X95)

# Add separate winners column
tab_teams_xrel_win <- tab_teams_xrel %>% 
  filter(prediction_track_ranking <= n_winners) %>%
  mutate(ranking_decile = 0, 
         decile_label = paste(1, n_winners, sep = " to "),
         decile_label_long = paste("Rank", decile_label)) %>%
  bind_rows(tab_teams_xrel) %>%
  mutate(decile_label = fct_inorder(decile_label),
         decile_label_long = fct_inorder(decile_label_long),
         winner = (ranking_decile == 0))

# Summarise by decile
tab_teams_xrel_summ <- tab_teams_xrel_win %>%
  group_by(ranking_decile) %>%
  summarise(geomean_xrel = geomean(xrel),
            ranking_decile = ranking_decile[1],
            decile_label = decile_label[1],
            decile_label_long = decile_label_long[1],
            winner = winner[1])

g_xrel_deciles <- ggplot(tab_teams_xrel_summ,
                         aes(x=decile_label, y = geomean_xrel,
                             fill = winner)) + 
  scale_x_discrete(name = "Innovation Track ranking") +
  scale_y_cont_nominor(name = "X99 ÷ X95 (geometric mean)", limits = c(0,10),
                       breaks = seq(0,100,2), expand = c(0,0)) +
  scale_fill_win(guide = FALSE) +
  geom_col() + theme_col + theme(
    axis.text.x.bottom = element_text(colour = palette_win_dec),
  )

#-----------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_fig <- purrr::partial(save_plot, ncol = 1, nrow = 1)

# xrel plot
save_fig(out_path_xrel, g_xrel_deciles, 
         base_width = output_width_xrel,
         base_height = output_width_xrel * output_asp_xrel)
