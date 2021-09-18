#==============================================================================
# SI FIGURES: SEQUENCES PER LAB
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries
source("scripts/aux_format-plots.R")

# Input paths
data_path <- "data/lab-counts.csv"

# Output paths
out_path_all  <- "figures/si_sequences-per-lab.png"
out_path_test <- "figures/si_test-lab-composition.png"

# Output parameters
output_width_in <- fig_width_single_wide_in
output_asp <- 0.7

# Palettes
palette_test <- unname(palette_secondary[c("red", "blue")])

#------------------------------------------------------------------------------
# Import and process data
#------------------------------------------------------------------------------

# Import data
data <- import_csv(data_path)

# Pre-collapse meta-counts
data_all <- data %>%
  mutate(n_all = n_leaderboard + n_test + n_train) %>%
  group_by(n_seq = n_all) %>% summarise(n_labs = n())

# Test-set counts (post-collapse)
id_unkn <- max(data$lab_id_hidden, na.rm = TRUE) + 1
data_test <- data %>% filter(n_test > 0) %>%
  mutate(lab_id_hidden = ifelse(unknown_engineered, id_unkn, 
                                lab_id_hidden)) %>%
  group_by(lab_id_hidden, unknown_engineered) %>% 
  summarise(n_seq = sum(n_test), .groups = "drop") %>%
  mutate(lab_rank = row_number(desc(n_seq)),
         p_seq = n_seq/sum(n_seq),
         unk_eng = ifelse(unknown_engineered,
                          "Unknown Engineered", "Other lab"),
         unk_eng = factor(unk_eng, 
                          levels = c("Unknown Engineered", "Other lab"))) %>%
  select(lab_rank, p_seq, unk_eng)

#------------------------------------------------------------------------------
# Plot pre-collapse meta-counts
#------------------------------------------------------------------------------

g_ccount <- ggplot(data_all, aes(x=n_seq, y=n_labs)) +
  geom_line(size = 0.5, colour = palette_primary_dark["grey"]) + 
  geom_point(shape = 16, size = 2, colour = palette_primary_dark["grey"]) +
  scale_x_log10(name="Plasmid entries per lab") +
  scale_y_continuous(name="Number of labs", minor_breaks = NULL) +
  theme_base + theme(aspect.ratio = 1/2)

#------------------------------------------------------------------------------
# Plot test-set composition
#------------------------------------------------------------------------------

g_test <- ggplot(data_test, aes(x=lab_rank, y=p_seq, colour=unk_eng)) +
  geom_line(size = 0.2, colour = palette_test[2]) +
  geom_point() +
  scale_x_log10("Lab ranking (by number of sequences in test set)") +
  scale_y_continuous(name = "Percentage of test set",
                     labels = function(x) as.integer(x*100),
                     limits = c(0,0.08)) +
  scale_colour_manual(name = "Lab",
                      values = palette_test) +
  theme_base + theme(aspect.ratio = 1/2)

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_fig(out_path_all, g_ccount, output_width_in, output_asp)
save_fig(out_path_test, g_test, output_width_in, output_asp)