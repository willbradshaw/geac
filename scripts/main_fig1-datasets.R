#==============================================================================
# MAIN TEXT FIGURE 1C, PART 1: DATASET SIZES & SEQUENCE LENGTHS
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries
source("scripts/aux_format-plots.R")

# Data paths
size_path <- "data/dataset-size.csv"
len_path  <- "data/sequence-lengths-sample.csv"

# Output path
out_svg <- "figures/main_fig1-datasets.svg"

# Plotting parameters
asp_ratio  <- 0.6 # Aspect ratio for export (width/height)

# Scales
scale_fill_palette <- purrr::partial(scale_fill_manual,
                                     values = unname(palette_primary))

# Factor levels
dataset_levels <- c("train", "leaderboard", "test")

#------------------------------------------------------------------------------
# Import and process data
#------------------------------------------------------------------------------

# Dataset sizes
data_size <- import_csv(size_path) %>% 
  mutate(dataset = factor(dataset, levels = dataset_levels))

# Sampled sequence lengths from each dataset
data_len <- import_csv(len_path) %>% 
  mutate(dataset = factor(dataset, levels = dataset_levels))

#------------------------------------------------------------------------------
# Plot relative sizes (for metadata / labels columns)
#------------------------------------------------------------------------------

g_size <- ggplot(data_size, aes(x=1, y=size, fill = dataset)) +
  geom_col(position = "fill") + scale_fill_palette() +
  theme_void() + theme(legend.position = "blank")

#------------------------------------------------------------------------------
# Plot sequence lengths (for sequence data column)
#------------------------------------------------------------------------------

g_seq <- ggplot(data_len, 
                aes(x=sequence_order, y=sequence_length, fill=dataset)) +
  geom_col(width = 1) + 
  coord_flip() + 
  scale_y_reverse() + scale_x_reverse() +
  scale_fill_palette() +
  theme_void() + theme(legend.position = "blank")

#------------------------------------------------------------------------------
# Arrange & save
#------------------------------------------------------------------------------

# Arrange with patchwork
g_all <- g_seq + g_size + g_size + plot_layout(widths = c(12,1,1))

# Save it
save_plot(out_svg, g_all, base_asp = asp_ratio)