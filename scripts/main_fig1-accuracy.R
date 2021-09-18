#==============================================================================
# MAIN TEXT FIGURE 1B: HISTORICAL GEA ACCURACY
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Input paths
path_key <- "data/key-model-stats.csv"

# Output path
out_path  <- "figures/main_fig1-accuracy.svg"

# Define plotting theme
xtext = element_text(angle = 45, vjust = 1, hjust = 1,
                     margin = lmargin(t=0.3,b=-0.9),
                     size = fontsize_title)
ltext = element_text(margin = lmargin(t=0.1,b=0.1,l=0.1,r=0))
theme_col <- theme_internal_strong + theme(
  axis.title.x = element_blank(),
  axis.text.x.bottom = xtext,
  legend.text = ltext,
  legend.position = c(0.98, 0.98),
  legend.justification = c("right", "top"),
  legend.title = element_blank(),
  legend.margin = lmargin(rep(0.1,4)),
  legend.box.margin = lmargin(rep(0.1,4)),
  legend.key.height = lines(0.9),
  legend.key.width  = lines(0.9),
  aspect.ratio = NULL,
  plot.margin = lmargin(t=0.3,l=0.05,r=0.05)
)

# Set parameters
out_width <- 2.762
out_asp   <- 2.050/out_width
source_names_raw <- c("BLAST (new)", "BLAST (Alley et al., original)",
                      "BLAST (Alley et al., corrected)", "Nielsen & Voigt (2018)", "Alley et al. (2020)",
                      "GEAC (1st place)", "GEAC (2nd place)", "GEAC (3rd place)",
                      "GEAC (4th place)", "GEAC Ensemble")
source_names_plot <- c(NA, NA, "BLAST", "Nielsen & Voigt\n(2018)",
                       "Alley et al.\n(2020)", "GEAC\n(1st place)", NA, NA, NA,
                       "GEAC\nEnsemble")
source_levels <- source_names_plot[!is.na(source_names_plot)]
palette_fill <- unname(c(palette_primary_dark["grey"], palette_primary["grey"]))

#------------------------------------------------------------------------------
# Import and process data
#------------------------------------------------------------------------------

tab_sources <- tibble(source = source_names_raw,
                      source_new = source_names_plot)

data_key <- import_csv(path_key) %>%
  inner_join(tab_sources, by = "source") %>%
  filter(!is.na(source_new)) %>% select(-source) %>%
  select(source = source_new, everything()) %>%
  mutate(source = factor(source, levels = source_levels)) %>%
  arrange(source)

tab_error <- data_key %>% select(source, accuracy_top1, accuracy_top10) %>%
  gather(metric, accuracy, -source) %>%
  mutate(metric = sub("accuracy_top", "Top ", metric),
         metric = fct_inorder(metric),
         error = 1-accuracy)

#------------------------------------------------------------------------------
# Make headline plot
#------------------------------------------------------------------------------

g_acc <- ggplot(tab_error, aes(x=source, y=accuracy, fill = metric)) +
  geom_col_mid(position = "dodge") +
  scale_y_percent(name = "Accuracy (%)") +
  scale_fill_manual(values = palette_fill) +
  theme_col + theme(legend.position = c(0.95, 0.05),
                    legend.justification = c("right", "bottom"))

g_error <- ggplot(tab_error, aes(x=source, y=error, fill=metric)) +
  geom_col_mid(position = "dodge") +
  scale_y_cont_nominor(name = "Misclassification rate (%)",
                       breaks = seq(0,1,0.1), limits = c(0,0.58),
                       expand = c(0,0), labels = function(x) x*100) +
  scale_fill_manual(values = palette_fill) +
  theme_col

print(g_error)
  
#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_fig(out_path, g_error, out_width, out_asp)
