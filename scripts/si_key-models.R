#==============================================================================
# KEY MODEL SUPPLEMENTARY FIGURES
#==============================================================================

#------------------------------------------------------------------------------
# Preamble
#------------------------------------------------------------------------------

# Libraries & formatting
source("scripts/aux_format-plots.R")

# Parameters
source_names_raw <- c("BLAST (new)", "BLAST (Alley et al., original)",
                  "BLAST (Alley et al., corrected)", "Nielsen & Voigt (2018)", "Alley et al. (2020)",
                  "GEAC (1st place)", "GEAC (2nd place)", "GEAC (3rd place)",
                  "GEAC (4th place)", "GEAC Ensemble")
source_names_plot <- c(NA, "BLAST (original)",
                      "BLAST (modified)", "Nielsen & Voigt (2018)",
                      "Alley et al. (2020)",
                      "GEAC (1st place)", "GEAC (2nd place)", "GEAC (3rd place)",
                      "GEAC (4th place)", "GEAC Ensemble")
levels_unkn <- c("All labs", "Known labs only", "Unknown Engineered only")
unk_eng_true_frequency <- 0.07541186

# Input paths
path_key <- "data/key-model-stats.csv"
path_unkn <- "data/key-model-unknown-engineered-stats.csv"

# Output parameters
out_path_acc     <- "figures/si_key-model-accuracy.png"
out_path_x       <- "figures/si_key-model-xmetrics.png"
out_path_cal     <- "figures/si_key-model-calibration.png"
out_path_unkn    <- "figures/si_key-model-unknown-engineered.png"
out_path_f1      <- "figures/si_key-model-f1.png"
out_width_full   <- page_width_in
out_width_single <- fig_width_single_wide_in
out_asp_acc      <- 1.5
out_asp_x        <- 1.5
out_asp_cal      <- 0.82
out_asp_unkn     <- 0.9
out_asp_f1       <- 0.82

# Themes, palettes, geoms
palette_calibration <- unname(c(palette_primary["grey"],
                                palette_primary_dark["grey"]))
palette_f1 <- unname(c(palette_primary_mid["grey"],
                       palette_primary_dark["grey"],
                       "black"))
palette_nlabs <- unname(palette_primary_darkmid["grey"])
scale_fill_cal <- purrr::partial(scale_fill_manual, name = "Metric:",
                                 values = palette_calibration)
scale_fill_f1 <- purrr::partial(scale_fill_manual, name = "Metric:",
                                values = palette_f1)
geom_hline_nlabs <- purrr::partial(geom_hline, yintercept = n_labs,
                                   colour = palette_nlabs, size = 0.3,
                                   linetype = "dashed")
annot_nlabs <- purrr::partial(annotate, geom = "text", x = Inf,
                              label = "All labs", hjust = 1, vjust = 1,
                              size = rescale_font(fontsize_base),
                              colour = palette_nlabs)
annot_nlabs_lin <- purrr::partial(annot_nlabs, y = n_labs-40)
annot_nlabs_log <- purrr::partial(annot_nlabs, y = n_labs*0.87)

#------------------------------------------------------------------------------
# Import data
#------------------------------------------------------------------------------

data_key <- import_csv(path_key)
data_unkn <- import_csv(path_unkn)

#------------------------------------------------------------------------------
# Process data
#------------------------------------------------------------------------------

# Modify sources and source names for display
tab_sources <- tibble(source = source_names_raw,
                      source_new = source_names_plot)
rename_sources <- function(tab){
  inner_join(tab, tab_sources, by = "source") %>%
    filter(!is.na(source_new)) %>% select(-source) %>%
    select(source = source_new, everything()) %>%
    mutate(source = factor(source, levels = source_names_plot[-1])) %>%
    arrange(source)
}

data_key_named <- rename_sources(data_key)
data_unkn_named <- rename_sources(data_unkn) %>%
  mutate(data_subset = factor(data_subset, levels = levels_unkn))

#------------------------------------------------------------------------------
# Accuracy
#------------------------------------------------------------------------------

scale_x_key <- purrr::partial(scale_x_discrete, name = "Model")
theme_col_bare_wide <- theme_col + 
  theme(aspect.ratio = 1/2,
        axis.title.x.bottom = element_blank())

tab_accuracy <- data_key_named %>% select(source, starts_with("accuracy")) %>%
  gather(metric, accuracy, -source) %>%
  mutate(metric = sub("accuracy_top", "Top ", metric),
         metric = fct_inorder(metric))
aes_col <- purrr::partial(aes, x=source, fill=metric)


g_accuracy <- ggplot(tab_accuracy, aes_col(y=accuracy)) +
  geom_col_mid(position = "dodge") +
  scale_x_key() + scale_y_percent(name = "Accuracy (%)") +
  scale_fill_accuracy() + theme_col_bare_wide

g_error <- ggplot(tab_accuracy, aes_col(y=1-accuracy)) +
  geom_col_mid(position = "dodge") +
  scale_x_key() + theme_col_bare_wide +
  scale_y_cont_nominor(name = "Misclassification rate (%)", expand = c(0,0),
                       labels = function(y) as.integer(y * 100),
                       limits = c(0,0.5), breaks = seq(0,1,0.1)) +
  scale_fill_accuracy() + theme_col_bare_wide

g_acc_out <- wrap_plots(g_accuracy, g_error, ncol = 1) +
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") &
  theme_legend

#------------------------------------------------------------------------------
# X-metrics
#------------------------------------------------------------------------------

tab_x <- data_key_named %>% select(source, starts_with("X")) %>%
  gather(metric, N, -source) %>% arrange(desc(metric)) %>%
  mutate(metric = fct_inorder(metric))

plot_x <- function(data){
  ggplot(data, aes_col(y=N)) + geom_col_mid(position = "dodge") +
    geom_hline_nlabs() + scale_x_key() + scale_fill_x() + theme_col_bare_wide
}

g_x_all <- plot_x(tab_x) + scale_y_log_nominor(expand=c(0,0)) +
  annot_nlabs_log()

g_x_large <- plot_x(filter(tab_x, metric %in% c("X99", "X95"))) +
  scale_y_cont_nominor(expand=c(0,0), breaks = seq(0,2000,400)) +
  annot_nlabs_lin()

g_x_out <- wrap_plots(g_x_all, g_x_large + guides(fill = FALSE), ncol = 1) +
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") &
  theme_legend

#------------------------------------------------------------------------------
# Calibration
#------------------------------------------------------------------------------

tab_cal <- data_key_named %>% select(source, contains("calibration")) %>%
  gather(metric, error, -source) %>%
  mutate(metric = toupper(sub(".._calibration_error", "ce", metric)),
         metric = fct_inorder(metric))
  
g_cal <- ggplot(tab_cal, aes_col(y=error)) +
  geom_col_mid(position = "dodge") + scale_x_key() +
  scale_y_cont_nominor(name = "Calibration error (%)", expand = c(0,0),
                       labels = function(y) as.integer(y * 100),
                       limits = c(0,0.4), breaks = seq(0,1,0.1)) +
  scale_fill_cal() + theme_col_bare_wide

g_cal_out <- wrap_plots(g_cal, ncol = 1) +
  plot_layout(guides = "collect") & theme_legend

#------------------------------------------------------------------------------
# Precision/recall/F1
#------------------------------------------------------------------------------

tab_f1 <- data_key_named %>% select(source, starts_with("macro")) %>%
  rename(Precision = macro_precision, Recall = macro_recall,
         `F1 score` = macro_f1) %>%
  gather(metric, value, -source) %>%
  mutate(metric = fct_inorder(metric))

g_f1 <- ggplot(tab_f1, aes_col(y=value)) +
  geom_col_mid(position = "dodge") + scale_x_key() +
  scale_y_proportion(name = NULL) + scale_fill_f1() +
  theme_col_bare_wide

g_f1_out <- wrap_plots(g_f1, ncol = 1) +
  plot_layout(guides = "collect") & theme_legend

#------------------------------------------------------------------------------
# Unknown Engineered
#------------------------------------------------------------------------------

aes_unkn <- purrr::partial(aes, x = source, fill = data_subset)

# Top-10 accuracy
g_unkn_acc_top10 <- ggplot(data_unkn_named, aes_unkn(y=accuracy_top10)) +
  geom_col_mid(position = "dodge") + scale_x_key() +
  scale_y_percent(name = "Top-10 accuracy (%)") + 
  scale_fill_unkn() + theme_col_bare_wide

# Top-1 accuracy
g_unkn_acc_top1 <- ggplot(data_unkn_named, aes_unkn(y=accuracy_top1)) +
  geom_col_mid(position = "dodge") + scale_x_key() +
  scale_y_percent(name = "Top-1 accuracy (%)") + 
  scale_fill_unkn() + theme_col_bare_wide

# Rate of Unknown Engineered in top-10
g_unkn_in_top10 <- ggplot(data_unkn_named, aes_unkn(y=unk_eng_in_top10)) +
  geom_col_mid(position = "dodge") + scale_x_key() +
  geom_hline(yintercept = unk_eng_true_frequency, colour = palette_secondary["red"],
             linetype = "dashed", size = 0.3) +
  scale_y_percent(name = "Mean frequency of\nUnk. Eng. in top 10 guesses") + 
  scale_fill_unkn() + theme_col_bare_wide

# Average rank of unknown engineered
g_unkn_rank <- ggplot(data_unkn_named, aes_unkn(y=unk_eng_avg_rank)) +
  geom_col_mid(position = "dodge") + scale_x_key() +
  scale_y_cont_nominor(name = "Geometric mean of\nUnk. Eng. ranking",
                       expand = c(0,0)) + 
  scale_fill_unkn() + theme_col_bare_wide

g_unkn_out <- wrap_plots(g_unkn_in_top10, g_unkn_rank, g_unkn_acc_top10,
                      g_unkn_acc_top1, ncol = 2) +
  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") &
  theme_legend

#------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------

save_fig(out_path_acc, g_acc_out, out_width_single, out_asp_acc)
save_fig(out_path_x, g_x_out, out_width_single, out_asp_x)
save_fig(out_path_cal, g_cal_out, out_width_single, out_asp_cal)
save_fig(out_path_unkn, g_unkn_out, out_width_full, out_asp_unkn)
save_fig(out_path_f1, g_f1_out, out_width_single, out_asp_f1)
