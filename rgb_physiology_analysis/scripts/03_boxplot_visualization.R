# =========================================================
# Script: 03_boxplot_visualization.R
# Project: Coral bleaching RGB analysis
# Purpose:
#   - Create treatment-wise boxplots for selected physiological
#     and color-based outcomes
#   - Run Kruskal-Wallis tests within species
#   - Run Dunn post-hoc tests within species
#   - Export publication-style boxplots and supplementary tables
# Input:
#   ../outputs/dat_final.csv
# Outputs:
#   ../outputs/boxplots/boxplot_<outcome>.pdf
#   ../outputs/boxplots/kw_results_supplementary.csv
#   ../outputs/boxplots/dunn_results_supplementary.csv
# =========================================================


# =========================================================
# 0. Setup
# =========================================================

rm(list = ls())
if (dev.cur() != 1) dev.off()
options(scipen = 999)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cran_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "rstatix",
  "ggh4x",
  "patchwork"
)

install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(cran_packages, install_and_load))


# =========================================================
# 1. User-defined inputs
# =========================================================

input_file <- "../outputs/dat_final.csv"
output_dir <- "../outputs/boxplots"

outcomes <- c(
  "FvFm",
  "FvFm_Middle",
  "cells_cm2",
  "total_chl_cm2",
  "chl_per_cell",
  "host_protein_cm2",
  "D_toWhite"
)

outcome_labels <- c(
  "FvFm"             = "Fv/Fm (Final)",
  "FvFm_Middle"      = "Fv/Fm (Middle)",
  "cells_cm2"        = "Symbiont density",
  "total_chl_cm2"    = "Total chlorophyll",
  "chl_per_cell"     = "Chlorophyll per cell",
  "host_protein_cm2" = "Host protein",
  "D_toWhite"        = "D_toWhite"
)

treatment_labels <- c(
  "C"  = "32°C",
  "T1" = "34°C",
  "T2" = "36°C",
  "T3" = "38°C"
)

species_colors <- c(
  "Acropora hemprichii"    = "#9b8d5c",
  "Stylophora pistillata"  = "#9b5c89"
)


# =========================================================
# 2. Helper functions
# =========================================================

median_cl_boot <- function(x, conf = 0.95, R = 999) {
  x <- x[!is.na(x)]
  
  boot_medians <- replicate(R, median(sample(x, replace = TRUE)))
  
  data.frame(
    y    = median(x),
    ymin = quantile(boot_medians, (1 - conf) / 2),
    ymax = quantile(boot_medians, 1 - (1 - conf) / 2)
  )
}

make_panel <- function(df, species_name, outcome_label, y_min, y_max, box_color) {
  ggplot(
    df %>% filter(Species == species_name),
    aes(x = Treatment, y = Value)
  ) +
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
             fill = "#ffff99", alpha = 0.15) +
    annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf,
             fill = "#ffcc33", alpha = 0.15) +
    annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
             fill = "#ee6600", alpha = 0.15) +
    annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -Inf, ymax = Inf,
             fill = "#ff0000", alpha = 0.15) +
    geom_boxplot(
      outlier.shape = NA,
      alpha = 0.85,
      width = 0.6,
      coef = 0,
      fill = box_color,
      color = "black",
      linewidth = 0.2,
      fatten = 2
    ) +
    geom_jitter(
      width = 0.1,
      size = 0.6,
      alpha = 0.55,
      shape = 16,
      color = "black"
    ) +
    stat_summary(
      fun.data = median_cl_boot,
      geom = "errorbar",
      width = 0.3,
      linewidth = 0.3,
      color = "black"
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 1.5,
      fill = "white",
      color = "black"
    ) +
    force_panelsizes(rows = unit(45, "mm"), cols = unit(50, "mm")) +
    scale_x_discrete(
      labels = treatment_labels,
      expand = expansion(add = 0)
    ) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    labs(
      title = NULL,
      x = NULL,
      y = outcome_label
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "none",
      strip.text = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_text(colour = "black", size = 10),
      axis.title = element_text(colour = "black", size = 10),
      axis.ticks.length.x = unit(0, "mm"),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
}


# =========================================================
# 3. Load and reshape data
# =========================================================

dat_final_csv <- read.csv(input_file)

dat_long <- dat_final_csv %>%
  dplyr::select(ID, Species, Treatment, all_of(outcomes)) %>%
  pivot_longer(
    cols = all_of(outcomes),
    names_to = "Outcome",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value)) %>%
  mutate(Outcome = factor(Outcome, levels = outcomes))


# =========================================================
# 4. Initialize supplementary result tables
# =========================================================

kw_all   <- list()
dunn_all <- list()


# =========================================================
# 5. Run tests and generate boxplots for each outcome
# =========================================================

for (outcome_y in outcomes) {
  
  df <- dat_long %>%
    filter(Outcome == outcome_y)
  
  # -------------------------------------------------------
  # 5.1 Kruskal-Wallis test within species
  # -------------------------------------------------------
  
  kw_tbl <- df %>%
    group_by(Species) %>%
    kruskal_test(Value ~ Treatment)
  
  # -------------------------------------------------------
  # 5.2 Dunn post-hoc test within species
  # -------------------------------------------------------
  
  dunn_tbl <- df %>%
    group_by(Species) %>%
    dunn_test(Value ~ Treatment, p.adjust.method = "BH") %>%
    ungroup()
  
  kw_all[[outcome_y]] <- kw_tbl %>%
    mutate(Outcome = outcome_y) %>%
    dplyr::select(Outcome, Species, n, statistic, df, p)
  
  dunn_all[[outcome_y]] <- dunn_tbl %>%
    mutate(Outcome = outcome_y) %>%
    dplyr::select(Outcome, Species, group1, group2, statistic, p, p.adj, p.adj.signif)
  
  # -------------------------------------------------------
  # 5.3 Plot settings
  # -------------------------------------------------------
  
  y_min <- min(df$Value, na.rm = TRUE)
  y_max <- max(df$Value, na.rm = TRUE)
  
  p1 <- make_panel(
    df = df,
    species_name = "Acropora hemprichii",
    outcome_label = outcome_labels[outcome_y],
    y_min = y_min,
    y_max = y_max,
    box_color = species_colors["Acropora hemprichii"]
  )
  
  p2 <- make_panel(
    df = df,
    species_name = "Stylophora pistillata",
    outcome_label = outcome_labels[outcome_y],
    y_min = y_min,
    y_max = y_max,
    box_color = species_colors["Stylophora pistillata"]
  ) +
    theme(axis.title.y = element_blank())
  
  # -------------------------------------------------------
  # 5.4 Save figure
  # -------------------------------------------------------
  
  ggsave(
    filename = paste0(output_dir, "/boxplot_", outcome_y, ".pdf"),
    plot = p1 + p2,
    device = cairo_pdf,
    width = 140,
    height = 55,
    units = "mm",
    bg = "transparent"
  )
}


# =========================================================
# 6. Combine and format supplementary result tables
# =========================================================

kw_all <- bind_rows(kw_all) %>%
  mutate(Outcome = outcome_labels[Outcome])

dunn_all <- bind_rows(dunn_all) %>%
  mutate(
    Outcome   = outcome_labels[Outcome],
    statistic = round(statistic, 3),
    p         = round(p, 4),
    p.adj     = round(p.adj, 4)
  )


# =========================================================
# 7. Optional exports
# =========================================================

# write.csv(
#   kw_all,
#   file.path(output_dir, "kw_results_supplementary.csv"),
#   row.names = FALSE
# )

# write.csv(
#   dunn_all,
#   file.path(output_dir, "dunn_results_supplementary.csv"),
#   row.names = FALSE
# )