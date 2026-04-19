# =========================================================
# Script: 04_heatmap_figure.R
# Project: Coral bleaching RGB analysis
# Purpose:
#   - Select top-performing RGB indices from rank_four
#   - Build a heatmap of absolute Spearman correlations
#     across focal outcomes and species
#   - Highlight the relative performance of D_toWhite
#     and D_toWhite_RGB
# Input:
#   ../outputs/rank_four.csv
# Output:
#   heatmap.pdf
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
  "ggplot2"
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

input_file  <- "../outputs/rank_four.csv"
output_file <- "heatmap.pdf"

outcome_a <- "cells_cm2"
outcome_b <- "FvFm"
outcome_c <- "total_chl_cm2"
outcome_d <- "host_protein_cm2"

n_top <- 2


# =========================================================
# 2. Load ranking table
# =========================================================

rank_four <- read.csv(input_file)


# =========================================================
# 3. Select top-performing indices across outcomes
# =========================================================

outcome_order <- c(outcome_a, outcome_c, outcome_b, outcome_d)
focus_outcomes <- c(outcome_a, outcome_c)

topN_union <- rank_four %>%
  group_by(Species, Outcome) %>%
  slice_head(n = n_top) %>%
  ungroup()

indices_union <- unique(c(
  topN_union$Index,
  "D_toWhite",
  "D_toWhite_RGB"
))


# =========================================================
# 4. Order indices for heatmap display
# =========================================================

index_order <- rank_four %>%
  filter(
    Outcome %in% focus_outcomes,
    Index %in% indices_union
  ) %>%
  group_by(Index) %>%
  summarise(
    r_abs_mean_ac  = mean(abs(p_spearman), na.rm = TRUE),
    CVRMSE_mean_ac = mean(CV_RMSE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(r_abs_mean_ac),
    CVRMSE_mean_ac
  ) %>%
  pull(Index)

index_order_mod <- c(
  "D_toWhite",
  "D_toWhite_RGB",
  setdiff(index_order, c("D_toWhite", "D_toWhite_RGB"))
)


# =========================================================
# 5. Prepare heatmap data
# =========================================================

heat_df <- rank_four %>%
  filter(
    Index %in% indices_union,
    Outcome %in% outcome_order
  ) %>%
  group_by(Species, Outcome) %>%
  mutate(
    r_abs = abs(p_spearman),
    Rank_min = min(Rank, na.rm = TRUE),
    IsTop = Rank == Rank_min
  ) %>%
  ungroup() %>%
  mutate(
    Outcome = factor(Outcome, levels = outcome_order),
    Index   = factor(Index, levels = rev(index_order_mod))
  )


# =========================================================
# 6. Generate heatmap
# =========================================================

p_heatmap <- ggplot(heat_df, aes(x = Outcome, y = Index, fill = r_abs)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(
    aes(label = sprintf("%.2f", r_abs)),
    size = 10 / .pt
  ) +
  facet_grid(. ~ Species, scales = "free_x", space = "free_x") +
  scale_fill_gradient2(
    low = "#0a5b92",
    mid = "white",
    high = "#920a17",
    midpoint = 0.5,
    limits = c(0, 1),
    name = "|Spearman \u03c1|"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(color = "black"),
    axis.title = element_blank(),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      color = "black"
    ),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", color = "black"),
    plot.title = element_text(face = "bold", color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.position = "right"
  )

p_heatmap


# =========================================================
# 7. Save figure
# =========================================================

ggsave(
  filename = output_file,
  plot = p_heatmap,
  width = 180,
  height = 120,
  units = "mm",
  device = cairo_pdf
)