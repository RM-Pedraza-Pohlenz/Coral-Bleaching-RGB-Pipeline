# =========================================================
# Script: 08_composite_ranking.R
# Project: Coral bleaching RGB analysis
# Purpose:
#   - Rank RGB-derived indices by association with
#     bleaching severity metrics (Table S3: symbiont density
#     and total chlorophyll content) and photophysiology
#     (Table S4: Fv/Fm) across both species
# Input:
#   ../outputs/rank_four.csv
# Outputs:
#   table_S3_fvfm_indices_ranked.csv
#   table_S4_bleaching_indices_ranked.csv
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
  "tidyr"
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

input_file <- "../outputs/rank_four.csv"


# =========================================================
# 2. Load input data
# =========================================================

rank_four <- read.csv(input_file)


# =========================================================
# Table S3: Bleaching index ranking (cells_cm2 + total_chl_cm2)
# =========================================================

bleaching_outcomes <- c("cells_cm2", "total_chl_cm2")

# ── Mean absolute rho across both outcomes AND both species (for ordering) ────
bleaching_summary <- rank_four %>%
  dplyr::filter(Outcome %in% bleaching_outcomes) %>%
  dplyr::group_by(Index) %>%
  dplyr::summarise(
    rho_abs_mean_ac = round(mean(abs(p_spearman), na.rm = TRUE), 3),
    .groups = "drop"
  )

# ── Wide table ────────────────────────────────────────────────────────────────
bleaching_wide <- rank_four %>%
  dplyr::filter(Outcome %in% bleaching_outcomes) %>%
  tidyr::pivot_wider(
    id_cols     = c(Index, Species),
    names_from  = Outcome,
    values_from = c(p_spearman, p_adj_BH)
  ) %>%
  dplyr::left_join(bleaching_summary, by = "Index") %>%
  dplyr::arrange(desc(rho_abs_mean_ac), Index, Species) %>%
  dplyr::select(
    Index, Species, rho_abs_mean_ac,
    rho_spearman_cells_cm2     = p_spearman_cells_cm2,
    rho_spearman_total_chl_cm2 = p_spearman_total_chl_cm2,
    p_adj_BH_cells_cm2,
    p_adj_BH_total_chl_cm2
  ) %>%
  dplyr::mutate(
    across(c(rho_abs_mean_ac,
             rho_spearman_cells_cm2,
             rho_spearman_total_chl_cm2),
           ~ round(.x, 3))
  )

print(bleaching_wide)

#write.csv(bleaching_wide, "table_S3_bleaching_indices_ranked.csv", row.names = FALSE)

# =========================================================
# Table S4: Fv/Fm index ranking
# =========================================================
fvfm_outcomes <- c("FvFm")

fvfm_summary <- rank_four %>%
  dplyr::filter(Outcome %in% fvfm_outcomes) %>%
  dplyr::group_by(Index) %>%
  dplyr::summarise(
    rho_abs_mean_ac = round(mean(abs(p_spearman), na.rm = TRUE), 3),
    .groups = "drop"
  )

fvfm_wide <- rank_four %>%
  dplyr::filter(Outcome %in% fvfm_outcomes) %>%
  tidyr::pivot_wider(
    id_cols     = c(Index, Species),
    names_from  = Outcome,
    values_from = c(p_spearman, p_adj_BH)
  ) %>%
  dplyr::left_join(fvfm_summary, by = "Index") %>%
  dplyr::arrange(desc(rho_abs_mean_ac), Index, Species) %>%
  dplyr::select(
    Index, Species, rho_abs_mean_ac,
    rho_spearman_FvFm = p_spearman_FvFm,
    p_adj_BH_FvFm
  ) %>%
  dplyr::mutate(
    across(c(rho_abs_mean_ac, rho_spearman_FvFm),
           ~ round(.x, 3))
  )

print(fvfm_wide)
# write.csv(fvfm_wide, "table_S4_fvfm_indices_ranked.csv", row.names = FALSE)