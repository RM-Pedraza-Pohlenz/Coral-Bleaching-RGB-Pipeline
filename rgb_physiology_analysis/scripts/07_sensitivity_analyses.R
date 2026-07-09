# =========================================================
# Script: 07_sensitivity_analyses.R
# Project: Coral bleaching RGB analysis
# Purpose:
#   - Run Colony x Treatment correlations
# Input:
#   ../outputs/dat_final.csv
# Outputs:
#   Correlations by colony x treatment
# =========================================================

#////////////////////////////////////////////////////////////////#
####           Sensitivity & Robustness Analyses              ####
#////////////////////////////////////////////////////////////////#

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

input_file <- "../outputs/dat_final.csv"


# =========================================================
# 2. Load data
# =========================================================

dat_final <- read.csv(input_file)


# =========================================================
# 3. Aggregate to genotype x treatment level
# =========================================================
dat_genotype_treatment <- dat_final %>%
  group_by(Species, Genotype, Treatment) %>%
  dplyr::summarise(
    D_toWhite     = mean(D_toWhite,     na.rm = TRUE),
    DeltaE76      = mean(DeltaE76,      na.rm = TRUE),
    FvFm          = mean(FvFm,          na.rm = TRUE),
    cells_cm2     = mean(cells_cm2,     na.rm = TRUE),
    total_chl_cm2 = mean(total_chl_cm2, na.rm = TRUE),
    .groups = "drop"
  )

cat("Fragment-level n per species:\n")
print(dat_final %>% group_by(Species) %>% dplyr::summarise(n = dplyr::n()))

cat("\nGenotype x treatment-level n per species:\n")
print(dat_genotype_treatment %>% group_by(Species) %>% dplyr::summarise(n = dplyr::n()))

# =========================================================
# 4. Fragment-level correlations
# =========================================================
rho_fragment <- dat_final %>%
  group_by(Species) %>%
  dplyr::summarise(
    D_toWhite_cells  = cor(D_toWhite, cells_cm2,     method = "spearman", use = "complete.obs"),
    D_toWhite_chl    = cor(D_toWhite, total_chl_cm2, method = "spearman", use = "complete.obs"),
    DeltaE76_cells   = cor(DeltaE76,  cells_cm2,     method = "spearman", use = "complete.obs"),
    DeltaE76_chl     = cor(DeltaE76,  total_chl_cm2, method = "spearman", use = "complete.obs"),
    FvFm_cells       = cor(FvFm,      cells_cm2,     method = "spearman", use = "complete.obs"),
    FvFm_chl         = cor(FvFm,      total_chl_cm2, method = "spearman", use = "complete.obs"),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(level = "fragment")

# =========================================================
# 5. Genotype x treatment-level correlations
# =========================================================
rho_genotype <- dat_genotype_treatment %>%
  group_by(Species) %>%
  dplyr::summarise(
    D_toWhite_cells  = cor(D_toWhite, cells_cm2,     method = "spearman", use = "complete.obs"),
    D_toWhite_chl    = cor(D_toWhite, total_chl_cm2, method = "spearman", use = "complete.obs"),
    DeltaE76_cells   = cor(DeltaE76,  cells_cm2,     method = "spearman", use = "complete.obs"),
    DeltaE76_chl     = cor(DeltaE76,  total_chl_cm2, method = "spearman", use = "complete.obs"),
    FvFm_cells       = cor(FvFm,      cells_cm2,     method = "spearman", use = "complete.obs"),
    FvFm_chl         = cor(FvFm,      total_chl_cm2, method = "spearman", use = "complete.obs"),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(level = "genotype_x_treatment")

# =========================================================
# 6. Combine and export
# =========================================================
robustness_table <- bind_rows(rho_fragment, rho_genotype) %>%
  dplyr::select(Species, level, n, everything()) %>%
  arrange(Species, level) %>%
  mutate(across(where(is.numeric), round, 3))

print(robustness_table)
#write.csv(robustness_table, "../outputs/robustness_check_genotype_level.csv", row.names = FALSE)


# =========================================================
# Outlier sensitivity check: T1-10-1 included vs excluded
# =========================================================

outlier_id <- "T1-10-1"

# ── Check if T1-10-1 exceeds 3 SD from its species x treatment group mean ─────
outlier_check <- dat_final %>%
  group_by(Species, Treatment) %>%
  mutate(
    mean_cells    = mean(cells_cm2,     na.rm = TRUE),
    sd_cells      = sd(cells_cm2,       na.rm = TRUE),
    mean_chl      = mean(total_chl_cm2, na.rm = TRUE),
    sd_chl        = sd(total_chl_cm2,   na.rm = TRUE),
    z_cells       = (cells_cm2     - mean_cells) / sd_cells,
    z_chl         = (total_chl_cm2 - mean_chl)   / sd_chl
  ) %>%
  ungroup() %>%
  filter(ID == "T1-10-1") %>%
  dplyr::select(ID, Species, Treatment,
                cells_cm2, mean_cells, sd_cells, z_cells,
                total_chl_cm2, mean_chl, sd_chl, z_chl)

print(outlier_check)

# ── With outlier excluded (your main analysis) ────────────────────────────────
rho_no_outlier <- dat_final %>%
  filter(ID != outlier_id) %>%
  group_by(Species) %>%
  dplyr::summarise(
    D_toWhite_cells  = cor(D_toWhite, cells_cm2,     method = "spearman", use = "complete.obs"),
    D_toWhite_chl    = cor(D_toWhite, total_chl_cm2, method = "spearman", use = "complete.obs"),
    DeltaE76_cells   = cor(DeltaE76,  cells_cm2,     method = "spearman", use = "complete.obs"),
    DeltaE76_chl     = cor(DeltaE76,  total_chl_cm2, method = "spearman", use = "complete.obs"),
    FvFm_cells       = cor(FvFm,      cells_cm2,     method = "spearman", use = "complete.obs"),
    FvFm_chl         = cor(FvFm,      total_chl_cm2, method = "spearman", use = "complete.obs"),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(outlier = "excluded")

# ── With outlier included ─────────────────────────────────────────────────────
rho_with_outlier <- dat_final %>%
  group_by(Species) %>%
  dplyr::summarise(
    D_toWhite_cells  = cor(D_toWhite, cells_cm2,     method = "spearman", use = "complete.obs"),
    D_toWhite_chl    = cor(D_toWhite, total_chl_cm2, method = "spearman", use = "complete.obs"),
    DeltaE76_cells   = cor(DeltaE76,  cells_cm2,     method = "spearman", use = "complete.obs"),
    DeltaE76_chl     = cor(DeltaE76,  total_chl_cm2, method = "spearman", use = "complete.obs"),
    FvFm_cells       = cor(FvFm,      cells_cm2,     method = "spearman", use = "complete.obs"),
    FvFm_chl         = cor(FvFm,      total_chl_cm2, method = "spearman", use = "complete.obs"),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(outlier = "included")

# ── Combine and export ────────────────────────────────────────────────────────
outlier_sensitivity <- bind_rows(rho_with_outlier, rho_no_outlier) %>%
  dplyr::select(Species, outlier, n, everything()) %>%
  arrange(Species, outlier) %>%
  mutate(across(where(is.numeric), round, 3))

print(outlier_sensitivity)
#write.csv(outlier_sensitivity, "../outputs/outlier_sensitivity_check.csv", row.names = FALSE)

