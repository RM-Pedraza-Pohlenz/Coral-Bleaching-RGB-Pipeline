# =========================================================
# Script: 06_variance_partitioning.R
# Project: Coral bleaching RGB analysis
# Purpose:
#   - Partition variance in symbiont density explained by
#     D_toWhite and a competing top-performing RGB index
#   - Evaluate whether D_toWhite captures largely the same
#     explanatory signal as the best alternative index
# Input:
#   ../outputs/dat_final.csv
# Outputs:
#   Variance partitioning results
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
  "vegan"
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

pred_candidates_reduced <- c(
  "D_toWhite_RGB",
  "MGRVI",
  "Grayscale_weight",
  "DeltaE76",
  "DeltaE2000",
  "FvFm",
  "D_toWhite",
  "host_protein_cm2"
)


# =========================================================
# 2. Load data
# =========================================================

dat_final <- read.csv(input_file)


# =========================================================
# 3. Prepare analysis dataset
# =========================================================

rda_df <- dat_final %>%
  dplyr::select(
    ID,
    Species,
    Treatment,
    Genotype,
    cells_cm2,
    all_of(pred_candidates_reduced)
  ) %>%
  tidyr::drop_na()


# =========================================================
# 4. Standardize variables for variance partitioning
# =========================================================

dat_rda <- rda_df %>%
  mutate(
    cells_cm2 = as.numeric(scale(cells_cm2)),
    D_toWhite = as.numeric(scale(D_toWhite)),
    DeltaE76  = as.numeric(scale(DeltaE76)),
    Species   = factor(Species),
    Treatment = factor(Treatment),
    Genotype  = factor(Genotype)
  )


# =========================================================
# 5. Variance partitioning between D_toWhite and DeltaE76
# =========================================================

vp_dtw_deltae <- varpart(
  dat_rda["cells_cm2"],
  ~ D_toWhite,
  ~ DeltaE76,
  data = dat_rda
)

print(vp_dtw_deltae)