# =========================================================
# Script: 01_rgb_analysis.R
# Project: Coral bleaching RGB analysis
# Purpose:
#   - Load corrected and uncorrected RGB data
#   - Calculate color indices and color-space metrics
#   - Merge RGB data with physiology data
#   - Run per-species association/ranking analysis
# Inputs:
#   ../data/corrected_rgb_data.csv
#   ../data/uncorrected_rgb_data.csv
#   ../data/physio_data.csv
# Outputs:
#   dat_final.csv
#   rank_four.csv
# =========================================================


# =========================================================
# 0. Setup
# =========================================================

rm(list = ls())
if (dev.cur() != 1) dev.off()
options(scipen = 999)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cran_packages <- c(
  "Rmisc", "dplyr", "tidyr",
  "ggtext", "purrr", "broom", "forcats",
  "tibble", "ggnewscale", "farver"
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

bad_id <- "T1-10-1"

rgb_corrected_file   <- "../data/corrected_rgb_data.csv"
rgb_uncorrected_file <- "../data/uncorrected_rgb_data.csv"
physio_file          <- "../data/physio_data.csv"


# =========================================================
# 2. Helper functions
# =========================================================

remove_bad_ids <- function(df, bad_ids) {
  df %>%
    filter(!ID %in% bad_ids) %>%
    droplevels()
}

keep_final_phase <- function(df, final_label = "F") {
  if ("Phase" %in% names(df)) {
    df %>%
      filter(Phase == final_label) %>%
      mutate(Phase = final_label) %>%
      droplevels()
  } else {
    df
  }
}

cv_rmse_grouped_y <- function(df, xvar, yvar, covars = NULL,
                              group_col = "Genotype", k = 5, reps = 100, seed = 42) {
  df <- dplyr::filter(df, stats::complete.cases(.data[[xvar]], .data[[yvar]]))
  if (nrow(df) < 6 || dplyr::n_distinct(df[[group_col]]) < 2) return(NA_real_)
  
  k <- min(k, dplyr::n_distinct(df[[group_col]]))
  set.seed(seed)
  
  terms  <- c(xvar, covars)
  out    <- numeric(reps)
  groups <- unique(df[[group_col]])
  
  for (r in seq_len(reps)) {
    folds <- sample(rep(1:k, length.out = length(groups)))
    rmse_folds <- c()
    
    for (f in seq_len(k)) {
      te_g <- groups[folds == f]
      te   <- df[df[[group_col]] %in% te_g, , drop = FALSE]
      tr   <- df[!(df[[group_col]] %in% te_g), , drop = FALSE]
      
      if (nrow(tr) < 5 || length(unique(tr[[xvar]])) < 2) next
      
      m    <- lm(reformulate(terms, yvar), data = tr)
      pred <- predict(m, newdata = te)
      
      rmse_folds <- c(rmse_folds, sqrt(mean((te[[yvar]] - pred)^2)))
    }
    
    out[r] <- mean(rmse_folds, na.rm = TRUE)
  }
  
  mean(out, na.rm = TRUE)
}

spearman_boot_ci <- function(x, y, R = 1000, conf = 0.95) {
  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)
  if (n < 6) return(list(ci_lower = NA_real_, ci_upper = NA_real_))
  boot_rho <- replicate(R, {
    idx <- sample(n, replace = TRUE)
    cor(x[idx], y[idx], method = "spearman")
  })
  list(
    ci_lower = unname(quantile(boot_rho, (1 - conf) / 2)),
    ci_upper = unname(quantile(boot_rho, 1 - (1 - conf) / 2))
  )
}

metrics_one_species_y <- function(df, xvars, yvar, covars = NULL) {
  do.call(rbind, lapply(xvars, function(xv) {
    dat <- dplyr::filter(df, stats::complete.cases(.data[[xv]], .data[[yvar]]))
    
    if (nrow(dat) < 6 || dplyr::n_distinct(dat[[xv]]) < 2) return(NULL)
    
    fit  <- lm(reformulate(c(xv, covars), yvar), data = dat)
    pred <- predict(fit, newdata = dat)
    
    r_est <- NA_real_
    r_p   <- NA_real_
    ci    <- list(ci_lower = NA_real_, ci_upper = NA_real_)
    
    if (sd(dat[[xv]]) > 0 && sd(dat[[yvar]]) > 0) {
      ct    <- suppressWarnings(cor.test(dat[[xv]], dat[[yvar]], method = "spearman"))
      r_est <- unname(ct$estimate)
      r_p   <- ct$p.value
      ci    <- spearman_boot_ci(dat[[xv]], dat[[yvar]], R = 1000)
    }
    
    tibble::tibble(
      Index       = xv,
      Outcome     = yvar,
      Covars      = paste(covars, collapse = "+"),
      p_spearman  = r_est,
      ci_lower    = ci$ci_lower,   # <-- new
      ci_upper    = ci$ci_upper,   # <-- new
      p_value    = r_p,
      CV_RMSE     = cv_rmse_grouped_y(
        dat, xv, yvar,
        covars    = covars,
        group_col = "Genotype",
        k = 5, reps = 100, seed = 42
      ),
      R2          = summary(fit)$r.squared,
      RMSE_in     = sqrt(mean((dat[[yvar]] - pred)^2)),
      AIC         = AIC(fit),
      BIC         = BIC(fit),
      N           = nrow(dat),
      nGenos      = dplyr::n_distinct(dat$Genotype)
    )
  }))
}

run_all_outcomes_final <- function(df, species_levels, xvars, outcomes, covars = NULL) {
  # Add to the top of run_all_outcomes_final before the map_dfr call
  message("Running analysis for ", length(species_levels), " species x ",
          length(outcomes), " outcomes x ", length(xvars), " indices...")
  purrr::map_dfr(species_levels, function(sp) {
    purrr::map_dfr(outcomes, function(yv) {
      metrics_one_species_y(
        df = dplyr::filter(df, Species == sp),
        xvars = xvars,
        yvar = yv,
        covars = covars
      ) %>%
        dplyr::mutate(Species = sp)
    }) %>%
      dplyr::group_by(Species, Outcome) %>%
      dplyr::mutate(p_adj_BH = p.adjust(p_value, method = "BH")) %>%
      dplyr::ungroup()
  })
}


# =========================================================
# 3. Load input datasets
# =========================================================

rgb_data_raw  <- read.csv(rgb_corrected_file)
physio_data        <- read.csv(physio_file)
distance_data <- read.csv(rgb_uncorrected_file)


# =========================================================
# 4. Clean input datasets
# =========================================================

rgb_data_raw  <- remove_bad_ids(rgb_data_raw, bad_id)
physio_data        <- remove_bad_ids(physio_data, bad_id)
distance_data <- remove_bad_ids(distance_data, bad_id)

fvfm_middle <- rgb_data_raw %>%
  filter(Phase == "Middle") %>%
  dplyr::select(ID, FvFm_Middle = FvFm)

final_rgb <- rgb_data_raw %>%
  tidyr::drop_na() %>%
  keep_final_phase("F")

distance_data <- distance_data %>%
  keep_final_phase("F")


# =========================================================
# 5. Compute color-space variables
# =========================================================

final_rgb <- final_rgb %>%
  mutate(
    R01 = R_coral / 255,
    G01 = G_coral / 255,
    B01 = B_coral / 255
  )

hsv_mat <- grDevices::rgb2hsv(
  rbind(
    R01 = final_rgb$R01,
    G01 = final_rgb$G01,
    B01 = final_rgb$B01
  )
)

final_rgb <- final_rgb %>%
  mutate(
    H_hsv = as.numeric(hsv_mat["h", ]),
    S_hsv = as.numeric(hsv_mat["s", ]),
    V_hsv = as.numeric(hsv_mat["v", ])
  )

lab_coral <- convert_colour(
  colour       = as.matrix(final_rgb[, c("R_coral", "G_coral", "B_coral")]),
  from = "rgb",
  to   = "lab"
)
colnames(lab_coral) <- c("L_coral", "a_coral", "b_coral")

lab_blank <- convert_colour(
  colour       = as.matrix(final_rgb[, c("R_blank", "G_blank", "B_blank")]),
  from = "rgb",
  to   = "lab"
)
colnames(lab_blank) <- c("L_blank", "a_blank", "b_blank")

final_rgb <- final_rgb %>%
  bind_cols(as.data.frame(lab_coral), as.data.frame(lab_blank)) %>%
  mutate(
    DeltaE76 = sqrt((L_coral - L_blank)^2 +
                      (a_coral - a_blank)^2 +
                      (b_coral - b_blank)^2),
    Chroma_coral = sqrt(a_coral^2 + b_coral^2),
    DeltaE2000 = diag(
      compare_colour(
        from       = lab_coral,
        to         = lab_blank,
        from_space = "lab",
        method     = "cie2000"
      )
    )
  )


# =========================================================
# 6. Compute RGB-derived indices
# =========================================================

final_rgb <- final_rgb %>%
  mutate(
    R_index = R_coral,
    G_index = G_coral,
    B_index = B_coral,
    RGB_sum = R_coral + G_coral + B_coral,
    
    r_norm = R_coral / RGB_sum,
    g_norm = G_coral / RGB_sum,
    b_norm = B_coral / RGB_sum,
    
    Grayscale_weight = 0.299 * R_coral + 0.587 * G_coral + 0.114 * B_coral,
    Grayscale_norm   = r_norm * R_coral + g_norm * G_coral + b_norm * B_coral,
    int              = RGB_sum / 3,
    
    R_plus_G  = R_coral + G_coral,
    R_plus_B  = R_coral + B_coral,
    R_minus_G = R_coral - G_coral,
    R_minus_B = R_coral - B_coral,
    G_minus_B = G_coral - B_coral,
    
    g_minus_r = g_norm - r_norm,
    g_minus_b = g_norm - b_norm,
    r_minus_b = r_norm - b_norm,
    
    NRBI = (R_coral - B_coral) / (R_coral + B_coral),
    NGRI = (G_coral - R_coral) / (G_coral + R_coral),
    NGBI = (G_coral - B_coral) / (G_coral + B_coral),
    
    KI = R_coral / (R_coral - B_coral),
    
    D_toWhite_RGB_corrected = if (all(c("R_blank", "G_blank", "B_blank") %in% names(final_rgb))) {
      sqrt((R_blank - R_coral)^2 +
             (G_blank - G_coral)^2 +
             (B_blank - B_coral)^2)
    } else {
      NA_real_
    },
    
    ExG   = 2 * G_coral - R_coral - B_coral,
    ExG_2 = (2 * G_coral - R_coral - B_coral) / RGB_sum,
    
    VEG = G_coral / ((R_coral^0.667) * (B_coral^(1 - 0.667))),
    
    ExR   = 1.4 * r_norm - g_norm,
    ExB   = 1.4 * b_norm - g_norm,
    ExGR  = (2 * G_coral - R_coral - B_coral) - (1.4 * r_norm - g_norm),
    ExGR_2 = ((2 * G_coral - R_coral - B_coral) / RGB_sum) - (1.4 * r_norm - g_norm),
    
    SAVI  = (1.5 * (g_norm - r_norm)) / ((g_norm + r_norm) + 0.5),
    OSAVI = (1.5 * (g_norm - r_norm)) / (g_norm + r_norm + 0.16),
    EVI   = (2.5 * (g_norm - r_norm)) / (g_norm + 6 * r_norm - 7.5 * b_norm + 1),
    EVI_2 = (2.5 * (g_norm - r_norm)) / (g_norm + 2.4 * r_norm + 1),
    
    VDVI  = (2 * G_coral - R_coral - B_coral) / (2 * G_coral + R_coral + B_coral),
    VARI  = (G_coral - R_coral) / (G_coral + R_coral - B_coral),
    MGRVI = (G_coral^2 - R_coral^2) / (G_coral^2 + R_coral^2),
    
    CIVE_raw = 0.441 * R_coral - 0.881 * G_coral + 0.385 * B_coral + 18.787,
    WI       = (G_coral - B_coral) / (R_coral - G_coral),
    WI_old   = (G_coral - B_coral) / (G_coral + R_coral),
    
    redness_index = (R_coral^2) / (G_coral * B_coral),
    chroma_index  = sqrt((R_coral - G_coral)^2 +
                           (R_coral - B_coral)^2 +
                           (G_coral - B_coral)^2),
    hue_angle = atan2(
      sqrt(3) * (G_coral - B_coral),
      2 * R_coral - G_coral - B_coral
    ),
    
    NDI   = (r_norm - g_norm) / (r_norm + g_norm + 0.01),
    GLI2  = (2 * G_coral - R_coral + B_coral) / (2 * G_coral + R_coral + B_coral),
    IPCA  = 0.994 * abs(R_coral - B_coral) +
      0.961 * abs(G_coral - B_coral) +
      0.914 * abs(G_coral - R_coral),
    RGBVI = (G_coral^2 - (B_coral * R_coral)) / (G_coral^2 + (B_coral * R_coral)),
    
    CIVE_norm = 0.441 * r_norm - 0.811 * g_norm + 0.3856 * b_norm + 18.79,
    
    COM_raw  = 0.25 * (2 * G_coral - R_coral - B_coral) +
      0.30 * ((2 * G_coral - R_coral - B_coral) - (1.4 * r_norm - g_norm)) +
      0.33 * (0.441 * R_coral - 0.881 * G_coral + 0.385 * B_coral + 18.787) +
      0.12 * VEG,
    
    COM_norm = 0.25 * (2 * G_coral - R_coral - B_coral) +
      0.30 * (((2 * G_coral - R_coral - B_coral) / RGB_sum) - (1.4 * r_norm - g_norm)) +
      0.33 * (0.441 * r_norm - 0.811 * g_norm + 0.3856 * b_norm + 18.79) +
      0.12 * VEG,
    
    ChOL = plogis((G_coral - R_coral / 3 - B_coral / 3) / 255)
  ) %>%
  tidyr::drop_na(R_coral, G_coral, B_coral)


# =========================================================
# 7. Compute PCA axes from RGB channels
# =========================================================

pcaF_mat <- final_rgb %>%
  dplyr::select(R_coral, G_coral, B_coral) %>%
  scale()

pcaF <- princomp(pcaF_mat)

scoresF <- tibble(
  ID    = final_rgb$ID,
  F_PC1 = as.numeric(pcaF$scores[, 1]),
  F_PC2 = as.numeric(pcaF$scores[, 2])
)

final_rgb <- final_rgb %>%
  left_join(scoresF, by = "ID") %>%
  left_join(fvfm_middle, by = "ID")


# =========================================================
# 8. Define RGB-derived variables to keep
# =========================================================

index_vars <- c(
  "FvFm",
  "R_index", "G_index", "B_index",
  "r_norm", "g_norm", "b_norm",
  "Grayscale_weight", "int", "Grayscale_norm",
  "R_plus_G", "R_plus_B",
  "R_minus_G", "R_minus_B", "G_minus_B",
  "g_minus_r", "g_minus_b", "r_minus_b",
  "NRBI", "NGRI", "NGBI",
  "KI", "D_toWhite_RGB_corrected",
  "ExG", "ExG_2", "VEG", "ExR", "ExB", "ExGR", "ExGR_2",
  "SAVI", "OSAVI", "EVI", "EVI_2", "VDVI", "VARI", "MGRVI",
  "CIVE_raw", "WI", "WI_old",
  "redness_index", "chroma_index", "hue_angle",
  "NDI", "GLI2", "IPCA", "RGBVI",
  "CIVE_norm", "COM_raw", "COM_norm", "ChOL",
  "H_hsv", "S_hsv", "V_hsv",
  "F_PC1", "F_PC2",
  "FvFm_Middle",
  "L_coral", "a_coral", "b_coral",
  "Chroma_coral",
  "DeltaE76",
  "DeltaE2000"
)


# =========================================================
# 9. Assemble merged final dataset
# =========================================================

dat_final <- final_rgb %>%
  dplyr::select(any_of(c(
    "ID", "Species", "Treatment", "Genotype", "Replicate", "Temperature",
    index_vars
  ))) %>%
  left_join(physio_data, by = "ID")


# =========================================================
# 10. Add uncorrected-image distance metrics
# =========================================================

distance_data <- distance_data %>%
  mutate(
    D_toWhite_RGB = if (all(c("R_blank", "G_blank", "B_blank", "R_coral", "G_coral", "B_coral") %in% names(.))) {
      sqrt((R_blank - R_coral)^2 +
             (G_blank - G_coral)^2 +
             (B_blank - B_coral)^2)
    } else {
      NA_real_
    }
  ) %>%
  dplyr::select(any_of(c("ID", "D_toWhite_RGB", "D_toWhite")))

distance_data$D_toWhite <- as.numeric(as.character(distance_data$D_toWhite))


dat_final <- dat_final %>%
  left_join(distance_data, by = "ID")

index_vars <- c(index_vars, "D_toWhite_RGB", "D_toWhite")


# =========================================================
# 11. Define predictors and outcomes
# =========================================================

xvars <- setdiff(index_vars, c("FvFm", "FvFm_Middle"))

outcomes <- c(
  "FvFm",
  "FvFm_Middle",
  "cells_cm2",
  "chl_a_cm2",
  "chl_c_cm2",
  "total_chl_cm2",
  "chl_per_cell",
  "host_protein_cm2",
  "cells_prot",
  "chl_a_prot",
  "chl_c_prot",
  "total_chl_prot"
)


# =========================================================
# 12. Run per-species association analysis
# =========================================================

res_panel_final <- run_all_outcomes_final(
  df = dat_final,
  species_levels = unique(dat_final$Species),
  xvars = xvars,
  outcomes = outcomes,
  covars = NULL
)


# =========================================================
# 13. Diagnostics for ranking table
# =========================================================

cols_sort <- c("p_spearman", "CV_RMSE", "R2", "p_value", "RMSE_in", "AIC", "BIC", "p_adj_BH")

sapply(res_panel_final[cols_sort], function(x) paste(class(x), collapse = "/"))
sapply(res_panel_final[cols_sort], is.list)


# =========================================================
# 14. Rank indices for four focal outcomes
# =========================================================

outcome_a <- "cells_cm2"
outcome_b <- "FvFm"
outcome_c <- "total_chl_cm2"
outcome_d <- "host_protein_cm2"

rank_four <- res_panel_final %>%
  dplyr::filter(Outcome %in% c(outcome_a, outcome_b, outcome_c, outcome_d)) %>%
  dplyr::group_by(Species, Outcome) %>%
  dplyr::arrange(
    desc(abs(p_spearman)),
    p_adj_BH,
    CV_RMSE,
    p_value,
    RMSE_in,
    AIC,
    BIC,
    .by_group = TRUE
  ) %>%
  dplyr::mutate(Rank = row_number()) %>%
  dplyr::ungroup()


# =========================================================
# 15. Optional exports
# =========================================================

# write.csv(dat_final, "../outputs/dat_final.csv", row.names = FALSE)
# write.csv(rank_four, "../outputs/rank_four.csv", row.names = FALSE)