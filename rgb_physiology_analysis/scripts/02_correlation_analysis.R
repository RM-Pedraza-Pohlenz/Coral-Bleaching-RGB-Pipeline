# =========================================================
# Script: 02_correlation_analysis.R
# Project: Coral bleaching RGB analysis
# Purpose:
#   - Merge corrected RGB data, uncorrected distance metrics,
#     and physiology data
#   - Build wide-format datasets across timepoints
#   - Calculate Spearman correlation matrices
#   - Check normality of variables by species
#   - Generate Fv/Fm trajectory plots across timepoints
#   - Run paired Wilcoxon tests across timepoints
# Inputs:
#   ../data/corrected_rgb_data.csv
#   ../data/uncorrected_rgb_data.csv
#   ../data/physio_data.csv
# Outputs:
#   Correlation matrices
#   boxplot_fvfm_timepoints.pdf
#   wilcox_timepoint_fvfm.csv
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
  "purrr",
  "tibble",
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

build_rgb_wide <- function(rgb_df, physio_df, include_treatment = FALSE) {
  cols_to_keep <- c("ID", "Species", "Phase", "FvFm", "D_toWhite")
  
  if (include_treatment) {
    cols_to_keep <- c(cols_to_keep, "Treatment")
  }
  
  rgb_wide <- rgb_df %>%
    filter(Phase %in% c("I", "Middle", "F")) %>%
    dplyr::select(any_of(cols_to_keep)) %>%
    pivot_wider(
      names_from  = Phase,
      values_from = c(FvFm, D_toWhite),
      names_sep   = "_"
    ) %>%
    left_join(physio_df, by = "ID") %>%
    mutate(
      ratio_m_i        = FvFm_Middle / FvFm_I,
      ratio_f_i        = FvFm_F / FvFm_I,
      ratio_D_toWhite  = D_toWhite_F / D_toWhite_I
    )
  
  rgb_wide
}

check_normality <- function(df) {
  df_num <- df %>%
    dplyr::select(where(is.numeric))
  
  purrr::map_df(names(df_num), ~ {
    x <- df_num[[.x]]
    x <- x[is.finite(x)]
    
    if (length(x) < 3) {
      tibble(
        variable = .x,
        W       = NA_real_,
        p_value = NA_real_,
        n       = length(x)
      )
    } else {
      st <- shapiro.test(x)
      tibble(
        variable = .x,
        W       = unname(st$statistic),
        p_value = st$p.value,
        n       = length(x)
      )
    }
  })
}

plot_dists <- function(df, title_prefix = "") {
  df_num <- df %>%
    dplyr::select(where(is.numeric)) %>%
    pivot_longer(
      everything(),
      names_to = "variable",
      values_to = "value"
    )
  
  p_hist <- ggplot(df_num, aes(x = value)) +
    geom_histogram(bins = 20, color = "black", fill = "grey70") +
    facet_wrap(~ variable, scales = "free", ncol = 4) +
    theme_bw() +
    labs(title = paste0(title_prefix, " – Histograms"))
  
  p_qq <- ggplot(df_num, aes(sample = value)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ variable, scales = "free", ncol = 4) +
    theme_bw() +
    labs(title = paste0(title_prefix, " – QQ plots"))
  
  list(hist = p_hist, qq = p_qq)
}

median_cl_boot <- function(x, conf = 0.95, R = 999) {
  x <- x[!is.na(x)]
  
  boot_medians <- replicate(R, median(sample(x, replace = TRUE)))
  
  data.frame(
    y    = median(x),
    ymin = quantile(boot_medians, (1 - conf) / 2),
    ymax = quantile(boot_medians, 1 - (1 - conf) / 2)
  )
}

make_panel_fvfm <- function(fvfm_long, species_name, box_color, y_min, y_max, treatment_labels) {
  ggplot(
    fvfm_long %>% filter(Species == species_name),
    aes(x = Position, y = FvFm)
  ) +
    annotate(
      "rect",
      xmin = 0.5, xmax = 1.5,
      ymin = -Inf, ymax = Inf,
      fill = "grey90", alpha = 0.3
    ) +
    annotate(
      "rect",
      xmin = 1.5, xmax = 2.5,
      ymin = -Inf, ymax = Inf,
      fill = "grey70", alpha = 0.3
    ) +
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
    facet_wrap(
      ~ Treatment,
      nrow = 1,
      labeller = labeller(Treatment = treatment_labels)
    ) +
    force_panelsizes(rows = unit(45, "mm"), cols = unit(30, "mm")) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    labs(
      title = species_name,
      x = NULL,
      y = expression(F[v] / F[m])
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position    = "none",
      panel.grid         = element_blank(),
      strip.text         = element_text(size = 10),
      strip.background   = element_rect(fill = "grey95", linewidth = 0.3),
      axis.text          = element_text(colour = "black", size = 10),
      axis.title         = element_text(colour = "black", size = 10),
      axis.ticks.length.x = unit(0, "mm"),
      plot.background    = element_rect(fill = "transparent", colour = NA),
      panel.background   = element_rect(fill = "transparent", colour = NA),
      plot.margin        = margin(0, 0, 0, 0, "mm")
    )
}


# =========================================================
# 3. Load input datasets
# =========================================================

rgb_data_raw  <- read.csv(rgb_corrected_file)
physio_data   <- read.csv(physio_file)
distance_data <- read.csv(rgb_uncorrected_file)


# =========================================================
# 4. Clean input datasets
# =========================================================

rgb_data_raw  <- remove_bad_ids(rgb_data_raw, bad_id)
physio_data   <- remove_bad_ids(physio_data, bad_id)
distance_data <- remove_bad_ids(distance_data, bad_id)


# =========================================================
# 5. Compute D_toWhite metrics from uncorrected images
# =========================================================

distance_data <- distance_data %>%
  mutate(
    D_toWhite_RGB = sqrt(
      (R_blank - R_coral)^2 +
        (G_blank - G_coral)^2 +
        (B_blank - B_coral)^2
    )
  ) %>%
  dplyr::select(any_of(c("Tag", "D_toWhite_RGB", "D_toWhite"))) %>%
  drop_na()

distance_data$D_toWhite <- as.numeric(as.character(distance_data$D_toWhite))


# =========================================================
# 6. Merge RGB data with uncorrected distance metrics
# =========================================================

rgb_data <- rgb_data_raw %>%
  left_join(distance_data, by = "Tag")

physio_data <- physio_data %>%
  dplyr::select(
    ID,
    cells_cm2,
    total_chl_cm2,
    host_protein_cm2,
    chl_a_cm2,
    chl_c_cm2
  )


# =========================================================
# 7. Build wide-format dataset across timepoints
# =========================================================

rgb_filter <- build_rgb_wide(
  rgb_df = rgb_data,
  physio_df = physio_data,
  include_treatment = FALSE
)


# =========================================================
# 8. Prepare datasets for correlation analyses
# =========================================================

rgb_corr_data <- rgb_filter %>%
  dplyr::select(-ID, -D_toWhite_Middle, -FvFm_I, -D_toWhite_I)

rgb_chl_data <- rgb_corr_data %>%
  dplyr::select(
    -FvFm_F,
    -FvFm_Middle,
    -D_toWhite_F,
    -cells_cm2,
    -host_protein_cm2,
    -Species,
    -ratio_m_i,
    -ratio_f_i,
    -ratio_D_toWhite
  )

# Table S7.
cor_chl_spearman <- cor(
  rgb_chl_data,
  use = "pairwise.complete.obs",
  method = "spearman"
)

rgb_corr_data <- rgb_corr_data %>%
  dplyr::select(
    -ratio_m_i,
    -ratio_D_toWhite,
    -ratio_f_i,
    -chl_a_cm2,
    -chl_c_cm2
  )


# =========================================================
# 9. Split correlation datasets by species
# =========================================================

rgb_corr_acro <- rgb_corr_data %>%
  filter(Species == "Acropora hemprichii") %>%
  dplyr::select(-Species)

rgb_corr_sty <- rgb_corr_data %>%
  filter(Species == "Stylophora pistillata") %>%
  dplyr::select(-Species)


# =========================================================
# 10. Check normality by species
# =========================================================

norm_acro <- check_normality(rgb_corr_acro)
norm_sty  <- check_normality(rgb_corr_sty)

plots_acro <- plot_dists(rgb_corr_acro, "Acropora")
plots_sty  <- plot_dists(rgb_corr_sty, "Stylophora")

# plots_acro$hist
# plots_acro$qq
# plots_sty$hist
# plots_sty$qq

# Most variables are not normal, so Spearman correlations are used.


# =========================================================
# 11. Calculate correlation matrices
# =========================================================

# Table S5
cor_acro_spearman <- cor(
  rgb_corr_acro,
  use = "pairwise.complete.obs",
  method = "spearman"
)

# Table S6
cor_sty_spearman <- cor(
  rgb_corr_sty,
  use = "pairwise.complete.obs",
  method = "spearman"
)

# write.csv(cor_acro_spearman, "cor_acropora_spearman.csv", row.names = TRUE)
# write.csv(cor_sty_spearman, "cor_stylophora_spearman.csv", row.names = TRUE)


# =========================================================
# 12. Calculate Fv/Fm-only correlations
# =========================================================

fvfm_data <- rgb_filter %>%
  dplyr::select(FvFm_Middle, FvFm_F, ratio_f_i, ratio_m_i)

# Table S8
cor_fvfm <- cor(
  fvfm_data,
  method = "spearman"
)

#FvFm timepoints per Species 
fvfm_data <- rgb_filter %>%
  dplyr::select(FvFm_Middle, FvFm_F, ratio_f_i, ratio_m_i, Species)

cor_fvfm_acro <- fvfm_data %>%
  filter(Species == "Acropora hemprichii") %>%
  dplyr::select(-Species)

cor_fvfm_sty <- fvfm_data %>%
  filter(Species == "Stylophora pistillata") %>%
  dplyr::select(-Species)

cor_fvfm_sp_acro <- cor(
  cor_fvfm_acro,
  method = "spearman"
)

cor_fvfm_sp_sty <- cor(
  cor_fvfm_sty,
  method = "spearman"
)


# =========================================================
# 13. Rebuild wide dataset for Fv/Fm trajectory plotting
# =========================================================

rgb_filter_plot <- build_rgb_wide(
  rgb_df = rgb_data,
  physio_df = physio_data,
  include_treatment = TRUE
)

fvfm_data <- rgb_filter_plot %>%
  dplyr::select(Species, Treatment, FvFm_I, FvFm_Middle, FvFm_F)

fvfm_long <- fvfm_data %>%
  mutate(replicate_id = row_number()) %>%
  pivot_longer(
    cols = c(FvFm_I, FvFm_Middle, FvFm_F),
    names_to = "Position",
    values_to = "FvFm"
  ) %>%
  mutate(
    Position = recode(
      Position,
      FvFm_I      = "Initial",
      FvFm_Middle = "Middle",
      FvFm_F      = "Final"
    ),
    Position = factor(
      Position,
      levels = c("Initial", "Middle", "Final")
    )
  )


# =========================================================
# 14. Figure S3
# =========================================================

y_min <- min(fvfm_long$FvFm, na.rm = TRUE)
y_max <- max(fvfm_long$FvFm, na.rm = TRUE)

treatment_labels <- c(
  "C"  = "32°C",
  "T1" = "34°C",
  "T2" = "36°C",
  "T3" = "38°C"
)

p1 <- make_panel_fvfm(
  fvfm_long = fvfm_long,
  species_name = "Acropora hemprichii",
  box_color = "#9b8d5c",
  y_min = y_min,
  y_max = y_max,
  treatment_labels = treatment_labels
)

p2 <- make_panel_fvfm(
  fvfm_long = fvfm_long,
  species_name = "Stylophora pistillata",
  box_color = "#9b5c89",
  y_min = y_min,
  y_max = y_max,
  treatment_labels = treatment_labels
) +
  theme(axis.title.y = element_blank())

p1 / p2

ggsave(
  "boxplot_fvfm_timepoints.pdf",
  device = cairo_pdf,
  width  = 160,
  height = 150,
  units  = "mm",
  bg     = "transparent"
)


# =========================================================
# 15. Paired Wilcoxon tests across timepoints
# =========================================================

wilcox_timepoint <- fvfm_long %>%
  group_by(Species, Treatment) %>%
  wilcox_test(FvFm ~ Position, paired = TRUE) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  ungroup()

print(wilcox_timepoint)

# write.csv(wilcox_timepoint, "wilcox_timepoint_fvfm.csv", row.names = FALSE)