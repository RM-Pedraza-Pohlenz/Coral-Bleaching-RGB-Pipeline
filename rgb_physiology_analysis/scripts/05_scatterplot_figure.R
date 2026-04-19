# =========================================================
# Script: 05_scatterplot_figure.R
# Project: Coral bleaching RGB analysis
# Purpose:
#   - Compare Fv/Fm and D_toWhite as predictors of key
#     bleaching-related physiological traits
#   - Plot both metrics on a shared x-axis by rescaling
#     D_toWhite onto the Fv/Fm range
#   - Generate publication-style scatterplots by species
# Input:
#   ../outputs/dat_final.csv
# Outputs:
#   fig_scatter_cells.pdf
#   fig_scatter_chla.pdf
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
  "ggplot2",
  "scales"
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

base_size   <- 10
base_family <- "sans"

pt_size <- 1.6
line_w  <- 0.5

metric_cols <- c(
  "Fv/Fm"     = "#1a6e8a",
  "D_toWhite" = "#a0522d"
)

xmin <- -0.02
xmax <- 0.50


# =========================================================
# 2. Load input data
# =========================================================

dat_final <- read.csv(input_file)


# =========================================================
# 3. Define rescaling between D_toWhite and Fv/Fm
# =========================================================

rng_f <- range(dat_final$FvFm, na.rm = TRUE)
rng_n <- range(dat_final$D_toWhite, na.rm = TRUE)

to_fvfm <- function(n) {
  scales::rescale(n, to = rng_f, from = rng_n)
}

to_dtw <- function(f) {
  scales::rescale(f, to = rng_n, from = rng_f)
}


# =========================================================
# 4. Helper function for shared scatterplot style
# =========================================================

make_dual_metric_scatter <- function(df, yvar, ylab_expr, output_file) {
  
  df_plot <- bind_rows(
    df %>% transmute(Species, Metric = "Fv/Fm", x = FvFm, y = .data[[yvar]]),
    df %>% transmute(Species, Metric = "D_toWhite", x = to_fvfm(D_toWhite), y = .data[[yvar]])
  ) %>%
    mutate(Metric = factor(Metric, levels = c("Fv/Fm", "D_toWhite")))
  
  y_rng <- range(df_plot$y, na.rm = TRUE)
  
  p <- ggplot(df_plot, aes(x = x, y = y, color = Metric)) +
    geom_point(alpha = 1, size = pt_size) +
    geom_smooth(
      aes(fill = Metric),
      method = "lm",
      se = TRUE,
      linewidth = line_w,
      alpha = 0.2
    ) +
    facet_wrap(~ Species, nrow = 1) +
    coord_cartesian(
      xlim = c(xmin, xmax),
      ylim = c(y_rng[1], y_rng[2] * 1.25)
    ) +
    scale_color_manual(values = metric_cols, name = NULL) +
    scale_fill_manual(values = metric_cols, guide = "none") +
    scale_x_continuous(
      breaks = pretty(c(xmin, xmax), n = 5),
      labels = function(x) {
        paste0(
          sprintf("%.2f", x),
          "\n",
          sprintf("%.1f", to_dtw(x))
        )
      },
      name = "Fv/Fm\nD_toWhite",
      expand = expansion(mult = c(0, 0))
    ) +
    labs(y = ylab_expr) +
    theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(linewidth = 0.4, color = "grey20"),
      strip.text         = element_text(face = "bold", size = base_size),
      strip.background   = element_rect(fill = "grey95", linewidth = 0.3),
      axis.text          = element_text(size = base_size, color = "black"),
      axis.title         = element_text(size = base_size, face = "bold", color = "black"),
      panel.spacing.x    = unit(6, "pt"),
      plot.margin        = margin(6, 6, 6, 6, unit = "pt"),
      legend.position    = "right",
      legend.text        = element_text(size = base_size),
      legend.key.height  = unit(10, "pt"),
      legend.key.width   = unit(10, "pt")
    )
  
  print(p)
  
  ggsave(
    filename = output_file,
    plot = p,
    device = cairo_pdf,
    width = 7.2,
    height = 3.5,
    units = "in"
  )
}


# =========================================================
# 5. Generate scatterplot for symbiont density
# =========================================================

make_dual_metric_scatter(
  df = dat_final,
  yvar = "cells_cm2",
  ylab_expr = expression(paste("Symbiont density (cells/", cm^2, ")")),
  output_file = "../outputs/fig_scatter_cells.pdf"
)


# =========================================================
# 6. Generate scatterplot for total chlorophyll
# =========================================================

make_dual_metric_scatter(
  df = dat_final,
  yvar = "total_chl_cm2",
  ylab_expr = expression(paste("Total chlorophyll (", mu, "g/", cm^2, ")")),
  output_file = "../outputs/fig_scatter_chla.pdf"
)
