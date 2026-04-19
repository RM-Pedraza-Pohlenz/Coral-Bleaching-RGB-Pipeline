# "Coral Bleaching RGB Pipeline"

This repository contains the scripts and workflow used to extract RGB-derived color metrics from standardized coral photographs and compare them with physiological bleaching metrics. The pipeline was developed for coral bleaching assessment in CBASS-style acute heat stress experiments.

## Overview

The repository includes:

- image segmentation and relabeling of coral fragments
- blank-image processing for white-reference extraction
- extraction and collation of RGB-based color metrics
- downstream analysis combining RGB-derived and physiological variables
- correlation analyses and figure generation

## Repository structure

```text
Coral-Bleaching-RGB-Pipeline/
├── README.md
├── environment.yml
├── rgb_analysis/
│   ├── data/
│   │   ├── 01_raw/
│   │   ├── 02_interim/
│   │   ├── 03_rename/
│   │   ├── 04_blank_folders/
│   │   ├── 05_rename_blanks/
│   │   ├── 06_combine/
│   │   └── 07_final/
│   ├── notebooks/
│   └── outputs/
└── rgb_physiology_analysis/
    ├── checkpoints/
    ├── data/
    ├── outputs/
    └── scripts/
    
```

`rgb_analysis/` contains the Python-based workflow for image segmentation, relabeling, blank processing, and RGB metric extraction.

`rgb_physiology_analysis/` contains the merged datasets and R scripts used for downstream statistical analyses and figure generation combining RGB-derived and physiological variables.

## Getting started

### Hardware specifications used

- CPU: 13th Gen Intel Core i9-13980HX @ 2.20 GHz
- GPU: NVIDIA GeForce RTX 4090 Laptop GPU (16 GB VRAM)
- RAM: 32 GB
- Storage: 1.86 TB NVMe SSD
- System: 64-bit, x64-based processor

### Requirements

- Ubuntu / WSL environment
- Conda environment: `coralscop`
- Python scripts located in `rgb_analysis/notebooks/`
- R scripts located in `rgb_physiology_analysis/scripts/`

### Installation

#### 1. Prepare the environment

Install Miniconda or Miniforge, then create and activate the environment:

```bash
conda env create -f environment.yml -p $PWD/env
conda activate $PWD/env
```

#### 2. Download the pre-trained weights for CoralSCOP

We use the trained CoralSCOP model (ViT-B backbone). Download the weights and save them in the `checkpoints` folder.

## RGB image analysis workflow

Move to the notebook directory:

```bash
cd rgb_analysis/notebooks
```

### 1. Segment coral photos

```bash
python 01_segmentation_ocr_script.py ../data/01_raw 375 clockwise
```

Arguments:

- `main_path`: folder containing the input photos
- `threshold`: distance from the center of the label to the coral fragment
- `rotate_option`: `clockwise`, `counterclockwise`, or `none`

To show usage:

```bash
python 01_segmentation_ocr_script.py
```

### 2. Relabel segmented fragments

Rename segmented photos to positions 1–25.

Inside each photo folder, include an `order.csv` file containing:

- the original position (1–25)
- the new fragment name (for example `T2-1-1`)

**Important:**

- the 1–25 order must match the reference image
- double-check the fragment order before continuing

Move the folders to `../data/03_rename`, then run:

```bash
for folder in ../data/03_rename/*; do
  echo "$(basename "$folder")"
  python 02_rename_files.py "$folder"
done
```

### 3. Prepare blank images

Crop the white blank from each photo and place the files in `../data/04_blank_folders`.

Create blank subfolders:

```bash
python3 03_organize_blanks.py
```

This creates the new directories in `../data/05_rename_blanks`.

### 4. Transfer order.csv to blank folders

Copy the coral fragment folders containing `order.csv` into `../data/05_rename_blanks`.

At this stage, the folder should contain:

- coral fragment folders, for example `P9241983`
- blank folders, for example `P9241983_-_Copy`

```bash
python3 04_blank_move.py
```

### 5. Rename blank files

```bash
python3 05_rename_blanks.py
```

### 6. Organize coral and blank folders by experiment

For each experiment, create a folder with this structure:

```text
Trial
├── corals
└── blanks
```

Each folder should contain the 25 relabeled images. Move the experiment folder to `../data/06_combine`, then run:

```bash
python3 06_organize_combined.py
```

**Note:** this script only creates output folders when both coral and blank files are present. This is useful for identifying missing pairs.

### 7. Extract color metrics

```bash
python 07_execute_and_collate_color_clustering.py --f ../data/07_final/trial --o trial
```

Arguments:

- `--f`: folder containing the final image folders
- `--o`: name of the output folder for the summary CSV

**Notes:**

- image folders must contain both fragment and white-reference images
- results are saved in the output folder

## R analysis scripts

The R scripts are organized as follows:

- `01_rgb_analysis.R` — prepares RGB-derived indices and merges them with physiology data
- `02_correlation_analysis.R` — correlation analyses across RGB and physiological variables
- `03_boxplot_visualization.R` — treatment-wise boxplots and non-parametric tests
- `04_heatmap_figure.R` — heatmap of top-performing indices across focal outcomes
- `05_scatterplot_figure.R` — scatterplots comparing Fv/Fm and D_toWhite
- `06_variance_partitioning.R` — variance partitioning analysis comparing D_toWhite with competing indices

## Outputs

Generated files are saved in the `outputs/` directories, including:

- merged datasets
- ranking tables
- boxplots
- heatmaps
- scatterplots
- supplementary statistical results

## Notes

This pipeline was developed for standardized coral bleaching assessment using digital photography and RGB-based color metrics. It is intended for use with CBASS-style experiments and paired physiological measurements such as Fv/Fm, symbiont density, chlorophyll content, and host protein.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.