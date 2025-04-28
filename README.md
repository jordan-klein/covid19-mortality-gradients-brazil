# Must Whatever Goes up Come Down? Mortality Gradients in the Emergence of COVID-19

This repository contains the R scripts, data structure, and instructions required to reproduce the results and figures from this manuscript:

Klein, J.D., 2025. Must Whatever Goes up Come Down? Mortality Gradients in the Emergence of COVID-19. medRxiv. [https://doi.org/10.1101/2025.04.23.25326307](https://doi.org/10.1101/2025.04.23.25326307)


---

## 🗂 Directory Structure

```
Project/
├── Data/
    ├── SES/
    │   └── IVS_IDHM_quintiles.csv
    ├── Population/
    │   ├── age_sex_structured_clean.csv
    │   └── population_density.csv
    ├── Elections/
    │   └── Bolsonaro_mean_voteshare.csv
    ├── Geospatial/
    │   ├── municipalities_cannonical_unitemporal_full.csv
    │   └── Municipalities.geojson
    ├── Parameters/
    │   ├── CFR.csv
    │   └── Household_size.csv
    ├── Mobility/
    │   ├── network_processed.zip
    │   └── mvt_range_maps/
    │       ├── Mobility_change_actual.csv
    │       └── Mobility_change_equality.csv
    └── Vaccination/
        └── Scenarios/
            ├── Vax_rate_actual.csv
            ├── Vax_rate_equality.csv
            ├── Vax_rate_priority_ivs.csv
            └── Vax_rate_priority_idhm.csv
```
📦 Data Archive

All large data files used in this project that are not stored in this github repo are available on Zenodo:

🔗 [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15272578.svg)](https://doi.org/10.5281/zenodo.15272578)
---

## 📋 Requirements

These scripts require the following R packages:
- `this.path`
- `tidyverse`
- `data.table`
- `lubridate`
- `fixest`
- `broom`
- `marginaleffects`
- `stringr`
- `dplyr`
- `sf`
- `svglite`
- `scales`
- `ggallin`
- `patchwork`
- `Cairo`
- `deSolve`

Each script will automatically install missing packages.

---

## ⚙️ Running the Scripts

Each script is designed to be run with command-line arguments or within a pipeline. Here's a brief overview with some illustrative examples (adjust filenames and arguments based on your specific run):

### 1. `Model.R`
Runs simulations of intervention scenarios.
```bash
Rscript Model.R yes InLoco actual abs_delta Facebook equality .5 1 .189 3.1 no_sensitivity output_filename
```

### 2. `extract_deaths_from_model.R`
Extracts weekly deaths from model output.
```bash
Rscript extract_deaths_from_model.R yes .5 1 .189 3.1 no_sensitivity output_filename
```

### 3. `ses_quintile_regression.R`
Runs regressions using SES and demographic data.
```bash
Rscript ses_quintile_regression.R Intervention filename idhm no_sensitivity base
```

### 4. `plot_regression_results_interventions.R`
Plots regression results from zipped outputs.
```bash
Rscript plot_regression_results_interventions.R base
```

### 5. `results_maps.R`
Generates choropleth maps of mortality.
```bash
Rscript results_maps.R yes output_filename
```

---

## 📄 License

This repository is released under the **[CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/)** license. You are free to copy, modify, distribute, and use the work, even for commercial purposes, without asking permission.

> **Note**: If you use or build on this work, citation is appreciated but not required.

---

## 📫 Contact

For any questions, contact the corresponding author as listed in the manuscript.
