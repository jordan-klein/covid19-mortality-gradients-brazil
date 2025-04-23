```markdown
# SES and COVID-19 Mortality Analysis

This repository contains the R scripts, data structure, and instructions required to reproduce the results and figures from our analysis of socio-economic status (SES) and COVID-19-related mortality under various intervention scenarios.

---

## 🗂 Directory Structure

```
Project/
├── Data/
│   ├── SES/
│   │   └── IVS_IDHM_quintiles.csv
│   ├── Population/
│   │   ├── age_sex_structured_clean.csv
│   │   └── population_density.csv
│   ├── Elections/
│   │   └── Bolsonaro_mean_voteshare.csv
│   ├── Geospatial/
│   │   ├── municipalities_cannonical_unitemporal_full.csv
│   │   └── Municipalities.geojson
│   ├── Parameters/
│   │   ├── CFR.csv
│   │   └── Household_size.csv
│   ├── Mobility/
│   │   ├── network_processed.zip
│   │   └── mvt_range_maps/
│   │       └── Mobility_change_<intervention>.csv
│   ├── Vaccination/
│   │   └── Scenarios/
│   │       └── Vax_rate_<scenario>.csv
│   └── Results/
│       └── Intervention_project/
│           ├── Model_output.zip
│           └── Regression_results.zip
├── ses_quintile_regression.R
├── results_maps.R
├── plot_regression_results_interventions.R
├── extract_deaths_from_model.R
└── Model.R
```

---

## 📋 Requirements

These scripts require the following R packages:
- `tidyverse`
- `data.table`
- `lubridate`
- `fixest`
- `broom`
- `marginaleffects`
- `sf`
- `patchwork`
- `Cairo`
- `deSolve`

Each script will automatically install missing packages.

---

## ⚙️ Running the Scripts

Each script is designed to be run with command-line arguments or within a pipeline. Here's a brief overview:

### 1. `Model.R`
Runs simulations of intervention scenarios.
```bash
Rscript Model.R yes InLoco actual abs_delta Facebook Vax eq .5 1 .189 3.1 no_sensitivity output_filename
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

## 📝 Notes

- Adjust filenames and arguments based on your specific run.
- For double-blind peer review, please anonymize identifiable elements or use [anonymous.4open.science](https://anonymous.4open.science/) to create a reviewer-safe GitHub link.
- For public sharing (e.g., via Zenodo), include the full data archive where license permits.

---

## 📄 License

This project is shared under the [MIT License](LICENSE).

---

## 📫 Contact

For any questions, contact the corresponding author as listed in the manuscript.
```

---

Would you like me to generate this as a downloadable file or add sections for how to prepare the repo for Zenodo or GitHub specifically?
