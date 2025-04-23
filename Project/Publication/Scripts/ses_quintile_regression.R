##### Script to run regressions on model output using SES quintile #####
##### Author: Jordan Klein
# * = to supply via shell
##### Inputs
#### Files
## SES: IVS_IDHM_quintiles.csv
## Population: age_sex_structured_clean.csv
## Elections: Bolsonaro_mean_voteshare.csv
## Geoospatial: municipalities_cannonical_unitemporal_full.csv
## Results: *project*_project/Model_deaths.zip
# Model_deaths/*sensitivity_analysis*/*filename*.csv
# (OR Model_deaths/Empirical.csv)
#### Arguments
# *ses_index* (ivs, idhm)
# *controls* (base, regionFE, stateFE, Bshare, Bshare_regionFE, Bshare_stateFE)
##### Outputs
## Results: *project*_project/Regression_results/
# *sensitivity_analysis*OR*controls*/*filename*_*ses_index*.csv

#### 1) Setup ####
rm(list = ls())

#### Load packages (install if necessary)
required_packages <- c("this.path", "tidyverse", "data.table", "lubridate", "fixest", "broom", 
                       "marginaleffects", "stringr", "dplyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#### Set working directory
project_dir <- here() %>%
  str_extract("^.*Project")
setwd(project_dir)
## Results directory stem
directory_stem <- "Publication/Data/Results/"

#### Supply shell args
model_args <- commandArgs(trailingOnly = TRUE)
project <- str_match_all(model_args[1], "[:graph:].*") %>%
  as.character()
filename <- str_match_all(model_args[2], "[:graph:].*") %>%
  as.character()
ses_index <- str_match_all(model_args[3], "[:graph:].*") %>%
  as.character()
sensitivity_analysis <- str_match_all(model_args[4], "[:graph:].*") %>%
  as.character()
controls <- str_match_all(model_args[5], "[:graph:].*") %>%
  as.character()

#### 2) Build dataset ####
#### SES (exposure: IVS & IDHM quintiles, + gini)
## Import data
SES <- fread("Data/SES/IVS_IDHM_quintiles.csv") %>% 
  rename(gini = i_gini)

#### Population (offset: person-years, + age structure)
## Import data
Pop <- fread("Data/Population/age_sex_structured_clean.csv") %>% 
  filter(year == 2020) %>% 
  rename(muni_code_6dig = muni_code) %>%
  left_join(select(SES, muni_code) %>% 
              mutate(muni_code_6dig = as.numeric(substr(as.character(muni_code), 1, 6))), .) %>%
  group_by(muni_code, age_group) %>% 
  summarise(pop = sum(pop)) %>% 
  data.table()

## Get weekly person-years by municipality
PYs <- group_by(Pop, muni_code) %>% 
  summarise(py = sum(pop) / 53) %>% # 53 epi weeks in 2020
  data.table()

## Age distribution
Age_dist <- Pop %>% 
  group_by(muni_code) %>% 
  mutate(age_dist = pop / sum(pop)) %>% 
  select(muni_code, age_group, age_dist) %>% 
  data.table()
# Get just  proportion 50+ by municipality
Age_50plus <- filter(Age_dist, age_group %in% c("50 a 59 anos", "60 a 69 anos", "70 a 79 anos", "80 anos e mais")) %>% 
  group_by(muni_code) %>% 
  summarise(age_50plus = sum(age_dist)) %>% 
  data.table()

#### Bolsonaro voteshare
## Import data
B_voteshare <- fread("Data/Elections/Bolsonaro_mean_voteshare.csv")

#### Municipality info (state & region fixed effects)
## Import data
muni_info <- fread("Data/Geospatial/municipalities_cannonical_unitemporal_full.csv")

#### Deaths (outcome)
## Load empirical data
# *Supply project via shell*
if (project == "Empirical") {
  Deaths <- fread("Data/Results/Model_deaths/Empirical.csv")
} else {
  ### Load rest of data
  ## Get zip file
  # *Supply project via shell*
  model_deaths_zip <- paste0(directory_stem, project, "_project/Model_deaths.zip")

  ## Get filepath based on sensitivity analysis setting
  # *Supply sensitivity_analysis via shell*
  # *Supply filename via shell*
  if (sensitivity_analysis == "no_sensitivity") {
    filepath <- paste0("Model_deaths/", filename, ".csv")
  } else {
    filepath <- paste0("Model_deaths/", sensitivity_analysis, "/", filename, ".csv")
  }

  ## Read the data - Method using temporary file
  temp <- tempfile()
  conn <- unz(model_deaths_zip, filepath)
  writeLines(readLines(conn), temp)
  close(conn)
  Deaths <- fread(temp)
  file.remove(temp)
}

#### Full regression dataset
# *Supply ses_index via shell*
if (str_length(Deaths$muni_code[1]) == 6) {
  Regression_data <- Deaths %>% 
    rename(muni_code_6dig = muni_code) %>%
    left_join(., select(SES, muni_code, ses = paste0(ses_index, "_quint"), gini) %>%
                mutate(muni_code_6dig = as.numeric(substr(as.character(muni_code), 1, 6)))) %>% 
    left_join(., PYs) %>% 
    left_join(., Age_50plus) %>% 
    left_join(., B_voteshare) %>%
    left_join(., select(muni_info, muni_code = muni_code_7dig, state_code = state_code_a, region)) %>%
    mutate(week = as.factor(week),
           state_code = as.factor(state_code),
           region = as.factor(region))
} else {
  Regression_data <- Deaths %>% 
    left_join(., select(SES, muni_code, ses = paste0(ses_index, "_quint"), gini)) %>% 
    left_join(., PYs) %>% 
    left_join(., Age_50plus) %>% 
    left_join(., B_voteshare) %>%
    left_join(., select(muni_info, muni_code = muni_code_7dig, state_code = state_code_a, region)) %>%
    mutate(week = as.factor(week),
           state_code = as.factor(state_code),
           region = as.factor(region))
}

#### 3) Run regression ####
## Write regression formula
# *Supply controls via shell*
if (controls == "base") {
  reg_formula <- deaths ~ i(week, i.ses, ref2 = 1) + i(week, gini) + i(week, age_50plus) | week
} else if (controls == "regionFE") {
  reg_formula <- deaths ~ i(week, i.ses, ref2 = 1) + i(week, gini) + i(week, age_50plus) | week + region
} else if (controls == "stateFE") {
  reg_formula <- deaths ~ i(week, i.ses, ref2 = 1) + i(week, gini) + i(week, age_50plus) | week + state_code
} else if (controls == "Bshare") {
  reg_formula <- deaths ~ i(week, i.ses, ref2 = 1) + i(week, gini) + i(week, age_50plus) + i(week, mean_share) | week
} else if (controls == "Bshare_regionFE") {
  reg_formula <- deaths ~ i(week, i.ses, ref2 = 1) + i(week, gini) + i(week, age_50plus) + i(week, mean_share) | 
    week + region
} else if (controls == "Bshare_stateFE") {
  reg_formula <- deaths ~ i(week, i.ses, ref2 = 1) + i(week, gini) + i(week, age_50plus) + i(week, mean_share) | 
    week + state_code
}

## Run model
reg_results <- femlm(reg_formula, 
                     Regression_data, offset = ~log(py), 
                     panel.id = c("muni_code", "week"),
                     vcov = "DK", family = "negbin")

#### 4) Get model results (RR of mortality in SES Q5/Q1) ####
#### Function to get model RRs
model_RRs <- function(model) {
  # Get row indices corresponding to coefficients of interest (quintile 5) 
  index <- str_which(names(model$coefficients), "ses::5")
  # RR point estimates
  RR <- exp(model$coefficients[index])
  # Get SEs (using delta method)
  SE <- sqrt(exp(model$coefficients[index])^2 * diag(model$cov.scaled[c(index), c(index)]))
  # RR CIs
  RR_ub <- RR + qnorm(.975) * SE
  RR_lb <- RR + qnorm(.025) * SE
  # Return point estimates & CIs
  result_table <- data.table(RR, RR_lb, RR_ub)
  # Add week variable (if only 25 rows in data, add week 1 w/ NAs)
  if (nrow(result_table) == 25) {
    result_table <- bind_rows(data.table(week = 1, RR = NA, RR_lb = NA, RR_ub = NA),
                              data.table(week = 2:26, result_table))
  } else {
    result_table <- data.table(week = 1:26, result_table)
  }
  # Return results
  return(result_table) # nolint: return_linter.
}

#### Produce model result table
model_results <- model_RRs(reg_results)

#### 5) Export regression results ####
#### Define write directory path (& create if necessary)
# *Supply project via shell*
# *Supply sensitivity_analysis via shell*
# *Supply controls via shell*
if (project != "Empirical") {
  if (sensitivity_analysis == "no_sensitivity" & controls == "base") { # nolint: vector_logic_linter.
    write_dir <- paste0(directory_stem, project, "_project/Regression_results/")
  } else if (sensitivity_analysis == "no_sensitivity") {
    write_dir <- paste0(directory_stem, project, "_project/Regression_results/", controls, "/")
  } else {
    write_dir <- paste0(directory_stem, project, "_project/Regression_results/", sensitivity_analysis, "/")
  }
  if (!dir.exists(write_dir)) {
    dir.create(write_dir, recursive = TRUE)
  }
} else {
  # Write Empirical results to both projects
  if (controls == "base") {
    write_dir1 <- paste0(directory_stem, "Intervention_project/Regression_results/")
    write_dir2 <- paste0(directory_stem, "Mobility_data_project/Regression_results/")
  } else {
    write_dir1 <- paste0(directory_stem, "Intervention_project/Regression_results/", controls, "/")
    write_dir2 <- paste0(directory_stem, "Mobility_data_project/Regression_results/", controls, "/")
  }
  if (!dir.exists(write_dir1)) {
    dir.create(write_dir1, recursive = TRUE)
  }
  if (!dir.exists(write_dir2)) {
    dir.create(write_dir2, recursive = TRUE)
  }
}

#### Define output filename
# *Supply filename via shell*
# *Supply ses_index via shell*
if (str_ends(filename, ses_index)) {
  output_filename <- paste0(filename, ".csv")
} else {
  output_filename <- paste0(filename, "_", ses_index, ".csv")
}

#### Write results
# *Supply project via shell*
if (project != "Empirical") {
  write_dir %>%
    paste0(output_filename) %>%
    fwrite(model_results, .)
} else {
  write_dir1 %>%
    paste0(output_filename) %>%
    fwrite(model_results, .)
  write_dir2 %>%
    paste0(output_filename) %>%
    fwrite(model_results, .)
}