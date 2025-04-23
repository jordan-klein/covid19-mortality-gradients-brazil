##### Script to produce SES quintile regression tables (for intervention project) #####
##### Author: Jordan Klein
# * = to supply via shell
##### Inputs
#### Files
## SES: IVS_IDHM_quintiles.csv
## Population: age_sex_structured_clean.csv
## Elections: Bolsonaro_mean_voteshare.csv
## Geoospatial: municipalities_cannonical_unitemporal_full.csv
## Results: Intervention_project/Model_deaths.zip
# Model_deaths/*filename*.csv (not doing for sensitivity analyses)
# (OR Model_deaths/Empirical.csv)
#### Arguments
# *ses_index* (ivs, idhm)
##### Outputs
# Tables: Intervention_project/regtbl_*filename*_*ses_index*.tex
# Tables: Intervention_project/regtbl_*filename*_*ses_index*_Bshare.tex

#### 1) Setup ####
rm(list = ls())

#### Load packages
required_packages <- c("this.path", "tidyverse", "data.table", "lubridate", "fixest", 
                       "broom", "marginaleffects", "tinytex", "pdftools", "magick")
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

#### Supply shell args
model_args <- commandArgs(trailingOnly = TRUE)
filename <- str_match_all(model_args[1], "[:graph:].*") %>%
  as.character()
ses_index <- str_match_all(model_args[2], "[:graph:].*") %>%
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
# *Supply filename via shell*
if (filename == "Empirical") {
  Deaths <- fread("Data/Results/Model_deaths/Empirical.csv")
} else {
  ### Load rest of data
  ## Get zip file
  # *Supply project via shell*
  model_deaths_zip <- "Publication/Data/Results/Intervention_project/Model_deaths.zip"

  ## Get filepath
  # *Supply filename via shell*
  filepath <- paste0("Model_deaths/", filename, ".csv")

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
                mutate(muni_code_6dig = as.numeric(substr(as.character(muni_code), 1, 6))))
} else {
  Regression_data <- Deaths %>% 
    left_join(., select(SES, muni_code, ses = paste0(ses_index, "_quint"), gini))
}
Regression_data <- Regression_data %>% 
  left_join(., PYs) %>% 
  left_join(., Age_50plus) %>% 
  left_join(., B_voteshare) %>%
  left_join(., select(muni_info, muni_code = muni_code_7dig, state_code = state_code_a, region)) %>%
  mutate(Week = as.factor(week),
         State = as.factor(state_code),
         Region = as.factor(region)) %>% 
  rename(Deaths = deaths)

#### 3) Run regressions ####
## Write regression formulae
base_formula <- Deaths ~ i(Week, i.ses, ref2 = 1) + i(Week, gini) + i(Week, age_50plus) | Week
regionFE_formula <- Deaths ~ i(Week, i.ses, ref2 = 1) + i(Week, gini) + i(Week, age_50plus) | Week + Region
stateFE_formula <- Deaths ~ i(Week, i.ses, ref2 = 1) + i(Week, gini) + i(Week, age_50plus) | Week + State
Bshare_formula <- Deaths ~ i(Week, i.ses, ref2 = 1) + i(Week, gini) + i(Week, age_50plus) + i(Week, mean_share) | Week
Bshare_regionFE_formula <- Deaths ~ i(Week, i.ses, ref2 = 1) + i(Week, gini) + i(Week, age_50plus) + i(Week, mean_share) | Week + Region
Bshare_stateFE_formula <- Deaths ~ i(Week, i.ses, ref2 = 1) + i(Week, gini) + i(Week, age_50plus) + i(Week, mean_share) | Week + State

## Run models
base_results <- femlm(base_formula, 
                     Regression_data, offset = ~log(py), 
                     panel.id = c("muni_code", "Week"),
                     vcov = "DK", family = "negbin")
regionFE_results <- femlm(regionFE_formula, 
                      Regression_data, offset = ~log(py), 
                      panel.id = c("muni_code", "Week"),
                      vcov = "DK", family = "negbin")
stateFE_results <- femlm(stateFE_formula, 
                      Regression_data, offset = ~log(py), 
                      panel.id = c("muni_code", "Week"),
                      vcov = "DK", family = "negbin")
Bshare_results <- femlm(Bshare_formula, 
                      Regression_data, offset = ~log(py), 
                      panel.id = c("muni_code", "Week"),
                      vcov = "DK", family = "negbin")
Bshare_regionFE_results <- femlm(Bshare_regionFE_formula, 
                      Regression_data, offset = ~log(py), 
                      panel.id = c("muni_code", "Week"),
                      vcov = "DK", family = "negbin")
Bshare_stateFE_results <- femlm(Bshare_stateFE_formula, 
                      Regression_data, offset = ~log(py), 
                      panel.id = c("muni_code", "Week"),
                      vcov = "DK", family = "negbin")

#### 4) Generate regression tables ####
#### Get scenario-specific text
# *Supply filename via shell*
if(filename == "Empirical") {
  scenario_text <- "empirical/real-world deaths"
} else if (filename == "Baseline") {
  scenario_text <- "simulated deaths with no interventions"
} else if (filename == "NPI") {
  scenario_text <- "simulated deaths with NPIs as in real world"
} else if (filename == "Vax") {
  scenario_text <- "simulated deaths with vaccination as in real world"
} else if (filename == "NPI_Vax") {
  scenario_text <- "simulated deaths with NPIs and vaccination as in real world"
} else if (filename == "NPI_eq") {
  scenario_text <- "simulated deaths with equal NPIs"
} else if (filename == "Vax_eq") {
  scenario_text <- "simulated deaths with equal vaccination"
} else if (str_starts(filename, "Vax_pr")) {
  scenario_text <- "simulated deaths with equitable vaccination"
} else if (filename == "NPI_eq_Vax_eq") {
  scenario_text <- "simulated deaths with equal NPIs and vaccination"
} else {
  scenario_text <- "simulated deaths with equal NPIs and equitable vaccination"
}

## Get name of regression table
# *Supply filename via shell*
# *Supply ses_index via shell*
if (str_ends(filename, ses_index)) {
  regtbl_name <- paste("regtbl", filename, sep = "_")
} else {
  regtbl_name <- paste("regtbl", filename, ses_index, sep = "_")
}

## Function to remove SES from regression table & change variables -> SES index quintile + reference cat
# *Supply ses_index via shell*
table_edit <- function(x) {
  str_remove_all(x, "ses [:digit:]") %>% 
    str_replace_all("Variables", paste(str_to_upper(ses_index), "Q5 (reference = Q1)"))
}

## Output table for models w/o Bolsonaro vote share
# *Supply ses_index via shell*
etable(base_results, regionFE_results, stateFE_results,
       keep = c("%ses::5"), file = paste0("Publication/Tables/Intervention_project/", regtbl_name, ".tex"), 
       replace = T, label = regtbl_name, fixef_sizes = T, powerBelow = -4, 
       digits = 3, digits.stats = "r3", tex = T, se.below = F, 
       interaction.combine = "", i.equal = " ", depvar = F, tpt = T,
       fitstat = c("n", "pr2", "bic", "theta"),
       postprocess.tex = table_edit, 
       notes = "Offset = ln(person-years)", 
       title = paste0("Estimated coefficients of ", 
                     str_to_upper(ses_index), 
                     " quintile 5 by week from negative binomial regression model of ", 
                     scenario_text, ".",
                     " Controls: population aged 50+, gini."))

## Output table for models w/ Bolsonaro vote share
# *Supply ses_index via shell*
etable(Bshare_results, Bshare_regionFE_results, Bshare_stateFE_results,
       keep = c("%ses::5"), file = paste0("Publication/Tables/Intervention_project/", regtbl_name, "_Bshare.tex"), 
       replace = T, label = regtbl_name, fixef_sizes = T, powerBelow = -4, 
       digits = 3, digits.stats = "r3", tex = T, se.below = F, 
       interaction.combine = "", i.equal = " ", depvar = F, tpt = T,
       fitstat = c("n", "pr2", "bic", "theta"),
       postprocess.tex = table_edit, 
       notes = "Offset = ln(person-years)", 
       title = paste0("Estimated coefficients of ", 
                     str_to_upper(ses_index), 
                     " quintile 5 by week from negative binomial regression model of ",
                     scenario_text, ".",
                     " Controls: population aged 50+, gini, Bolsonaro vote share."))