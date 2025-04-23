##### Script to extract deaths from model outputs #####
##### Author: Jordan Klein
# * = to supply via shell
##### Inputs
## Results: Intervention_project/Model_output.zip
# Model_output/*sensitivity_analysis*/*output_filename_interventions*.csv
##### Outputs
## Results: Intervention_project/
# Model_deaths/*sensitivity_analysis*/*output_filename_interventions*.csv

#### 1) Setup ####
rm(list = ls())

#### Load packages (install if necessary)
required_packages <- c("this.path", "tidyverse", "data.table", "lubridate")
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
intervention_scenarios <- str_match_all(model_args[1], "[:graph:].*")
sensitivity_analysis <- str_match_all(model_args[12], "[:graph:].*")
output_filename_interventions <- str_match_all(model_args[13], "[:graph:].*")

#### 2) Get deaths from model output ####
#### Load data
## Get zip file & filename
# *Supply project directories intervention_scenarios via shell*
# *Supply output_filename_interventions via shell*
if (intervention_scenarios == "yes") {
  proj_directory <- paste0(directory_stem, "Intervention_project/")
  filename <- paste0(output_filename_interventions, ".csv")
}
# zip file
model_output_zip <- paste0(proj_directory, "Model_output.zip")

## Get filepath based on sensitivity analysis setting
# *Supply sensitivity_analysis via shell*
if (sensitivity_analysis == "no_sensitivity") {
  filepath <- paste0("Model_output/", filename)
} else {
  filepath <- paste0("Model_output/", sensitivity_analysis, "/", filename)
}

## Read the data - Method using temporary file
temp <- tempfile()
conn <- unz(model_output_zip, filepath)
writeLines(readLines(conn), temp)
close(conn)
Model_output <- fread(temp)
file.remove(temp)

#### Get weekly deaths by municipality
Deaths <- filter(Model_output, compartment == "D") %>%
  left_join(., data.table(day = seq(1, 182, 1),
                          week = c(sapply(seq(1, 182 / 7, 1), function(x) {
                                                                           rep(x, 7)})))) %>%
  group_by(muni_code, week) %>%
  filter(day == max(day)) %>% # Get last day of each week (since deaths in model are cumulative)
  ungroup() %>%
  dplyr::select(muni_code, week, deaths_cum = population) %>% # Cumulative deaths @ end of every week
  mutate(deaths_cum_round = round(deaths_cum)) %>% # Round deaths to a whole number
  group_by(muni_code) %>%
  mutate(deaths = deaths_cum_round - lag(deaths_cum_round, default = 0)) %>%  # Get (new) weekly deaths by municipality
  select(muni_code, week, deaths)

#### 3) Export data ####
## Write output to both project accordingly
# *Supply intervention_scenarios via shell*
if (intervention_scenarios == "yes") {
  # *Supply sensitivity_analysis and output_filename_interventions via shell*
  if (sensitivity_analysis == "no_sensitivity") {
    if (dir.exists(paste0(directory_stem, "Intervention_project/Model_deaths"))) {
      paste0(directory_stem, "Intervention_project/Model_deaths/", output_filename_interventions, ".csv") %>%
        fwrite(Deaths, .)
    } else {
      dir.create(paste0(directory_stem, "Intervention_project/Model_deaths"))
      paste0(directory_stem, "Intervention_project/Model_deaths/", output_filename_interventions, ".csv") %>%
        fwrite(Deaths, .)
    }
  } else {
    if (dir.exists(paste0(directory_stem, "Intervention_project/Model_deaths/", sensitivity_analysis))) {
      paste0(directory_stem, "Intervention_project/Model_deaths/", sensitivity_analysis, "/", 
             output_filename_interventions, ".csv") %>%
        fwrite(Deaths, .)
    } else {
      dir.create(paste0(directory_stem, "Intervention_project/Model_deaths/", sensitivity_analysis))
      paste0(directory_stem, "Intervention_project/Model_deaths/", sensitivity_analysis, "/", 
             output_filename_interventions, ".csv") %>%
        fwrite(Deaths, .)
    }
  }
}