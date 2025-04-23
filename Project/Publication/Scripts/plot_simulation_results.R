##### Script to extract deaths from model outputs #####
##### Author: Jordan Klein
# * = to supply via shell
##### Inputs
## Results: [*Intervention_project*OR*Mobility_data_project*]/Model_output.zip
# Model_output/*sensitivity_analysis*/*output_filename_[interventionsORmobility_data]*.csv

#### 1) Setup ####
rm(list = ls())

#### Load packages (install if necessary)
required_packages <- c("this.path", "tidyverse", "data.table", "lubridate", "svglite", "scales", "purrr")
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
mobility_data_sources <- str_match_all(model_args[2], "[:graph:].*")
sensitivity_analysis <- str_match_all(model_args[12], "[:graph:].*")
output_filename_interventions <- str_match_all(model_args[13], "[:graph:].*")
output_filename_mobility_data <- str_match_all(model_args[14], "[:graph:].*")

#### 2) Load data ####
#### Results
## Get zip file & filename
# *Supply project directories (intervention_scenarios OR mobility_data_sources) via shell*
# *Supply output_filename_interventions/output_filename_mobility_data via shell*
if (intervention_scenarios == "yes") {
  proj_directory <- paste0(directory_stem, "Intervention_project/")
  filename <- paste0(output_filename_interventions, ".csv")
} else {
  proj_directory <- paste0(directory_stem, "Mobility_data_project/")
  filename <- paste0(output_filename_mobility_data, ".csv")
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

## Read the data using temporary file
temp <- tempfile()
conn <- unz(model_output_zip, filepath)
writeLines(readLines(conn), temp)
close(conn)
Results <- fread(temp)
file.remove(temp)

#### 3) Plot aggregate compartments ####
if (str_detect(filename, "Vax")) {
  ## Create dataset for plotting
  Results_agg <- Results %>% 
    group_by(compartment, day) %>% 
    summarise(population = sum(population)) %>% 
    ungroup() %>% 
    mutate(vax_status = ifelse(grepl("V_", compartment), "Vaccinated", ""), 
           compartment = factor(compartment, levels = c("S", "V_S",  "E", "V_E", "I", "V_I", "R", "D"), 
                                labels = c("Susceptible", "Susceptible (vaccinated)", 
                                           "Exposed", "Exposed (vaccinated)",
                                           "Infected", "Infected (vaccinated)",
                                           "Recovered", "Deceased"))) %>% 
    mutate(state = factor(gsub(" .*$", "", compartment), 
                          levels = c("Susceptible", "Exposed", "Infected", "Recovered", "Deceased")))
  
  ## Plot
  Results_agg %>% 
    ggplot(aes(x = day, y = population, color = state, linetype = vax_status)) +
    geom_line() + 
    theme_bw() + 
    # Fix axes
    scale_x_continuous(breaks = seq(0, 182, 14), labels = function(x) x / 7) + 
    # Remove scientific notation from y axis
    scale_y_continuous(labels = function(x) x / 1000000) +
    # Fix labels
    labs(x = "Weeks", y = "Population (millions)", color = "Compartment", linetype = "") + 
    # Fix legend- only display vaccinated in vax status
    guides(linetype = guide_legend(override.aes = list(color = "black", linetype = c(NA, 2))))
} else {
  ## Create dataset for plotting
  Results_agg <- filter(Results, !grepl("V_", compartment)) %>% 
    group_by(compartment, day) %>%
    summarise(population = sum(population)) %>%
    ungroup() %>%
    mutate(compartment = factor(compartment, levels = c("S", "E", "I", "R", "D"),
                                labels = c("Susceptible", "Exposed", "Infected", "Recovered", "Deceased")))
  
  ## Plot
  Results_agg %>%
    ggplot(aes(x = day, y = population, color = compartment)) +
    geom_line() +
    theme_bw() +
    # Fix axes
    scale_x_continuous(breaks = seq(0, 182, 14), labels = function(x) x / 7) +
    # Remove scientific notation from y axis
    scale_y_continuous(labels = function(x) x / 1000000) +
    # Fix labels
    labs(x = "Weeks", y = "Population (millions)", color = "Compartment")
}

#### 4) Save plot ####
## Figures directory stem
figures_dir <- "Publication/Figures/Simulation_results/"

## Write output to both project accordingly
# *Supply intervention_scenarios via shell*
if (intervention_scenarios == "yes") {
  # *Supply sensitivity_analysis and output_filename_interventions via shell*
  if (sensitivity_analysis == "no_sensitivity") {
    if (dir.exists(paste0(figures_dir, "Intervention_project"))) {
      paste0(figures_dir, "Intervention_project/", output_filename_interventions, ".svg") %>%
        ggsave(width = 6, height = 4, dpi = 300)
    } else {
      dir.create(paste0(figures_dir, "Intervention_project"))
      paste0(figures_dir, "Intervention_project/", output_filename_interventions, ".svg") %>%
        ggsave(width = 6, height = 4, dpi = 300)
    }
  } else {
    if (dir.exists(paste0(figures_dir, "Intervention_project/", sensitivity_analysis))) {
      paste0(figures_dir, "Intervention_project/", sensitivity_analysis, "/", output_filename_interventions, ".svg") %>%
        ggsave(width = 6, height = 4, dpi = 300)
    } else {
      dir.create(paste0(figures_dir, "Intervention_project/", sensitivity_analysis))
      paste0(figures_dir, "Intervention_project/", sensitivity_analysis, "/", output_filename_interventions, ".svg") %>%
        ggsave(width = 6, height = 4, dpi = 300)
    }
  }
}
# *Supply mobility_data_sources via shell*
if (mobility_data_sources == "yes") {
  # *Supply sensitivity_analysis and output_filename_mobility_data via shell*
  if (sensitivity_analysis == "no_sensitivity") {
    if (dir.exists(paste0(figures_dir, "Mobility_data_project"))) {
      paste0(figures_dir, "Mobility_data_project/", output_filename_mobility_data, ".svg") %>%
        ggsave(width = 6, height = 4, dpi = 300)
    } else {
      dir.create(paste0(figures_dir, "Mobility_data_project"))
      paste0(figures_dir, "Mobility_data_project/", output_filename_mobility_data, ".svg") %>%
        ggsave(width = 6, height = 4, dpi = 300)
    }
  } else {
    if (dir.exists(paste0(figures_dir, "Mobility_data_project/", sensitivity_analysis))) {
      paste0(figures_dir, "Mobility_data_project/", sensitivity_analysis, "/", 
             output_filename_mobility_data, ".svg") %>%
        ggsave(width = 6, height = 4, dpi = 300)
    } else {
      dir.create(paste0(figures_dir, "Mobility_data_project/", sensitivity_analysis))
      paste0(figures_dir, "Mobility_data_project/", sensitivity_analysis, "/", 
             output_filename_mobility_data, ".svg") %>%
        ggsave(width = 6, height = 4, dpi = 300)
    }
  }
}