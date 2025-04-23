#### Script to produce maps of results (monthly mortality by municipality) ####
##### Author: Jordan Klein
# * = to supply via shell
##### Inputs
## Geospatial: Municipalities.geojson
## Results: [*Intervention_project*OR*Mobility_data_project*]/Model_output.zip
# Model_output/*output_filename_[interventionsORmobility_data]*.csv

# Not doing for sensitivity analyses #

#### 1) Setup ####
rm(list = ls())

#### Load packages (install if necessary)
required_packages <- c("this.path", "tidyverse", "data.table", "lubridate", "sf", "svglite", "scales", 
                       "ggallin", "patchwork", "Cairo")   
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
model_args <- commandArgs(trailingOnly = T)
intervention_scenarios <- str_match_all(model_args[1], "[:graph:].*")
mobility_data_sources <- str_match_all(model_args[2], "[:graph:].*")
output_filename_interventions <- str_match_all(model_args[13], "[:graph:].*")
output_filename_mobility_data <- str_match_all(model_args[14], "[:graph:].*")

#### 2) Load data ####
#### Geospatial
geo <- st_read("Data/Geospatial/Municipalities.geojson")

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
# file path
filepath <- paste0("Model_output/", filename)

## Read the data using temporary file
temp <- tempfile()
conn <- unz(model_output_zip, filepath)
writeLines(readLines(conn), temp)
close(conn)
Results <- fread(temp)
file.remove(temp)

#### 3) Data wrangling ####
#### Get original population data from model output
Population <- filter(Results, day == 1) %>%
  group_by(muni_code) %>%
  summarise(population = sum(population))

#### Create dataset for plotting
Results_map <- Results %>%
  filter(grepl("D", compartment)) %>%
  mutate(month = ceiling(day / 30)) %>%
  mutate(month = if_else(month == 7, 6, month)) %>%
  group_by(muni_code, month) %>%
  filter(day == max(day)) %>%
  ungroup() %>%
  select(muni_code, month, deaths = population) %>%
  mutate(month = paste("Month", month),
         deaths = round(deaths)) %>%
  left_join(Population, by = "muni_code") %>%
  mutate(mortality = deaths / population * 100000) %>%
  mutate(muni_code = as.character(muni_code)) %>%
  left_join(geo %>%
              mutate(muni_code = substr(code_muni, 1, 6)),
            by = "muni_code") %>%
  select(muni_code, month, deaths, population, mortality, name_muni, name_state, geometry)

#### 4) Plotting ####
## Figures directory stem
figures_dir <- "Publication/Figures/Simulation_maps/"

# Define the months to loop through
months <- c("Month 1", "Month 2", "Month 3", "Month 4", "Month 5", "Month 6")
# Loop through each month
for (i in months) {
  # Create a plot for the current month
  plot <- filter(Results_map, month == i) %>%
    mutate(mortality = if_else(mortality > 1000, 1000, mortality)) %>%
    ggplot(aes(fill = mortality, geometry = geometry)) +
    geom_sf(size = .001) +
    facet_wrap(~month, ncol = 1) +
    theme_dark() +
    scale_fill_distiller(palette = "Reds", na.value = "black",
                         limits = c(0, 1000), trans = "pseudo_log", direction = 1,
                         breaks = c(0, 10, 100, 1000)) +
    labs(fill = "Mortality (per 100,000)") +
    scale_color_manual(values = "transparent")
  ## Save plots to both project accordingly
  # *Supply intervention_scenarios via shell*
  if (intervention_scenarios == "yes") {
    str_extract(i, "\\d+") %>%
      paste0(figures_dir, "Intervention_project/", output_filename_interventions, "_mortality_map_month", ., ".pdf") %>%
      ggsave(plot, width = 10, height = 10, dpi = 300)
  }
  # *Supply mobility_data_sources via shell*
  if (mobility_data_sources == "yes") {
    str_extract(i, "\\d+") %>%
      paste0(figures_dir, "Mobility_data_project/", output_filename_mobility_data, "_mortality_map_month", ., ".pdf") %>%
      ggsave(plot, width = 10, height = 10, dpi = 300)
  }
  # Clear the plot from memory
  gc()
}