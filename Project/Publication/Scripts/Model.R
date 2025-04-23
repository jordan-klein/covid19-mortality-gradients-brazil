##### Script to run simulation #####
##### Author: Jordan Klein
# * = to supply via shell
#### Project: *intervention_scenarios*
##### Inputs
#### Files
### Fixed
# Population: population_density.csv
# Parameters: CFR.csv, Household_size.csv
### Mobility network
# *network_data_source* (InLoco = Mobility/network_processed.zip)
### Interventions
## Social distancing
# *social_distancing_intervention* (no, actual, equality)
# *social_distancing_metric* (abs_delta)
# *social_distancing_data_source* (Facebook)
# abs_delta: Mobility/mvt_range_maps/Mobility_change_*social_distancing_intervention*.csv
## Vaccination
# Scenarios/Vax_rate_*vaccination*.csv
### Sensitivity analyses
# *IFR_sensitivity* (.5, 1, 2)
# *NPI_sensitivity* (.5, 1, 2)
#### Parameters
### Fixed
# Latent period = 2.9 days (Pascual paper)
# Infectious period = 7 days (Pascual paper)
# Vaccine efficacy against infection (symptomatic+asymptomatic) = .89 (ACIP GRADE report)
# Vaccine protection against death (in infected) = .509
# HH_size (2010 Census) = 3.286
### Variable (sensitivity analyses)
# *R0* (de Souza paper) = 3.1 [2.4, 5.5]
# *SAR* (household SAR, Madewell & Thompson papers) = .189 [.14, .248]
##### Output
## Results: Model_output/Intervention_project/
# *sensitivity_analysis*/*output_filename_interventions*.csv
# caution changed file structure since running this script #

#### 1) Setup ####
rm(list = ls())

#### Load packages (install if necessary)
required_packages <- c("this.path", "tidyverse", "data.table", "deSolve")
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
intervention_scenarios <- str_match_all(model_args[1], "[:graph:].*")
network_data_source <- str_match_all(model_args[3], "[:graph:].*")
social_distancing_intervention <- str_match_all(model_args[4], "[:graph:].*")
social_distancing_metric <- str_match_all(model_args[5], "[:graph:].*")
social_distancing_data_source <- str_match_all(model_args[6], "[:graph:].*")
vaccination <- str_match_all(model_args[7], "[:graph:].*")
IFR_sensitivity <- as.numeric(str_match_all(model_args[8], "[:graph:].*"))
NPI_sensitivity <- as.numeric(str_match_all(model_args[9], "[:graph:].*"))
SAR <- as.numeric(str_match_all(model_args[10], "[:graph:].*"))
R0 <- as.numeric(str_match_all(model_args[11], "[:graph:].*"))
sensitivity_analysis <- str_match_all(model_args[12], "[:graph:].*")
output_filename_interventions <- str_match_all(model_args[13], "[:graph:].*")

#### 2) Load & clean data ####
#### Pre-existing conditions parameters
### CFR
CFR <- read_csv("Data/Parameters/CFR.csv") %>%
  .[order(.$muni_code), ]
# Adjust for sentitivity analysis *supply IFR_sensitivity via shell*
CFR <- mutate(CFR, CFR = CFR * IFR_sensitivity) %>% 
  mutate(CFR = if_else(CFR > 1, 1, CFR))

### Household size
Household_size <- read_csv("Data/Parameters/Household_size.csv") %>%
  .[order(.$muni_code), ]

#### Population
Pop <- fread("Data/Population/population_density.csv") %>%
  filter(year == 2020) %>%
  select(muni_code, population) %>%
  # Only keep municipalities I have params for
  .[.$muni_code %in% CFR$muni_code, ]

#### Mobility network
# *supply network_data_source via shell*
if (network_data_source == "InLoco") {
  ## InLoco data
  # Set up temporary directory
  temp_dir <- tempdir()
  # Get files to extract
  files_to_extract <- unzip("Data/Mobility/network_processed.zip", list = TRUE) %>%
    select(Name) %>% 
    pull(Name)
  # Unzip files
  lapply(files_to_extract, function(file) {
    unzip("Data/Mobility/network_processed.zip", files = file, exdir = temp_dir)
  })
  # Load data (this is a list of matrices)
  Mob_network <- lapply(files_to_extract, function(file) {
    readRDS(file.path(temp_dir, file))
  })
  names(Mob_network) <- str_extract(files_to_extract, "[0-9-]+(?=\\.rds)")
}
gc()  # Clean up memory

#### Interventions
### Social distancing
# *supply social_distancing_intervention via shell*
if (social_distancing_intervention != "no") {
  # *supply social_distancing_metric via shell*
  if (social_distancing_metric == "abs_delta") {
    # Absolute change in mobility (Facebook movement range maps) *supply social_distancing_intervention via shell*
    Delta_M <- paste0("Data/Mobility/mvt_range_maps/Mobility_change_", social_distancing_intervention, ".csv") %>%
      fread() %>%
      # Adjust for sensitivity analysis *supply NPI_sensitivity via shell*
      mutate(delta_M = delta_M * NPI_sensitivity) %>%
      mutate(delta_M = if_else(delta_M < -1, -1, delta_M)) %>%
      # Split into list by week
      split(.$week) %>%
      lapply(function(x) {
        x[order(x$muni_code), c("muni_code", "delta_M")]
      })
  }
}

### Vaccination
# *supply vaccination via shell*
if (vaccination != "no") {
  Vax <- fread(paste0("Data/Vaccination/Scenarios/Vax_rate_", vaccination, ".csv")) %>%
    # Split into list by day
    split(.$date) %>%
    lapply(function(x) {
      x[order(x$muni_code), c("muni_code", "vax_rate")]
    })
}

#### 3) Set compartments (population initial conditions) & parameter values ####
#### Set compartments (start with 10 exposed in Sao Paulo, 3550308/355030)
Comparts <- mutate(Pop,
                   S = if_else(muni_code == 355030, population - 10, population), V_S = 0,
                   E = if_else(muni_code == 355030, 10, 0), V_E = 0,
                   I = 0, V_I = 0, R = 0, D = 0) %>%
  select(-muni_code, -population) %>%
  c() %>%
  unlist()

#### Set fixed parameter values
# Known parameters
delta_E <- 2.9^-1 # Latent period = 2.9 days (Pascual paper)
gamma <- 7^-1 # Infectious period = 7 days (Pascual paper)
v_e <- .89 # Vaccine efficacy against infection (symptomatic+asymptomatic) = .89 (ACIP GRADE report)
v_p <- .509 # Derived from v_e & weighted mean of vax protection against death = .946 (dos Santos paper)
SAR <- SAR # *SAR* (Madewell & Thompson papers)
R0 <- R0 # *R0* (de Souza paper)
HH_size_bar <- 3.286 # mean HH_size (2010 Census)
# Derived parameters
tau_HH <- SAR * gamma # Household SAR = P(inf|contact) for the whole infectious period, tau_HH = P(inf|contact) per day
kappa_M <- gamma * R0 - tau_HH * (HH_size_bar - 1)

#### List of list of parameter values, each element of list is a date
# Create blank object to fill using for loop
param_values <- list()
# For each date (182 dates)
for (i in 1:182) {
  params <- list(
    delta_E = delta_E,
    gamma = gamma,
    v_e = v_e,
    v_p = v_p,
    tau_HH = tau_HH,
    kappa_M = kappa_M,
    d = CFR$CFR, # IFR
    HH_size = Household_size$hh_size, # Household size parameter
    # Mobility network
    omega = if (network_data_source == "InLoco") {
      Mob_network[[i]] # Dynamic network
    },
    # NPIs (social distancing)
    NPI = if (social_distancing_intervention == "no") {
      1 # No intervention
    } else {
      if (social_distancing_metric == "abs_delta") {
        1 + rep(Delta_M, each = 7)[[i]]$delta_M # Absolute change in mobility
      }
    },
    # Vaccination
    eta = if (vaccination == "no") {
      0 # No vaccination
    } else {
      Vax[[i]]$vax_rate # Vaccination rate
    }
  )
  param_values <- c(param_values, list(params))
}
rm(params)
gc()

#### 4) Write function to compute model ####
# Model function
model <- function(t, y, parms) {
  with(c(split(y, rep(c("S", "V_S", "E", "V_E", "I", "V_I", "R", "D"), each = 5565, length.out = length(y))) %>%
           .[c("S", "V_S", "E", "V_E", "I", "V_I", "R", "D")],
         parms[[t + 1]]), {
         # Population size
         N <- S + V_S + E + V_E + I + V_I + R
         # Force of infection
         lambda_HH <- tau_HH * (HH_size - 1) * (I + V_I) / N
         lambda_M <- kappa_M * NPI * omega %*% c((I + V_I) / N)
         lambda <- lambda_HH + lambda_M
         # Differential equations
         dSdt <- -(lambda + eta) * S
         dV_Sdt <- eta * S - lambda * (1 - v_e) * V_S
         dEdt <- lambda * S - delta_E * E
         dV_Edt <- lambda * (1 - v_e) * V_S - delta_E * V_E
         dIdt <- delta_E * E - gamma * I
         dV_Idt <- delta_E * V_E - gamma * V_I
         dRdt <- (1 - d) * gamma * I + (1 - d * (1 - v_p)) * gamma * V_I
         dDdt <- d * gamma * I + d * (1 - v_p) * gamma * V_I
         list(c(dSdt, dV_Sdt, dEdt, dV_Edt, dIdt, dV_Idt, dRdt, dDdt))
       })
}

#### 5) Compute model ####
#### Set timepoints (exactly 26 weeks/182 days)
time.out <- seq(0, 26 * 7 - 1, 1)
gc()

#### Solve the model
model_output <- data.table(lsoda(y = Comparts, times = time.out, func = model, parms = param_values))

#### 6) Export model results ####
#### Get into long format
Model_format <- pivot_longer(model_output, cols = -time, names_to = "compartment", values_to = "population") %>%
  mutate(muni_no = as.numeric(str_remove_all(compartment, "[[:alpha:]]|[[:punct:]]"))) %>%
  mutate(compartment = str_remove_all(compartment, "[[:digit:]]")) %>%
  full_join(data.table(muni_code = Pop$muni_code, muni_no = seq_len(nrow(Pop)))) %>%
  mutate(day = time + 1) %>%
  select(muni_code, day, compartment, population) %>%
  data.table()

#### Export
directory_stem <- "Publication/Data/Results/"
# *supply intervention_scenarios via shell* 
if (intervention_scenarios == "yes") {
  proj_directory <- paste0(directory_stem, "Intervention_project/")
  # *supply sensitivity_analysis, output_filename_interventions via shell*
  if (sensitivity_analysis == "no_sensitivity") {
    paste0(proj_directory, output_filename_interventions, ".csv") %>%
      fwrite(Model_format, .)
  } else {
    if (dir.exists(paste0(proj_directory, sensitivity_analysis))) {
      paste0(proj_directory, sensitivity_analysis, "/", output_filename_interventions, ".csv") %>%
        fwrite(Model_format, .)
    } else {
      dir.create(paste0(proj_directory, sensitivity_analysis))
      paste0(proj_directory, sensitivity_analysis, "/", output_filename_interventions, ".csv") %>%
        fwrite(Model_format, .)
    }
  }
}