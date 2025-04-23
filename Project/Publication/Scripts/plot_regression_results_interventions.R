##### Script to produce plots of SES quintile regression results (for intervention project) #####
##### Author: Jordan Klein
# * = to supply via shell
##### Inputs
## Results: Intervention_project/Regression_results.zip
# Regression_results/*subfolder*/[all files]

#### 1) SETUP ####
# clear workspace
rm(list = ls())

# packages
required_packages <- c(
  "this.path", "stringr", "tidyverse", "data.table", "lubridate", 
  "fixest", "broom", "marginaleffects", "ggallin", "patchwork", "Cairo"
)
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# base directory: find where this script lives, then climb up to Project
script_dir  <- this.dir()
project_dir <- str_extract(script_dir, "^.*Project")
setwd(project_dir)

#### 2) ARGS & PATHS ####
model_args <- commandArgs(trailingOnly = TRUE)
subfolder <- str_match_all(model_args[1], "[:graph:].*") %>%
  as.character()

zipfile    <- file.path(
  project_dir,
  "Publication","Data","Results",
  "Intervention_project","Regression_results.zip"
)
data_path  <- if (subfolder=="none") "Regression_results/" else paste0("Regression_results/", subfolder, "/")
fig_dir    <- if (subfolder=="none") {
  file.path(project_dir, "Publication","Figures","Regression_results","Intervention_project")
} else {
  dir <- file.path(
    project_dir,
    "Publication","Figures","Regression_results","Intervention_project", subfolder
  )
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

#### 3) IMPORT & BIND ALL CSVs ####
all_files <- unzip(zipfile, list = TRUE) %>%
  as_tibble() %>%
  filter(str_detect(Name, fixed(data_path)), Length > 0) %>%
  pull(Name)

Data <- map_dfr(all_files, ~{
  tmp <- tempfile()
  unzip(zipfile, files = .x, exdir = dirname(tmp))
  df  <- fread(file.path(dirname(tmp), .x))
  df$Model <- str_remove(.x, data_path) %>% str_remove("\\.csv$")
  file.remove(file.path(dirname(tmp), .x))
  df
})

#### 4) CLEAN & PREPARE ####
Data <- Data %>%
  mutate(
    ses_index = if_else(str_detect(Model, "idhm"), "MHDI", "SVI"),
    Scenario  = str_remove(Model, "_idhm|_ivs"),
    RR_lb = if_else(between(RR_lb, 0, 20), RR_lb, NA_real_),
    RR_ub = if_else(between(RR_ub, 0, 20), RR_ub, NA_real_)
  )

base_theme <- theme_bw() +
  theme(
    plot.tag   = element_text(face = "bold")
  )
# Color palletes
pal2 <- c("#CC79A7","#56B4E9","#F0E442","#009E73","#E69F00","#0072B2")
pal1 <- c(pal2[1:4], "black")

#### 5) PLOTTING FUNCTION ####
plot_ses <- function(df, ses, scenarios, labels, tag, pal) {
  df %>%
    filter(ses_index==ses, Scenario %in% scenarios) %>%
    mutate(Scenario=factor(Scenario, levels=scenarios, labels=labels)) %>%
    ggplot(aes(week, RR, ymin=RR_lb, ymax=RR_ub)) +
      geom_line(aes(color=Scenario, linetype=Scenario), linewidth = 1.25) +
      geom_ribbon(aes(fill=Scenario), alpha=0.5) +
      scale_x_continuous("Weeks", breaks=seq(0,26,2)) +
      scale_y_continuous("Relative Risk", trans=pseudolog10_trans, breaks=c(0,0.4,1,4,10)) +
      geom_hline(yintercept=1, linetype="dashed", alpha=0.5, linewidth = .5) +
      scale_color_manual(values=pal) +
      scale_fill_manual(values=pal) +
      coord_cartesian(ylim=c(0,20)) +
      base_theme +
      labs(tag=tag)
}

#### 6) CREATE & SAVE STACKED FIGURES ####

# Set 1
s1_scen <- c("Baseline","NPI","Vax","NPI_Vax","Empirical")
s1_lab  <- c("Baseline","NPIs","Vaccination","NPIs+Vaccination","Empirical")
p1_svi  <- plot_ses(Data, "SVI",  s1_scen, s1_lab, "SVI", pal1)
p1_mhdi <- plot_ses(Data, "MHDI", s1_scen, s1_lab, "MHDI", pal1)
p1      <- p1_svi / p1_mhdi + plot_layout(ncol=1)
# Save plot
ggsave(
  filename = file.path(fig_dir, "Scenarios1.pdf"),
  plot    = p1,
  dpi     = 300, 
  width = 12,
  height = 16,
)

# Set 2
s2_scen <- c("Baseline","NPI_eq","Vax_eq","Vax_pr","NPI_eq_Vax_eq","NPI_eq_Vax_pr")
s2_lab  <- c(
  "Baseline","NPIs-equal","Vaccination-equal","Vaccination-equitable",
  "NPIs+Vaccination-equal","NPIs-equal+Vaccination-equitable"
)
p2_svi  <- plot_ses(Data, "SVI",  s2_scen, s2_lab, "SVI", pal2)
p2_mhdi <- plot_ses(Data, "MHDI", s2_scen, s2_lab, "MHDI", pal2)
p2      <- p2_svi / p2_mhdi + plot_layout(ncol=1)
# Save plot
ggsave(
  filename = file.path(fig_dir, "Scenarios2.pdf"),
  plot    = p2,
  dpi     = 300, 
  width = 12,
  height = 16,
)
