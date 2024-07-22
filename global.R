# Core Shiny functionality
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Data manipulation and analysis
library(dplyr)
library(tidyverse)
library(tidymodels)
library(broom)
library(magrittr)

# Visualization
library(viridis)
library(UpSetR)
library(shapviz)
library(leaflet)
library(survminer)
library(apyramid)

# Geospatial data
# library(sf)
# library(rgeos)
# library(rnaturalearth)
# library(rnaturalearthdata)

# Statistical and survival analysis
library(boot)
library(survival)
library(fitdistrplus)
library(binom)

# Time series and forecasting
library(forecast)
library(TTR)

# Date and time manipulation
library(lubridate)

# Reporting and documentation
library(knitr)
library(kableExtra)
library(rmarkdown)
library(tinytex)

# SHAP explanations
library(kernelshap)

# Utility
library(glue)


# Environment Variables

reprocess.data <- FALSE
generate.reports <- FALSE

today = "2024-07-10" #reupdated
dataset.filename = "PhilippinesCCPCORE_DATA_2024-06-07_0617.csv" #reupdated

code.path <- 'D:/ISARIC_COVID19_Report_Dashboard/'
data.path <- 'D:/ISARIC_COVID19_Report_Dashboard/'
row.data.file <- 'D:/ISARIC_COVID19_Report_Dashboard/'

if(!dir.exists(code.path)){
  code.path <- ''
  data.path <- ''
  row.data.file <- ''
}

# Source files

if(!(reprocess.data) & file.exists(glue("{code.path}patient_data_{today}.rda"))){
  load(glue("{code.path}patient_data_{today}.rda"))
} else {
  source(glue("{code.path}process_data.R"))
}

if(!(reprocess.data) & file.exists(glue("{code.path}backup_data_{today}.rda"))){
  load(glue("{code.path}backup_data_{today}.rda"))
} else {
  source(glue("{code.path}backup.R"))
}

if(!(reprocess.data) & file.exists(glue("{code.path}time_series_data_{today}.rda"))){
  load(glue("{code.path}time_series_data_{today}.rda"))
} else {
  source(glue("{code.path}time_series.R"))
}

if(!(reprocess.data) & file.exists(glue("{code.path}forecast_data_{today}.rda"))){
  load(glue("{code.path}forecast_data_{today}.rda"))
} else {
  source(glue("{code.path}forecasts.R"))
}

if(!(reprocess.data) & file.exists(glue("{code.path}ml_data_{today}.rda"))){
  load(glue("{code.path}ml_data_{today}.rda"))
} else {
  source(glue("{code.path}mortality_los.R"))
}

source(glue("{code.path}plot_func.R"))

if(generate.reports){
  rmarkdown::render(glue("{code.path}COV-report.Rmd"),pdf_document(latex_engine="xelatex"))
  rmarkdown::render(glue("{code.path}markdown/summary.Rmd"))
  rmarkdown::render(glue("{code.path}markdown/summarytables.Rmd"))
}
