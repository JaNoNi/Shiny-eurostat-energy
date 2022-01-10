
required_packages <- c(
  # Basics
  "tidyverse",
  "tools",
  "lubridate",
  "DT",
  # Shiny
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "shinyjs",
  "shinyauthr",
  "shinycssloaders",
  # Plots
  "ggplot2",
  "plotly",
  "highcharter",
  "RColorBrewer",
  # Data
  "DBI",
  "RMySQL",
  "eurostat"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)