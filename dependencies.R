
required_packages <- c(
  # Basics
  "tidyverse",
  "tools",
  "lubridate",
  "DT",
  # Shiny
  "shiny",
  "shinydashboard",
  "shinyjs",
  "shinyauthr",
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

# Basics
library(tidyverse)
library(tools)
library(lubridate)
library(DT)
# Shiny
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyauthr)
library(ggplot2)
library(plotly)
library(highcharter)
library(RColorBrewer)
# Data
library(DBI)
library(RMySQL)
library(eurostat)