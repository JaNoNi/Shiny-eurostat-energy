
required_packages <- c(
  "checkpoint"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(tools)
library(DT)