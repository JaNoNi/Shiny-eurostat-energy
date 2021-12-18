# INSTALL DEPENDENCIES ----------------------------------------------------

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)