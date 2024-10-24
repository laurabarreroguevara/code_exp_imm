####################################################################################################
# Load packages, themes, and directories
####################################################################################################
library(tidyverse)
library(readxl)
library(glue)
library(lubridate)
library(patchwork)
library(brms)
library(pomp)
library(bayesplot)
library(naniar)
library(colorspace)
library(tictoc)
theme_set(theme_classic())

dirs <- list()
# To Kramer's server "saved zipped" data 
dirs$server_save_kramer =  "/Volumes/public-dom/kramer/gsk_data"
# To Barrero Guevaras's server "saved zipped" data 
dirs$server_save_bg =  "/Volumes/public-dom/barrero-guevara/gsk_data_save"
# To Barrero Guevaras's server "working unzipped" data 
dirs$server_bg =  "/Volumes/public-dom/barrero-guevara/gsk_data"
dirs$server_vacc_con =  "/Volumes/public-dom/barrero-guevara/vacc_con_data"
# To local data 
dirs$raw <- "data/raw"
dirs$processed <- "data/processed"
dirs$outputs =  "outputs"

####################################################################################################
# END
####################################################################################################