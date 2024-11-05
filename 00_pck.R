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
dirs$server_save_kramer <- "/Volumes/public-dom/kramer/gsk_data"
# To Barrero Guevaras's server "saved zipped" data 
dirs$server_save_bg <- "/Volumes/public-dom/barrero-guevara/gsk_data_save"
# To Barrero Guevaras's server "working unzipped" data 
dirs$server_bg <- "/Volumes/public-dom/barrero-guevara/gsk_data"
dirs$server_vacc_con <- "/Volumes/public-dom/barrero-guevara/vacc_con_data"
# To local data 
dirs$raw <- "data/raw"
dirs$processed <- "data/processed"
dirs$outputs <- "outputs"
dirs$rhats <- "outputs/modres_rhats"
dirs$waic <- "outputs/modres_waic"
dirs$coef <- "outputs/modres_coef"
dirs$ceffects <- "outputs/modres_ceffects"
dirs$figures <- "figures"
dirs$modfit <- "outputs/modfit"

pat_tidy_names <- data.frame(
  pat = c(
    "rota", "hav", "hbv", "dipht", "dtpa", 
    "dtpw", "tetan", "pertu", "pertufha", "pertuprn", 
    "multi_pertu", "multi_dtp", 
    "multi_dtpa", "multi_dtpw", "polio", "multi_polio", 
    "ipv", "ipv2", "ipv3",
    "multi_ipv", "opv", "multi_opv", 
    "hib", "men", "multi_men", "multi_hib-men", 
    "strep", "multi_strep", "measl", "mumps", "rubel", "multi_mmr", "vzv", "multi_mmr-vzv"),
  pat_tidy = c(
    "Rotavirus", "Hepatitis A virus", "Hepatitis B virus", "Diphteria", "Diphteria (Acellular)", 
    "Diphteria (Whole-cell)", "Tetanus", "Pertussis PT", "Pertussis FHA", "Pertussis PRN", 
    "Pertussis multi", "DTP multi", 
    "DTP (Acellular) multi", "DTP (Whole-cell) multi", "Polio", "Polio multi", 
    "Polio 1 (inactivated)", "Polio 2 (inactivated)", "Polio 3 (inactivated)", 
    "Polio (inactivated) multi", "Polio (oral)", "Polio (oral) multi", 
    "H. influenzae Type B", "N. meningitidis (c)", "N. meningitidis (c and a) multi", "Men-Hib", 
    "S. pneumoniae", "S. pneumoniae multi","Measles", "Mumps", "Rubella", "MMR multi", "Varicella",
    "MMRV multi")) %>%
  mutate(pat_tidy = factor(pat_tidy, levels = pat_tidy))
####################################################################################################
# END
####################################################################################################