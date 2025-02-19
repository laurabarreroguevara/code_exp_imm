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

# To Barrero Guevaras's server main 
dirs$seas_imm_server  <- "/Volumes/public-dom/barrero-guevara/_seas_imm_server"
# To Barrero Guevaras's server "saved zipped" data 
dirs$server_save_bg   <- paste0(dirs$seas_imm_server, "/gsk_raw/gsk_data_save")
# To Barrero Guevaras's server "working unzipped" data 
dirs$server_bg        <- paste0(dirs$seas_imm_server, "/gsk_raw/gsk_data")
dirs$server_vacc_con  <- paste0(dirs$seas_imm_server, "/gsk_raw/vacc_con_data")

# To data 
dirs$raw              <- paste0(dirs$seas_imm_server, "/data/raw")
dirs$processed        <- paste0(dirs$seas_imm_server, "/data/processed")

# To outputs
dirs$outputs          <- paste0(dirs$seas_imm_server, "/outputs")
dirs$modfit           <- paste0(dirs$seas_imm_server, "/outputs/modfit")
dirs$rhats            <- paste0(dirs$seas_imm_server, "/outputs/modres_rhats")
dirs$waic             <- paste0(dirs$seas_imm_server, "/outputs/modres_waic")
dirs$coef             <- paste0(dirs$seas_imm_server, "/outputs/modres_coef")
dirs$ceffects         <- paste0(dirs$seas_imm_server, "/outputs/modres_ceffects")
dirs$cespa            <- paste0(dirs$seas_imm_server, "/outputs/modres_cespa")
dirs$pred             <- paste0(dirs$seas_imm_server, "/outputs/modres_pred")

# To local
dirs$figures <- "figures"

# For pomp
dirs$pomp <- ""

# List of pathogen names
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