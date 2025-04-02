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
dirs$samples          <- paste0(dirs$seas_imm_server, "/outputs/modres_samples")

# To local
dirs$figures <- "figures"

# For pomp
dirs$pomp <- ""

# List of pathogen names
pat_tidy_names <- data.frame(
  pat = c(
    "rota", "hav", "hbv", "dipht", "tetan", 
    "pertuwpt", "multi_dtpw", 
    "pertuapt", "pertuafha", "pertuaprn", "multi_pertua", "multi_dtpa",
    "ipv1", "ipv2", "ipv3", "multi_ipv",
    "opv1", "opv2", "opv3", "multi_opv", 
    "hib", "mena", "menc", "meny", 
    "multi_men", "multi_hib-men", 
    "strep01", "strep03", "strep04", "strep05", "strep06a", 
    "strep06b", "strep07f", "strep09v", "strep14", "strep18c", 
    "strep19a", "strep19f", "strep23f", "multi_strep", 
    "measl", "mumps", "rubel", "multi_mmr", "vzv", "multi_mmr-vzv"),
  pat_tidy = c(
    "Rotavirus", "Hepatitis A virus", "Hepatitis B virus", "Diphteria", "Tetanus", 
    "Pertussis (W) PT", "DTP (W) multi",
    "Pertussis (A) PT", "Pertussis (A) FHA", "Pertussis (A) PRN", "Pertussis (A) multi", "DTP (A) multi", 
    "Polio (I) 1", "Polio (I) 2", "Polio (I) 3", "Polio (I) multi", 
    "Polio (O) 1", "Polio (O) 2", "Polio (O) 3", "Polio (O) multi", 
    "H. influenzae Type B", "N. meningitidis A", "N. meningitidis C", "N. meningitidis Y", 
    "N. meningitidis multi", "Men-Hib", 
    "S. pneumoniae 01", "S. pneumoniae 03", "S. pneumoniae 04", "S. pneumoniae 05", "S. pneumoniae 06a", 
    "S. pneumoniae 06b", "S. pneumoniae 07f", "S. pneumoniae 09v", "S. pneumoniae 14", "S. pneumoniae 18c", 
    "S. pneumoniae 19a", "S. pneumoniae 19f", "S. pneumoniae 23f", "S. pneumoniae multi",
    "Measles", "Mumps", "Rubella", "MMR multi", "Varicella", "MMRV multi")) %>%
  mutate(pat_tidy = factor(pat_tidy, levels = pat_tidy))

####################################################################################################
# END
####################################################################################################