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
library(viridis)
library(tictoc)
library(ggh4x)
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
#dirs$outputs          <- paste0(dirs$seas_imm_server, "/outputs")
dirs$outputs           <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs")
#dirs$modfit           <- paste0(dirs$seas_imm_server, "/outputs/modfit")
dirs$modfit           <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/modfit")
#dirs$modres           <- paste0(dirs$seas_imm_server, "/outputs/modres")     
dirs$modres           <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/modres")    
#dirs$rhats            <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_rhats")
dirs$rhats            <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/modres/modres_rhats")
#dirs$waic             <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_waic")
dirs$waic             <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/modres/modres_waic")
#dirs$coef             <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_coef")
dirs$coef             <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/modres/modres_coef")
dirs$ceffects         <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_ceffects")
dirs$cespa            <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_cespa")
#dirs$pred             <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_pred")
dirs$pred             <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/modres/modres_pred")
#dirs$samples          <- paste0(dirs$seas_imm_server, "/outputs/modres/modres_samples")
dirs$samples          <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/modres/modres_samples")
dirs$simcosfit        <- paste0(dirs$seas_imm_server, "/outputs/simcosfit")
dirs$simcosres        <- paste0(dirs$seas_imm_server, "/outputs/simcosres")
dirs$simcos_rhats     <- paste0(dirs$seas_imm_server, "/outputs/simcosres/simcosres_rhats")
dirs$simcos_coef      <- paste0(dirs$seas_imm_server, "/outputs/simcosres/simcosres_coef")
dirs$simcos_samples   <- paste0(dirs$seas_imm_server, "/outputs/simcosres/simcosres_samples")
#dirs$simpredfit        <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/simpredfit")
dirs$simpredfit        <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/simpredfit")
#dirs$simpredres        <- paste0(dirs$seas_imm_server, "/outputs/simpredres")
dirs$simpredres        <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/simpredres")
#dirs$simpred_rhats     <- paste0(dirs$seas_imm_server, "/outputs/simpredres/simpredres_rhats")
dirs$simpred_rhats     <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/simpredres/simpredres_rhats")
#dirs$simpred_coef      <- paste0(dirs$seas_imm_server, "/outputs/simpredres/simpredres_coef")
dirs$simpred_coef      <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/simpredres/simpredres_coef")
#dirs$simpred_samples   <- paste0 (dirs$seas_imm_server, "/outputs/simpredres/simpredres_samples")
dirs$simpred_samples   <- paste0("/Users/u_barrero-guevara/_seas_imm_pappos/outputs/simpredres/simpredres_samples")
dirs$climfit           <- paste0(dirs$seas_imm_server, "/outputs/climfit")
dirs$climres        <- paste0(dirs$seas_imm_server, "/outputs/climres")
dirs$clim_rhats     <- paste0(dirs$seas_imm_server, "/outputs/climres/climres_rhats")
dirs$clim_coef      <- paste0(dirs$seas_imm_server, "/outputs/climres/climres_coef")
dirs$clim_samples   <- paste0(dirs$seas_imm_server, "/outputs/climres/climres_samples")

# To local
dirs$figures <- "figures"

# For pomp
dirs$pomp <- ""

# List of pathogen names
tidy_names_pat <- data.frame(
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
    "strep19a", "strep19f", "strep23f", 
    "multi_strep7", "multi_strep10", "multi_strep11", "multi_strep13",   
    "measl", "mumps", "rubel", "multi_mmr", "vzv", "multi_mmr-vzv"),
  pat_tidy = c(
    "Rotavirus", "HAV", "HBV", "Diphteria", "Tetanus", 
    "Pertu (W) PT", "DTP (W) multi",
    "Pertu (A) PT", "Pertu (A) FHA", "Pertu (A) PRN", "Pertu (A) multi", "DTP (A) multi", 
    "Polio (I) 1", "Polio (I) 2", "Polio (I) 3", "Polio (I) multi", 
    "Polio (O) 1", "Polio (O) 2", "Polio (O) 3", "Polio (O) multi", 
    "H. inf B", "N. men A", "N. men C", "N. men Y", 
    "N. men multi", "Men-Hib", 
    "S. pn 01", "S. pn 03", "S. pn 04", "S. pn 05", "S. pn 06a", 
    "S. pn 06b", "S. pn 07f", "S. pn 09v", "S. pn 14", "S. pn 18c", 
    "S. pn 19a", "S. pn 19f", "S. pn 23f", 
    "S. pn multi 7", "S. pn multi 10", "S. pn multi 11", "S. pn multi 13",
    "Measles", "Mumps", "Rubella", "MMR multi", "VZV", "MMRV multi")) %>%
  # pat_tidy = c(
  #   "Rotavirus", "Hepatitis A virus", "Hepatitis B virus", "Diphteria", "Tetanus", 
  #   "Pertussis (W) PT", "DTP (W) multi",
  #   "Pertussis (A) PT", "Pertussis (A) FHA", "Pertussis (A) PRN", "Pertussis (A) multi", "DTP (A) multi", 
  #   "Polio (I) 1", "Polio (I) 2", "Polio (I) 3", "Polio (I) multi", 
  #   "Polio (O) 1", "Polio (O) 2", "Polio (O) 3", "Polio (O) multi", 
  #   "H. influenzae B", "N. meningitidis A", "N. meningitidis C", "N. meningitidis Y", 
  #   "N. meningitidis multi", "Men-Hib", 
  #   "S. pneumoniae 01", "S. pneumoniae 03", "S. pneumoniae 04", "S. pneumoniae 05", "S. pneumoniae 06a", 
  #   "S. pneumoniae 06b", "S. pneumoniae 07f", "S. pneumoniae 09v", "S. pneumoniae 14", "S. pneumoniae 18c", 
  #   "S. pneumoniae 19a", "S. pneumoniae 19f", "S. pneumoniae 23f", 
  #   "S. pneumoniae multi 7", "S. pneumoniae multi 10", "S. pneumoniae multi 11", "S. pneumoniae multi 13",
  #   "Measles", "Mumps", "Rubella", "MMR multi", "Varicella", "MMRV multi")) %>%
  mutate(pat_tidy = factor(pat_tidy, levels = pat_tidy))

# List of immunogencity names
tidy_names_imm <- data.frame(
  imm_name = c("measl", "mumps", "rubel", "vzv", 
               "pertupt", "pertufha", "pertuprn", "dipht", "tetan", 
               "polio1", "polio2", "polio3", "mena", "menc", "meny"),
  imm_name_tidy = c("Measles", "Mumps", "Rubella", "Varicella", 
                    "Diphteria", "Tetanus", "Pertussis PT", "Pertussis FHA", "Pertussis PRN",
                    "Polio 1", "Polio 2", "Polio 3", 
                    "N. meningitidis A", "N. meningitidis C", "N. meningitidis Y"))

# List of cluster names
tidy_names_clu <- data.frame(
  clu = c("1_Temperate", "2_Subtropical", "2_Tropical", "3_Tropical", "1_North", 
          "2_South", "3_Tropicalsouth", "4_Tropicalsouth", "5_Subtropicalsouth",
          "3_Temperatesouth", "4_Temperatesouth", "6_Temperatesouth", NA),
  clu_tidy = c("Temperate", "Subtropical", "Tropical", "Tropical", "North",
               "South", "Tropical south", "Tropical south", "Subtropical south",
               "Temperate south", "Temperate south", "Temperate south", "No latitude")) %>%
  mutate(clu_tidy = factor(clu_tidy, levels = c("No latitude", "North",
    "Temperate", "Subtropical", "Tropical", "Tropical south", 
    "Subtropical south", "Temperate south", "South")))

####################################################################################################
# END
####################################################################################################