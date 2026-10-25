####################################################################################################
# Load packages and themes
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
  mutate(pat_tidy = factor(pat_tidy, levels = pat_tidy))

# List of immunogencity names
tidy_names_imm <- data.frame(
  imm_name = c("measl", "mumps", "rubel", "vzv", 
               "dipht", "tetan", "pertupt", "pertufha", "pertuprn", 
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