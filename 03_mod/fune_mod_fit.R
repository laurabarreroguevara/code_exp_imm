################################################################################
# Purpose of script: To fit brms models to all the studies
################################################################################

# Setup ------------------------------------------------------------------------
rm(list = ls())
source("00_pck.R")
source("fun_brms_mod.R")

args_pat <- Sys.getenv("arg_pat")
dirs$outputs <- glue("outputs/{args_pat}")

# Read the data ----------------------------------------------------------------
l_formula <- readRDS(file = glue("outputs/{args_pat}/formula_brms_{args_pat}_l.rds"))

args_dat <- Sys.getenv("arg_dat")
dat <- readRDS(file = glue("{dirs$outputs}/{args_dat}"))

# Retrieve your model formula from the command line arguments ------------------
args_fo <- Sys.getenv("arg_fo")

# Run the models ---------------------------------------------------------------
mod <- fun_brms_mod(fo = args_fo, dat = dat, crit = TRUE)

################################################################################
# End
################################################################################
