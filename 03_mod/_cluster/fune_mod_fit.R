################################################################################
# Purpose of script: To fit brms models to all the studies
################################################################################

# Setup ------------------------------------------------------------------------
rm(list = ls())
source("00_pck.R")

args_pat <- Sys.getenv("arg_pat")
dirs$outputs <- glue("outputs/modfit")

# Read the data ----------------------------------------------------------------
l_formula <- readRDS(file = glue("{dirs$outputs}/formula_brms_{args_pat}_l.rds"))

args_dat <- Sys.getenv("arg_dat")
dat <- readRDS(file = glue("{dirs$outputs}/{args_dat}"))

# Retrieve your model formula from the command line arguments ------------------
args_fo <- Sys.getenv("arg_fo")

# Retrieve the iters from the command line arguments ---------------------------
args_iter <- as.numeric(Sys.getenv("arg_iter"))

# Add criterion? 
crit <- TRUE
# Run the models ---------------------------------------------------------------
mod <- bake(file = glue("outputs/models/{args_pat}/{args_fo}.rds"), {
  mod <- brm(formula = l_formula[[args_fo]],
             data = dat, family = Gamma(link = "log"),
             warmup = 1000, iter = args_iter, chains = 4, cores = 4,
             init = 0, control = list(adapt_delta = 0.95),
             save_all_pars = TRUE)
  if(crit == TRUE) { mod <- add_criterion(mod, criterion = c("waic")) }
  return(mod)
})

################################################################################
# End
################################################################################
