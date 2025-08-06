################################################################################
# Purpose of script: To fit brms models to all the studies
################################################################################

# Setup ------------------------------------------------------------------------
rm(list = ls())
source("00_pck.R")
dirs$outputs <- glue("outputs/climfit")

# Read the data ----------------------------------------------------------------
# Filter for the llid 
args_llidx  <- as.integer(Sys.getenv("arg_llidx", unset = NA))
dat <- readRDS(file = glue("{dirs$outputs}/all_nasapower.rds")) %>%
  filter(llid == unique(all_clim$llid)[args_llidx])

# Define models ----------------------------------------------------------------
args_clim  <- Sys.getenv("arg_clim")
clim_l <- if(args_clim == "all") unique(dat$clim) else c(args_clim)

# Retrieve the iters from the command line arguments ---------------------------
args_iter <- as.numeric(Sys.getenv("arg_iter"))

# Run the models ---------------------------------------------------------------
walk(clim_l, function(x) {
  bake(file = glue("outputs/clim_models/brmclim_{x}_.rds"), {
    brm(formula = value_norm ~ cos(pi_week) + sin(pi_week),
               data = filter(dat, clim == x), family = gaussian(),
               warmup = 1000, iter = args_iter, chains = 4, cores = 4,
               save_all_pars = TRUE)
  })
})

################################################################################
# End
################################################################################
