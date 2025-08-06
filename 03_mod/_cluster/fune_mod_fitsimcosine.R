################################################################################
# Purpose of script: To fit brms models to simulations of the studies
################################################################################

# Setup ------------------------------------------------------------------------
rm(list = ls())
#setwd("projects/_seas_imm")
source("00_pck.R")
dirs$outputs <- glue("outputs/modfit")
dirs$simcosfit <- glue("outputs/simcosfit")

# Read the simulated data -------------------------------------------------------
# Retrieve the simulation model and number from the command line arguments
args_fo <- Sys.getenv("arg_fo")
args_simx  <- as.integer(Sys.getenv("arg_simx", unset = NA))
# Read in the data
dat <- readRDS(glue("{dirs$simcosfit}/simcos_{args_fo}.rds"))[,args_simx] %>%
  as.data.frame()

# Run the models ---------------------------------------------------------------

  # Run uvn models ---------------------------------------------------------------
if(!str_detect(args_fo, "multi")) {
  if(str_detect(args_fo, "csi_reg")) {
    mod <- bake(file = glue(
      "outputs/simcos_models/modsimcos_{args_simx}_{args_fo}.rds"), {
        mod <- brm(formula = funcos_sim ~ sin(pi_vac_week_v1)*clu + 
                     cos(pi_vac_week_v1)*clu, 
                   data = dat, family = Gamma(link = "log"), init = 0, 
                   iter = 4000, chains = 4, cores = 4, 
                   save_pars = save_pars(all = TRUE))
        return(mod)
      }) 
  } else {
    mod <- bake(file = glue(
      "outputs/simcos_models/modsimcos_{args_simx}_{args_fo}.rds"), {
        mod <- brm(formula = funcos_sim ~ sin(pi_vac_week_v1) + 
                     cos(pi_vac_week_v1), 
                   data = dat, family = Gamma(link = "log"), init = 0, 
                   iter = 4000, chains = 4, cores = 4, 
                   save_pars = save_pars(all = TRUE))
        return(mod)
      }) 
  }
  
  # Run mvn models ---------------------------------------------------------------
} else {
  # Prepare mvn data 
  dat_mvn <- dat %>%
    select(-funcos) %>%
    group_by(pat, mod, clu, imm_name) %>%
    mutate(pid = row_number()) %>%
    ungroup() %>%
    pivot_wider(names_from = imm_name, values_from = funcos_sim, names_prefix = "funcos_sim_")
  
  # Run mvn models 
  if(str_detect(args_fo, "csi_reg")) {
    # Write formula 
    bf_formula <- flatten(lapply(str_subset(names(dat_mvn), "funcos_sim"), function(x) {
      list(reformulate(termlabels = c("sin(pi_vac_week_v1)*clu", "cos(pi_vac_week_v1)*clu"), 
                       response = x, intercept = TRUE)) }))
    mbf_formula <- Reduce(`+`, lapply(bf_formula, function(x) bf(x))) + gaussian(link = "log")
  } else {
    # Write formula 
    bf_formula <- flatten(lapply(str_subset(names(dat_mvn), "funcos_sim"), function(x) {
      list(reformulate(termlabels = c("sin(pi_vac_week_v1)", "cos(pi_vac_week_v1)"), 
                       response = x, intercept = TRUE)) }))
    mbf_formula <- Reduce(`+`, lapply(bf_formula, function(x) bf(x))) + gaussian(link = "log")
  }
  # Model 
  mod <- bake(file = glue(
    "outputs/simcos_models/modsimcos_{args_simx}_{args_fo}.rds"), {
      mod <- brm(formula = mbf_formula,  data = dat_mvn, 
                 init = 0, iter = 4000, chains = 4, cores = 4, save_pars = save_pars(all = TRUE))
      return(mod)
      }) 
}
  
################################################################################
# End
################################################################################