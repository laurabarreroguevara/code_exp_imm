################################################################################
# Purpose of script: To fit brms models to simulations of the studies
################################################################################

# Setup ------------------------------------------------------------------------
rm(list = ls())
source("00_pck.R")
dirs$outputs <- glue("outputs/modfit")
dirs$simpredfit <- glue("outputs/simpredfit")

# Retrieve the data, model and simulation names
args_dat <- Sys.getenv("arg_dat")
args_fo <- Sys.getenv("arg_fo")
args_simx  <- as.integer(Sys.getenv("arg_simx", unset = NA))

# Retrieve the data 
dat <- readRDS(file = glue("{dirs$outputs}/{args_dat}"))
# Retrieve the posterior and select simulation
simpred <- readRDS(file = glue("{dirs$simpredfit}/simpred_{args_fo}.rds"))

# Run the models ---------------------------------------------------------------

# Run uvn models ---------------------------------------------------------------
if(!str_detect(args_fo, "multi")) {
  
  # Add simulation to the data -------------------------------------------------
  dat <- dat %>% 
    mutate(
      # Select simulation to use (1-100)
      simpred = simpred[args_simx,],
      simpred_cens = case_when(simpred < imm_cutoff_s1 ~ 1,
                               simpred >= imm_cutoff_s1 ~ 0)) %>%
    rename(
      clu = str_extract(args_fo, "reg_6|reg_4|reg_3|reg_2"))
  
  # Run models -----------------------------------------------------------------
  if(str_detect(args_fo, "csi_reg")) {
    mod <- bake(file = glue(
      "outputs/simpred_models/modsimpred_{args_simx}_{args_fo}.rds"), {
        mod <- brm(formula = simpred ~ sin(pi_vac_week_v1)*clu + 
                     cos(pi_vac_week_v1)*clu, 
                   data = dat, family = Gamma(link = "log"), init = 0, 
                   iter = 4000, chains = 4, cores = 4, 
                   save_pars = save_pars(all = TRUE))
        return(mod)
      }) 
  } else {
    mod <- bake(file = glue(
      "outputs/simpred_models/modsimpred_{args_simx}_{args_fo}.rds"), {
        mod <- brm(formula = simpred ~ sin(pi_vac_week_v1) + 
                     cos(pi_vac_week_v1), 
                   data = dat, family = Gamma(link = "log"), init = 0, 
                   iter = 4000, chains = 4, cores = 4, 
                   save_pars = save_pars(all = TRUE))
        return(mod)
      }) 
  }
  
  # Run mvn models ---------------------------------------------------------------
} else {
  
  # Add simulation to the data -------------------------------------------------
  dat_mvn <- dat
  for(i in dimnames(simpred)[[3]]) {
    imm_name <- str_remove_all(i, "immvals1anti")
    dat_mvn <- dat_mvn %>%
      rename(
        imm_cutoff_s1 = paste0("imm_cutoff_s1_anti_", imm_name)) %>%
      mutate(
        # Select simulation to use (1-100)
        simpred = simpred[args_simx, , i],
        simpredcens = case_when(simpred < imm_cutoff_s1 ~ 1,
                                simpred >= imm_cutoff_s1 ~ 0)) %>%
      rename(
        !!paste0("imm_cutoff_s1_anti_", imm_name) := imm_cutoff_s1,
        !!paste0("simpred_", imm_name) := simpred,
        !!paste0("simpredcens_", imm_name) := simpredcens)
  }
  
  dat_mvn <- dat_mvn %>%
    rename(
      clu = str_extract(args_fo, "reg_6|reg_4|reg_3|reg_2"))
  
  # Create formula mvn models 
  if(str_detect(args_fo, "csi_reg")) {
    # Write formula 
    bf_formula <- flatten(
      lapply(str_subset(names(dat_mvn), "simpred_"), function(x) {
        list(reformulate(termlabels = c("sin(pi_vac_week_v1)*clu", 
                                        "cos(pi_vac_week_v1)*clu"), 
                         response = x, intercept = TRUE)) }))
    mbf_formula <- Reduce(`+`, lapply(
      bf_formula, function(x) bf(x))) + gaussian(link = "log")
    
  } else {
    # Write formula 
    bf_formula <- flatten(
      lapply(str_subset(names(dat_mvn), "funcos_sim"), function(x) {
        list(reformulate(termlabels = c("sin(pi_vac_week_v1)", 
                                        "cos(pi_vac_week_v1)"), 
                         response = x, intercept = TRUE)) }))
    mbf_formula <- Reduce(`+`, lapply(
      bf_formula, function(x) bf(x))) + gaussian(link = "log")
  }
  # Model 
  mod <- bake(file = glue(
    "outputs/simpred_models/modsimpred_{args_simx}_{args_fo}.rds"), {
      mod <- brm(formula = mbf_formula,  data = dat_mvn, init = 0, iter = 4000, 
                 chains = 4, cores = 4, save_pars = save_pars(all = TRUE))
      return(mod)
    }) 
}

################################################################################
# End
################################################################################
