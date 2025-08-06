################################################################################
# Simulations for the SE2I2R model 
################################################################################

fun_traj_mod_simulations = function(clim_name = "w_hum_spe", 
                                    amp_vec = c(0, -0.05, -0.10),
                                    R0_val = 10,
                                    n_years = 200,
                                    delta_t = 0.1,
                                    amp_term = 0.25,
                                    mun = "BOGOTA",
                                    country_name = "Colombia") {
  # Arguments: 
  # clim_name: climatic variable name (string: "w_hum_spe")
  # amp_vec: amplitude climate (vector of integers)
  # R0_val:  R0 varicella (integer)
  # n_years: no of years of simulations (integer)
  # country_name: country name (string, "Colombia", "Mexico", "Panama", 
  ## "Costa Rica", "Nicaragua", "Honduras", "El Salvador", "Guatemala" or "Belize")
  # delta_t: time step separating consecutive observations (in weeks)
  # amp_term: amplitude school terms (integer)
  # mun: muncipality name (strings)
  # Returns: 
  
  # Prepare covars 
  df_covars_sel <- df_covars %>%
    filter(mun_name == mun) %>%
    filter(clim_var == clim_name) %>%
    select(week, value_stand) %>%
    rename(clim = value_stand)
  covars <- data.frame(week = rep(1:52, n_years)) %>%
    left_join(df_covars_sel) %>%
    mutate(time = 1:(52*n_years)) %>%
    select(-week)
  
  # 1. Create pomp object and make basic checks
  mod <- fun_createSE2I2Rmod(n_years = n_years, 
                             country_name = country_name, 
                             covars = covars,  #Here covars
                             amp_term = amp_term,
                             dt = delta_t, 
                             debug_bool = F)
  stopifnot(sum(rinit(mod)) == 1) # Initial conditions must sum to 1
  # 2. Run one simulation and check that pop size is constant and sums to 1 (no climate forcing)
  pomp::coef(mod, "R0") <- R0_val
  pomp::coef(mod, "amp_clim") <- 0
  ## Initialize model at endemic equilibrium
  EE <- fun_eeunforcedmodel(po = mod)
  pomp::coef(mod, c("S0", "E0", "I0")) <- unname(EE)
  ## Run simulation
  tj <- trajectory(object = mod, 
                   format = "data.frame", 
                   method = "lsoda")
  stopifnot(round(rowSums(select(tj, c("S", "E1", "E2", "I1", "I2", "R"))), digits = 3) == 1) # Check total pop size
  # 3. Vary the amplitude of climate forcing
  R0_vec <- R0_val
  amp_clim_vec <- round(amp_vec, 2) 
  pomp::coef(mod, "R0") <- R0_vec
  pomp::coef(mod, c("S0", "E0", "I0")) <- unname(fun_eeunforcedmodel(po = mod))
  all_parms <- expand_grid(R0 = R0_vec, 
                           amp_clim = amp_clim_vec) %>% 
    mutate(.id = seq_len(nrow(.)))
  p_mat <- parmat(params = coef(mod), 
                  nrep = nrow(all_parms))
  p_mat["R0", ] <- all_parms$R0
  p_mat["amp_clim", ] <- all_parms$amp_clim
  ## Run simulation (trajectories)
  sims <- trajectory(object = mod, 
                     params = p_mat, 
                     format = "data.frame", 
                     method = "lsoda") 
  # 4. Get data from simulations 
  sims_long <- sims %>% 
    fun_reformatsimulations(delta_t = delta_t) %>% 
    left_join(y = all_parms) %>%
    filter(between(year, max(year) - 10, max(year))) %>% 
    mutate(mun_name = mun)
  
  return(sims_long)
}

################################################################################
# End
################################################################################