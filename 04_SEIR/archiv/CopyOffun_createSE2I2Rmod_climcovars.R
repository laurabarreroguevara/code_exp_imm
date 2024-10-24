################################################################################
# Create pomp object for SE2I2R model of varicella transmission
# Two sources of seasonal forcing: 
## school terms (square wave) and climate (covariate(exp function))
# State variables are PROPORTIONS
# time is measured in WEEKS; all rates are per WEEK
################################################################################

fun_createSE2I2Rmod <- function(n_years = 100, 
                                country_name = "Colombia",
                                dt = 0.1,
                                covars,
                                amp_term = 0.25,
                                debug_bool = F) {
  
  # Args: 
  # n_years: no of years of simulations (integer)
  # country_name: country name (string, "Colombia", "Mexico", "Panama", 
  ## "Costa Rica", "Nicaragua", "Honduras", "El Salvador", "Guatemala" or "Belize")
  # debug_bool: boolean (T or F), should messages be displayed to help debug? 
  # dt: time step separating consecutive observations (in weeks)
  # Returns:
  # pomp object 
  
  # ADD IN COVARIATES
  covars <- as.data.frame(
    covars[, c("time", 
               "clim")])
  
  # Extract C code from file
  mod_code <- readLines("c-SE2I2R_wcovars.c")
  components_nm <- c("globs", "skel", "rinit")
  components_l <- vector(mode = 'list', length = length(components_nm))
  names(components_l) <- components_nm
  
  for(nm in components_nm) {
    components_l[[nm]] <- mod_code[str_which(mod_code, paste0("start_", nm)):str_which(mod_code, paste0("end_", nm))] %>% 
      str_flatten(collapse = "\n")
    
    if(nm == "globs") {
      components_l[[nm]] <- paste(components_l[[nm]], 
                                  sprintf("static int c_name = %d;\nstatic int debug = %d;", 
                                          if(country_name == "Colombia") {1}
                                          else if(country_name == "Mexico") {2}
                                          else if(country_name == "Panama") {3}
                                          else if(country_name == "Costa Rica") {4}
                                          else if(country_name == "Nicaragua") {5}
                                          else if(country_name == "Honduras") {6}
                                          else if(country_name == "El Salvador") {7}
                                          else if(country_name == "Guatemala") {8}
                                          else if(country_name == "Belize") {9}
                                          else {0}, 
                                          as.integer(debug_bool)), 
                                  sep = "\n")
    }
    components_l[[nm]] <- Csnippet(text = components_l[[nm]])
  }
  
  po <- pomp(data = data.frame(time = seq(from = dt, to = n_years * 52, by = dt), X = NA),
             times = "time",
             covar = covariate_table(covars, times = "time"),
             t0 = 0,
             obsnames = "X",
             statenames = c("S", "E1", "E2", "I1", "I2", "R", "CC"),
             accumvars = c("CC"), # Incidence of cases, reset to 0 at the end of every time step
             paramnames = c("mu", # Birth/death rate
                            "R0", # Basic reproduction no
                            "sigma", # 1 / average latent period
                            "gamma", # 1 / average infectious period
                            "amp_term", # amplitude of term-time forcing
                            "amp_clim", # amplitude of climate forcing
                            "S0", "E0", "I0" # Initial conditions 
                            ), 
             params = c(mu = 1 / 75 / 52, # Average life expectancy: 75 yr
                        R0 = 10, 
                        sigma = 7 / 14, # Average latent period: 14
                        gamma = 7 / 7, # Average infectious period: 7 
                        amp_term = amp_term, 
                        amp_clim = 0, 
                        S0 = 1 / 10, 
                        E0 = 1e-4, 
                        I0 = 1e-4
                        ),
             globals = components_l[["globs"]],
             skeleton = vectorfield(components_l[["skel"]]),
             rinit = components_l[["rinit"]],
             verbose = F
  )
  return(po)
}

################################################################################
# End
################################################################################