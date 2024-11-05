####################################################################################################
# Function to create SE2I2R POMP model with seasonal vaccine protection
####################################################################################################

fun_createSE2I2Rmod <- function(n_years = 100, 
                                country_name = "Colombia",
                                dt = 0.1,
                                covars,
                                debug_bool = F) {
  # Args: 
  # n_years: no of years of simulations (integer)
  # country_name: country name (string, "Colombia")
  # dt: time step separating consecutive observations (in weeks)
  # covars: df with time, rh and temperature
  # debug_bool: boolean (T or F), should messages be displayed to help debug? 
  # Returns:
  # pomp object 
  
  # Add covars 
  covars <- as.data.frame(covars[, c("time", "RH_pred", "Te")])
  
  # Create list of Csnippets 
  components_l <- list()
  # Globs: Function to create school terms
  components_l$globs <- Csnippet(paste(
  "// Indicator function of school term (Returns 1 during school term, -1 during holidays)
   static double isSchool(double time, int country) {
     //Args:
     // time: time 
     // country: character indicating country (1 for Colombia)
     // Returns: 1 during school term, -1 during holidays
     
     double wk_no = fmod(time, 52.0); // Week no (remainder of integer division by 52)
     double out; 
     
     if(country == 1) {
       out = ((wk_no >= 3.0 && wk_no < 15.0) || 
       (wk_no >= 16.0 && wk_no < 24.0) ||
       (wk_no >= 27.0 && wk_no < 40.0) ||
       (wk_no >= 41.0 && wk_no < 48.0)) ? 1.0 : -1.0;
     } else {
       out = 0.0;
     }
     return(out);
   }", 
  # Add country and debug 
  sprintf("static int c_name = %d;\nstatic int debug = %d;", 
          if(country_name == "Colombia") {1} else {0}, as.integer(debug_bool)), sep = "\n"))
  
  # Initial conditions
  components_l$rinit <- Csnippet(" 
    // Initial conditions
    V = V0;
    S = S0; 
    E1 = E0 / 2.0;
    E2 = E0 / 2.0;
    I1 = I0 / 2.0;
    I2 = I0 / 2.0; 
    R = 1.0 - S0 - E0 - I0 - V0;
    CC = 0;
  ")
  
  # Skeleton 
  components_l$skel <- Csnippet(" 
  // Deterministic skeleton
    
  // Calculate seasonal terms
  double beta0 = R0 * gamma; // Transmission rate
  
  // Proportion of weeks at school 
  double p_school; 
  double values[] = {40.0 / 52.0,
                     40.0 / 52.0};
  if (c_name >= 1 && c_name <= 9) {
    p_school = values[c_name];
  } else {
    p_school = values[0];
  }
    
  // Climate forcing
  double s_clim_t = e_Te * ((Te / Te_mean) - 1) + e_RH * ((RH_pred / RH_mean) - 1);
  //double s_clim_t = e_Te * ((Te)) + e_RH * ((RH_pred));
  s_clim_t = exp(s_clim_t);
  
  // Term-time forcing
  double s_term_t = 1.0 + amp_term * isSchool(t, c_name);
  s_term_t /= (1.0 - amp_term * (1.0 - 2.0 * p_school)); 
    
  // Force of infection
  double lambda_t = beta0 * s_clim_t * s_term_t * (I1 + I2); 
    
  // Debug
  if(debug && (t <= 52.0)) {
    Rprintf(\"t=%.2f, country=%d, school=%.1f\\n\\n\", t, c_name, isSchool(t, c_name));
  }
    
  // Protection 
  double omega = 2.0 * M_PI / 52.0; // Annual angular frequency (per week)
  double prot_t = amp_prot * cos((omega * t) - (omega * phi_prot)); 
  prot_t = prot0 * (1.0 + prot_t);
    
  // ODEs
  DV = mu * (p_V * prot_t) - mu * V;
  DS = mu * (1.0 - (p_V * prot_t)) - (lambda_t + mu) * S; 
  DE1 = lambda_t * S - (2.0 * sigma + mu) * E1; 
  DE2 = 2.0 * sigma * (E1 - E2) - mu * E2; 
  DI1 = 2.0 * sigma * E2 - (2.0 * gamma + mu) * I1;
  DI2 = 2.0 * gamma * (I1 - I2) - mu * I2; 
  DR = 2.0 * gamma * I2 - mu * R; 
  DCC = 2.0 * gamma * I1;
  ")
  
  po <- pomp(
    data = data.frame(time = seq(from = dt, to = n_years * 52, by = dt), X = NA),
    #data = data.frame(time = 1:(max(covars$time) - 1), CC_obs = NA), 
    times = "time",
    covar = covariate_table(covars, times = "time"),
    t0 = 0,
    obsnames = "X",
    statenames = c("V", "S", "E1", "E2", "I1", "I2", "R", "CC"),
    skeleton = vectorfield(components_l[["skel"]]),
    rinit = components_l[["rinit"]],
    globals = components_l[["globs"]],
    accumvars = c("CC"), # Incidence of cases, reset to 0 at the end of every time step
    params = c(
      mu = 1 / 80 / 52, # Birth/death rate, average life expectancy: 80 yr
      R0 = 10, # Basic reproduction no
      sigma = 7 / 14, # 1 / average latent period, average latent period: 14
      gamma = 7 / 7, # 1 / average infectious period, average infectious period: 7 
      amp_term = 0, # amplitude of term-time forcing
      e_Te = 0, # effect of temperature 
      Te_mean = mean(covars$Te, na.rm = T), # Temporal mean of Te 
      e_RH = 0, # effect of relative humidity
      RH_mean = mean(covars$RH_pred, na.rm = T), # Temporal mean of RH
      p_V = 0, # coverage (probability to get vaccinated)
      prot0 = 0, # average vaccine protection
      amp_prot = 0, # amplitude of seasonal vaccine protection
      phi_prot = 0, # phase of seasonal vaccine protection
      V0 = 0, S0 = 1 / 10, E0 = 1e-4, I0 = 1e-4 # Initial conditions 
      ),
    paramnames = c("mu", "R0", "sigma", "gamma", "amp_term", "e_Te", "Te_mean", "e_RH", "RH_mean",
      "p_V", "prot0", "amp_prot", "phi_prot", "V0","S0", "E0", "I0"),
    verbose = F
  )
  return(po)
}

