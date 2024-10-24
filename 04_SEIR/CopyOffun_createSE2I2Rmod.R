


fun_createSE2I2Rmod <- function(n_years = 100, 
                                country_name = "Colombia",
                                dt = 0.1,
                                covars,
                                amp_term = 0.25,
                                amp_clim = -0.1,
                                prot0 = 0.2,
                                amp_prot = 0,
                                debug_bool = F) {
  # Args: 
  # n_years: no of years of simulations (integer)
  # country_name: country name (string, "Colombia")
  # debug_bool: boolean (T or F), should messages be displayed to help debug? 
  # dt: time step separating consecutive observations (in weeks)
  # Returns:
  # pomp object 
  
  # Add covars 
  covars <- as.data.frame(covars[, c("time", "clim")])
  
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
    S = S0; 
    E1 = E0 / 2.0;
    E2 = E0 / 2.0;
    I1 = I0 / 2.0;
    I2 = I0 / 2.0; 
    R = 1.0 - S0 - E0 - I0;
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
  double s_clim_t = exp(amp_clim * clim);
    
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
  double prot_t = a1 * cos(omega * t) + b1 * sin(omega * t); 
  prot_t = prot0 * (1.0 + amp_prot * prot_t);
    
  // ODEs
  DS = mu * (1.0 - prot_t) - (lambda_t + mu) * S; 
  DE1 = lambda_t * S - (2.0 * sigma + mu) * E1; 
  DE2 = 2.0 * sigma * (E1 - E2) - mu * E2; 
  DI1 = 2.0 * sigma * E2 - (2.0 * gamma + mu) * I1;
  DI2 = 2.0 * gamma * (I1 - I2) - mu * I2; 
  DR = mu * (prot_t) + 2.0 * gamma * I2 - mu * R; 
  DCC = 2.0 * gamma * I1;
  ")
  
  po <- pomp(
    data = data.frame(time = seq(from = dt, to = n_years * 52, by = dt), X = NA),
    times = "time",
    covar = covariate_table(covars, times = "time"),
    t0 = 0,
    obsnames = "X",
    statenames = c("S", "E1", "E2", "I1", "I2", "R", "CC"),
    accumvars = c("CC"), # Incidence of cases, reset to 0 at the end of every time step
    paramnames = c(
      "mu", # Birth/death rate
      "R0", # Basic reproduction no
      "sigma", # 1 / average latent period
      "gamma", # 1 / average infectious period
      "amp_term", # amplitude of term-time forcing
      "amp_clim", # amplitude of climate forcing
      "S0", "E0", "I0", # Initial conditions 
      "prot0", # average protection
      "amp_prot", # amplitude of protection forcing
      "a1", "b1"), # Coefficients of first Fourier component (1-yr period)
    params = c(
      mu = 1 / 75 / 52, # Average life expectancy: 75 yr
      R0 = 10, 
      sigma = 7 / 14, # Average latent period: 14
      gamma = 7 / 7, # Average infectious period: 7 
      amp_term = 0, 
      amp_clim = amp_clim, 
      S0 = 1 / 10, 
      E0 = 1e-4, 
      I0 = 1e-4,
      prot0 = prot0,
      amp_prot = amp_prot,
      a1 = 0.26, 
      b1 = -0.16),
    globals = components_l[["globs"]],
    skeleton = vectorfield(components_l[["skel"]]),
    rinit = components_l[["rinit"]],
    verbose = F
  )
  return(po)
}

