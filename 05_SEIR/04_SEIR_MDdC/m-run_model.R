#######################################################################################################
# Implement and run model with seasonal birth cohorts, vaccine effectiveness and waning rate
# Time unit: year; rates are per YEAR
#######################################################################################################

# Load packages -----------------------------------------------------------
rm(list = ls())
source("s-base_packages.R")
library(pomp)

# Set parameters ----------------------------------------------------------
n_seas <- 4L # No of seasons per year

# Relative amplitude of seasonality in vaccine effectiveness and duration of protection
amp_seas <- 0.5 

# Check seasonal indicator function  --------------------------------------
f_seas <- function(time, n_seas) {
  # Args: 
  # time: time (in units corresponding to the no of seasons) [numeric]
  # n_seas: no of seasons [integer]
  # Returns: 
  # Season associated with time [integer between 0 and n_seas - 1]
  
  #season <- floor(time) %% n_seas 
  season <- floor(time %% n_seas) # Integer indicating season (range: 0 to n_seas -1)
  return(season)
}

f_seas <- Vectorize(FUN = f_seas)

dat <- data.frame(time = seq(0, 3 * n_seas, by = 0.1)) %>% 
  mutate(season = f_seas(time = time, n_seas = n_seas), 
         rel_eff = 1 + amp_seas * cos(2 * pi * (season - 0.1) / n_seas)) # Relative vaccine effectiveness

pl <- ggplot(data = dat, mapping = aes(x = time / n_seas, y = season, color = factor(round(rel_eff, 2)))) + 
  geom_point() + 
  theme_classic() + 
  labs(x = "Time (years)", y = "Season no")
print(pl)

seas_eff <- dat %>% select(season, rel_eff) %>% unique() %>% arrange(season)
stopifnot(nrow(seas_eff) == 4)

# Model skeleton ----------------------------------------------------------
skel_code <- Csnippet(
  "
  int seas_cur = (int) floor(fmod(t, n_seas)); // Current season
  double lambda, p_V_cur, seas_B; 
  int i; 
  
  // Pointer to state variables and derivatives 
  double *V_vec = (double *) &V_1; 
  double *DV_vec = (double *) &DV_1; 
  
  // Pointer to season-specific vaccine effectiveness/duration
  double *seas_vec = (double *) &seas_eff_1; 
  
  // Effective vaccine coverage
  p_V_cur = c_V * mu_eff * seas_vec[seas_cur]; 
  
  // FoI 
  seas_B = 1.0 + A_trans * cos(2 * M_PI * (t - phi_trans)); 
  lambda = R0 * (gamma + mu) * seas_B * I; 
  
  // ODE for S
  DS = mu * (1.0 - p_V_cur) - (lambda + mu) * S; 
  
  // ODEs for V states
  for(i = 0; i < n_seas; i++) {
    DV_vec[i] = mu * p_V_cur * (i == seas_cur) - (1.0 / (mu_dur * seas_vec[seas_cur]) + mu) * V_vec[i]; 
    DS +=  V_vec[i] / (mu_dur * seas_vec[seas_cur]); 
  }
  
  // ODEs for other state variables
  DE = lambda * S - (sigma + mu) * E; 
  DI = sigma * E - (gamma + mu) * I; 
  DR = gamma * I - mu * R; 
  "
)

global_code <- Csnippet(sprintf("static int n_seas = %d;", n_seas))

# Create POMP object ------------------------------------------------------
parms_vec <- c("mu" = 1 / 80, # Birth/death rate
               "R0" = 10, # Basic reproduction number
               "sigma" = 365 / 7, # 1 / average latent period
               "gamma" = 365 / 7, # 1 / average infectious period 
               "c_V" = 0.9, # Vaccine coverage
               "mu_eff" = 0.5, # Mean vaccine effectiveness 
               "mu_dur" = 10, # Mean duration of vaccine protection
               "A_trans" = 0, # Amplitude of seasonality in transmission rate
               "phi_trans" = 0 # Phase of seasonality in transmission rate
)

# Add season-specific effectiveness/duration parameters
for(i in 1:nrow(seas_eff)) {
  parms_vec[paste0("seas_eff_", i)] <- seas_eff$rel_eff[i]
}

# Initial conditions
parms_vec["S.0"] <- unname(1 / parms_vec["R0"])
parms_vec["E.0"] <- 1e-4
parms_vec["I.0"] <- 0
parms_vec["R.0"] <- unname(1 - parms_vec["S.0"] - parms_vec["E.0"])
for(i in seq_len(n_seas)) parms_vec[paste0("V_", i, ".0")] <- 0

mod <- pomp(data = data.frame(time = seq(from = 0.01, to = 500, by = 0.01)), 
            times = "time", 
            t0 = 0,
            statenames = c(paste0("V_", 1:n_seas), "S", "E", "I", "R"), 
            params = parms_vec, 
            paramnames = names(parms_vec), 
            skeleton = vectorfield(f = skel_code), 
            globals = global_code, 
            cdir = ".", 
            cfile = "code.c")

stopifnot(all.equal(sum(rinit(mod)), 1))

# Plot seasonal vaccine properties ----------------------------------------
plot(seas_eff$season, parms_vec["mu_eff"] * seas_eff$rel_eff, type = "b", xlab = "Season")
plot(seas_eff$season, parms_vec["mu_dur"] * seas_eff$rel_eff, type = "b", xlab = "Season")

# Run simulation ----------------------------------------------------------
sim <- trajectory(object = mod, format = "data.frame")
sim$N <- rowSums(select(sim, -c(".id", "time")))
sim_long <- sim %>% 
  pivot_longer(cols = -c(".id", "time"), names_to = "var", values_to = "val")

pl <- ggplot(data = sim_long, mapping = aes(x = time, y = val)) + 
  geom_line() + 
  facet_wrap(~ var, scales = "free_y") + 
  theme_classic() + 
  labs(x = "Time (years)", y = "Proportion")
print(pl)

pl <- ggplot(data = sim_long %>% filter(between(time, max(time) - 10, max(time))), 
             mapping = aes(x = time, y = val)) + 
  geom_line() + 
  facet_wrap(~ var, scales = "free_y", ncol = 1) + 
  theme_classic() + 
  labs(x = "Time (years)", y = "Proportion")
print(pl)


#######################################################################################################
# END
#######################################################################################################