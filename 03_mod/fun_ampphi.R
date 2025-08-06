####################################################################################################
# Function to get amplitude from cos and sin estimates
####################################################################################################
fun_amp_ls <- function(p_cos = p_cos, p_sin = p_sin, mu = mu) {
  amp <- (sqrt(p_cos^2 + p_sin^2))/mu
  return(amp)
}

fun_amp_emp <- function(p_cos = p_cos, p_sin = p_sin, mu = mu, val_return = "amp_ns") {
  omega <- 2 * pi / 52
  weeks <- seq(1, 52, length.out = 52*2)
  # Simulate function in log scale 
  fcos_log_mat <- matrix(mu, nrow = length(weeks), ncol = length(p_cos), byrow = TRUE) +
    outer(cos(weeks * omega), p_cos, `*`) + outer(sin(weeks * omega), p_sin, `*`)
  # Simulate function in natural scale 
  fcos_nat_mat <- exp(fcos_log_mat)
  # Obtain mean and max of the 
  mean_nat <- colMeans(fcos_nat_mat)
  max_nat <- apply(fcos_nat_mat, 2, max)
  # Return
  amp <- switch(val_return,
                # Amplitude natural scale, symmetric 
                "amp_ns" = (max_nat / mean_nat) - 1,
                # Mean natural scale, symmetric 
                "mu_ns" = mean_nat,
                # Amplitude natural scale, asymmetric 
                "amp_na" = (max_nat / exp(mu)) - 1,
                stop("Unknown val_return"))
  return(amp)
}

####################################################################################################
# Function to get phase from cos and sin estimates
####################################################################################################
fun_phi <- function(p_cos = p_cos, p_sin = p_sin, phi_type = "week") {
  # Phase in radians (from -pi to pi)
  phirad <- atan2(p_sin, p_cos)
  # Shift phase (from 0 to 2*pi)
  # We want to wrap it around if it's greater than 2*pi or less than 0
  phishifted <- (phirad + 2 * pi) %% (2 * pi)
  # Scale to a 0–52 week year:
  phi <- (phishifted / (2 * pi)) * 52
  # Return
  return(case_when(phi_type == "week" ~ phi,
                   phi_type == "rad" ~ phishifted))
}

####################################################################################################
# Function to get summary statistics from the phase
####################################################################################################
fun_phisummary <- function(phivec, summ_type = "phi_mean") {
  # Transform to circular
  phicir <- circular::circular(phivec * ((2*pi)/52), units = "radians", modulo = "2pi")
  # Obtain summary statistics 
  phi_summary <- list(
    phi_mean = as.numeric(mean(phicir)),
    phi_min = as.numeric(quantile(phicir, probs = 0)),
    phi_ci95lo = as.numeric(quantile(phicir, probs = 0.025)),
    phi_q25 = as.numeric(quantile(phicir, probs = 0.25)),
    phi_median = as.numeric(quantile(phicir, probs = 0.5)),
    phi_q75 = as.numeric(quantile(phicir, probs = 0.75)),
    phi_ci95up = as.numeric(quantile(phicir, probs = 0.975)),
    phi_max = as.numeric(quantile(phicir, probs = 1))
  )
  # Scale to a 0–52 week year:
  phi_summary <- lapply(phi_summary, function(x) (x*52) / (2*pi))
  # Return
  return(phi_summary[[summ_type]])
}
####################################################################################################
# END
####################################################################################################