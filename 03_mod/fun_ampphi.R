####################################################################################################
# Function to get amplitude from cos and sin estimates
####################################################################################################
fun_amp <- function(p_cos = p_cos, p_sin = p_sin) {
  amp <- sqrt(p_cos^2 + p_sin^2)
}
####################################################################################################
# Function to get phase from cos and sin estimates
####################################################################################################
fun_phi <- function(p_cos = p_cos, p_sin = p_sin) {
  perphi_rad <- case_when(
    p_cos > 0 ~ atan((-1*p_sin)/(p_cos)),
    p_cos < 0 ~ atan((-1*p_sin)/(p_cos)) - pi)
  # Periodic phi in weeks
  perphi_week <- 52*(perphi_rad*-1)/(2*pi)
  # Phi within 1:52 weeks
  phi <- case_when(
    perphi_week < 0 ~ perphi_week + 52,
    perphi_week > 0 ~ perphi_week,
    perphi_week == 0 ~ 0)
  phi <- case_when(phi > 0 & phi < 1 ~ 52, .default = phi)
  return(phi)
}
####################################################################################################
# END
####################################################################################################