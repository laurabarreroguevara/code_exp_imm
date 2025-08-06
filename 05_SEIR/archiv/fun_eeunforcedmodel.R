################################################################################
# Calculate the endemic equilibrium (EE) of the unforced SEIR model
# See Keeling&Rohani's modeling book, p 42
################################################################################

fun_eeunforcedmodel <- function(po) {
  # Args: 
  # po: model object (pomp object)
  # Returns: name vector with values of endemic equilibrium 
  
  R0 <- unname(coef(po, "R0"))
  mu <- unname(coef(po, "mu"))
  gamma <- unname(coef(po, "gamma"))
  sigma <- unname(coef(po, "sigma"))
  beta0 <- R0 * gamma
  
  out <- c("S" = 1 / R0, 
           "E" = mu * (mu + gamma) / (beta0 * sigma) * (R0 - 1), 
           "I" =  mu / beta0 * (R0 - 1))
  
  return(out)
}

################################################################################
# End
################################################################################