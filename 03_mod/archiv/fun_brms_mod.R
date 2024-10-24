################################################################################
# Purpose of script: Function to fit brms with added criterion 
################################################################################

fun_brms_mod <- function(
  fo = "bmad_base", 
  dat = dat,
  crit = T
  ) {
  # Arguments 
  ## fo = string, name of the formula to call from list l_formula
  ## dat = data frame, data for the model 
  # Returns
  ## Fitted brms model with added criterion 
  mod_f <- bake(
    file = glue("{dirs$outputs}/{fo}.rds"), {
      mod <- brm(formula = l_formula[[fo]],
                 data = dat, family = Gamma(link = "log"),
                 warmup = 1000, iter = 10000, chains = 4, cores = 4,
                 init = 0, control = list(adapt_delta = 0.95),
                 save_all_pars = TRUE)
      if(crit == T) {
        mod <- add_criterion(mod, criterion = c("waic"))
      }
    }
  )
  return(mod_f)
}

################################################################################
# End
################################################################################