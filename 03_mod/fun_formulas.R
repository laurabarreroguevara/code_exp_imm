# Function formulas --------------------------------------------------------------------------------
fun_formulas <- function(pathogen = "rota", 
                         data = "all", 
                         doses = "d1", 
                         model_nm = "csi", 
                         varname = "", 
                         varname2 = "", 
                         others = "", 
                         multi = NULL, 
                         cens = TRUE) {
  
  # Base covariates always included and additions 
  covars_mod <- c("1", "sex", "age", others)
  
  # Seasonal covariates based on model name
  seasonal_covars <- switch(
    model_nm,
    "cc"    = "s(vac_week_v1, bs = 'cc')",
    "cci"   = glue("s(vac_week_v1, {varname}, bs = 'fs', xt = list(bs = 'cc'))"),
    "cs"    = c("cos(pi_vac_week_v1)", "sin(pi_vac_week_v1)"),
    "csni"  = c("cos(pi_vac_week_v1)", "sin(pi_vac_week_v1)", varname),
    "csi"   = c(glue("{varname} * cos(pi_vac_week_v1)"), glue("{varname} * sin(pi_vac_week_v1)")),
    "cs2i"  = c(glue("{varname} * cos(pi_vac_week_v1)"), glue("{varname} * sin(pi_vac_week_v1)"),
                glue("{varname2} * cos(pi_vac_week_v1)"), glue("{varname2} * sin(pi_vac_week_v1)")),
    "cs2ni" = c("cos(pi_vac_week_v1)", "sin(pi_vac_week_v1)", varname, varname2),
    NULL  # For "base" or unrecognized model names
  )
  if (!is.null(seasonal_covars)) covars_mod <- c(covars_mod, seasonal_covars)
  
  # Time to vaccination covariates based on doses
  time_covars <- switch(
    doses,
    "d1" = "time_vl_s1",
    "d2" = c("time_v1_v2", "time_vl_s1"),
    "d3" = c("time_v1_v2", "time_v2_v3", "time_vl_s1"),
    "d4" = c("time_v1_v2", "time_v2_v3", "time_v3_v4", "time_vl_s1"),
    "d5" = c("time_v1_v2", "time_v2_v3", "time_v3_v4", "time_v4_v5", "time_vl_s1"),
    NULL  # In case of unrecognized doses
  )
  if (!is.null(time_covars)) covars_mod <- c(covars_mod, time_covars)
  
  # Determine the response variable based on 'multi' and 'cens'
  if (!is.null(multi)) {
    response_mod <- if(cens) { glue("imm_val_s1_{multi} | cens(censored_{multi})")
    } else { glue("imm_val_s1_{multi}") }
  } else {
    response_mod <- if(cens) { "imm_val_s1 | cens(censored)"
    } else { glue("imm_val_s1") }
  }
  
  # Build the formula
  form_mod <- list(reformulate(termlabels = covars_mod, response = response_mod, intercept = TRUE))
  
  # Generate the model name based on parameters
  doses_nm <- if (doses == "d1") "v1" else "vl"
  model_suffix <- case_when(model_nm %in% c("base", "cs", "cc") ~ "",
                            model_nm %in% c("cs2ni", "cs2i")    ~ glue("_{varname}_{varname2}"),
                            model_nm %in% c("cci", "csni", "csi") ~ glue("_{varname}"),
                            .default = "")
  
  names(form_mod) <- glue("bm{doses_nm}_{data}_{pathogen}_{model_nm}{model_suffix}")
  
  return(form_mod)
}

# Multi --------------------------------------------------------------------------------------------

fun_formulas_multipathogen <- function(pathogen = "multi_men", 
                                       data = "s08",
                                       doses = "d3", 
                                       model_nm = "base", 
                                       varname = "", 
                                       varname2 = "",
                                       others = "", 
                                       multi_pat = "",
                                       cens = FALSE) {

  # Run for each response fun_formula 
  bf_formula <- flatten(lapply(multi_pat, function(x) {
    fun_formulas(pathogen = glue("{pathogen}_{x}"), data = data, doses = doses, model_nm = model_nm, 
                 varname = varname, varname2 = varname2, others = others, multi = x, cens = cens)
  }))
  
  # Add family distribution 
  if(cens == FALSE) {
    name_cens <- "gauss"
    mbf_formula <- Reduce(`+`, lapply(bf_formula, function(x) bf(x))) + gaussian(link = "log")
  } else {
    name_cens <- "gamma"
    mbf_formula <- Reduce(`+`, lapply(bf_formula, function(x) bf(x))) 
  }
  
  # Generate the model name based on parameters
  model_suffix <- paste0(str_replace(str_remove(names(bf_formula), multi_pat), "__", "_")[[1]],
                         "_", name_cens)
  mbf_formula <- list(mbf_formula)
  names(mbf_formula) <- model_suffix
  
  return(mbf_formula)
}

# Multi long ---------------------------------------------------------------------------------------

fun_formulas_multipathogenlong <- function(pathogen = "multi_men", 
                                       data = "s08",
                                       doses = "d3", 
                                       model_nm = "base", 
                                       varname = "", 
                                       varname2 = "",
                                       others = "", 
                                       multi_pat = NULL,
                                       cens = FALSE) {
  
  # Run for each response fun_formula 
  bf_formula <- fun_formulas(pathogen = pathogen, data = data, doses = doses, model_nm = model_nm, 
                 varname = varname, varname2 = varname2, others = others, multi = NULL, cens = cens)
  
  # Add family distribution 
  if(cens == FALSE) {
    name_cens <- "gauss"
    mbf_formula <- bf(bf_formula[[1]], sigma ~ 0 + imm_sero_s1) + gaussian(link = "log")
  } else {
    name_cens <- "gamma"
    mbf_formula <- bf(bf_formula[[1]], sigma ~ 0 + imm_sero_s1)
  }
  
  # Generate the model name based on parameters
  model_suffix <- paste0(names(bf_formula)[[1]], "_", name_cens)
  mbf_formula <- list(mbf_formula)
  names(mbf_formula) <- model_suffix
  
  return(mbf_formula)
}





