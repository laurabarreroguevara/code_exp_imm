####################################################################################################
# Function to generate BRMS formulas
# Constructs a model formula for Bayesian Regression Models (BRMS) that incorporates a combination 
# of fixed covariates, seasonal effects, and time-to-vaccination effects. The function adapts the 
# formula based on model type, vaccine doses, and optional parameters (e.g., multivariate or 
# censoring adjustments).
####################################################################################################

fun_formulas <- function(pathogen = "rota", 
                         data = "all", 
                         doses = "d1", 
                         model_nm = "csi", 
                         varname = "", 
                         varname2 = "", 
                         others = "", 
                         multi = NULL, 
                         cens = TRUE,
                         outseropos = FALSE) {
  
  # Arguments:
  # - pathogen: (string) Identifier for the pathogen (e.g., "rota").
  # - data:     (string) Dataset identifier (e.g., "s08", "s08_dfkgc1", etc.).
  # - doses:    (string) Specifies the dosing scheme ("d1", "d2", "d3", etc.), 
  #             which determines the time-to-vaccination covariates.
  # - model_nm: (string) Model type. Options include: "base" "cc", "cci", "cs", "csni", "csi",
  #             "cs2i", "cs2ni". This influences the seasonal covariates.
  # - varname:  (string) Name of the latitude region. Required for models: "csni", "csi",
  #             "cs2i", "cs2ni".
  # - varname2: (string) Name of the climate region. Required for models: "cs2i", "cs2ni".
  # - others:   (string) Additional covariate(s) to include in the model.
  # - multi:    (logical or identifier) If provided, indicates a multivariate model and adjusts the 
  #             response variable accordingly.
  # - cens:     (logical) Specifies whether the model should include censoring.
  # - outseropos: (logical) Specifies wheter the model should use seropositivitiy as outcome. 
  
  # Output:
  # - form_mod: A named list containing the constructed BRMS model formula.
  
  # 1. Define Base Covariates ----------------------------------------------------------------------
  # These are the default covariates included in every model:
  # - "1": Represents the intercept.
  # - "sex" and "age": Demographic variables.
  # - others: Additional covariates provided by the user.
  covars_mod <- c("1", "sex", "age", others)
  
  # 2. Add Seasonal Covariates ---------------------------------------------------------------------
  # The seasonal covariates vary depending on the chosen model type.
  seasonal_covars <- switch(
    model_nm,
    # Cyclic cubic spline for the vaccination week.
    "cc"    = "s(vac_week_v1, bs = 'cc')",
    # Factor-smoother interaction with latitudinal region.
    "cci"   = glue("s(vac_week_v1, {varname}, bs = 'fs', xt = list(bs = 'cc'))"),
    # Cosine and sine transformations for the vaccination week. 
    "cs"    = c("cos(pi_vac_week_v1)", "sin(pi_vac_week_v1)"),
    # Cosine and sine transformations for the vaccination week, with latitudinal region.
    "csni"  = c("cos(pi_vac_week_v1)", "sin(pi_vac_week_v1)", varname),
    # Cosine and sine transformations for the vaccination week interacting with latitudinal region.
    "csi"   = c(glue("{varname} * cos(pi_vac_week_v1)"), glue("{varname} * sin(pi_vac_week_v1)")),
    # Cosine and sine transformations for the vaccination week interacting with latitudinal region
    # and climate.
    "cs2i"  = c(glue("{varname} * cos(pi_vac_week_v1)"), glue("{varname} * sin(pi_vac_week_v1)"),
                glue("{varname2} * cos(pi_vac_week_v1)"), glue("{varname2} * sin(pi_vac_week_v1)")),
    # Cosine and sine transformations for the vaccination week, with latitudinal region and climate.
    "cs2ni" = c("cos(pi_vac_week_v1)", "sin(pi_vac_week_v1)", varname, varname2),
    # Default: No seasonal covariates if the model type is base or unrecognized.
    NULL
  )
  if (!is.null(seasonal_covars)) covars_mod <- c(covars_mod, seasonal_covars)
  
  # 3. Add Time-to-Vaccination and -to-Immunogenicity Covariates -----------------------------------
  # Based on the dosing scheme, select the relevant time interval covariates. Include the interval 
  # between doses plus the time from last vaccination to immunogenicity. 
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
  
  # 4. Define the Response Variable ----------------------------------------------------------------
  # The response variable is modified based on:
  # - Whether the model is multivariate (indicated by 'multi').
  # - Whether censoring is considered (indicated by 'cens').
  if (outseropos) {
    out_prefix <- "spos_"
    if (!is.null(multi)) {
      response_mod <- glue("imm_spos_s1_{multi}")
    } else {
      response_mod <- "imm_spos_s1"
    }
  } else {
    out_prefix <- ""
    if (!is.null(multi)) {
      response_mod <- if(cens) { glue("imm_val_s1_{multi} | cens(censored_{multi})")
      } else { glue("imm_val_s1_{multi}") }
    } else {
      response_mod <- if(cens) { "imm_val_s1 | cens(censored)"
      } else { glue("imm_val_s1") }
    }
  }
  # 5. Build the Model Formula ---------------------------------------------------------------------
  # Use the reformulate function to combine the response and predictor variables.
  # The intercept is included by default.
  form_mod <- list(reformulate(termlabels = covars_mod, response = response_mod, intercept = TRUE))
  
  if (outseropos) form_mod[[1]] <- bf(form_mod[[1]]) + bernoulli(link = "logit")
  
  # 6. Create a Descriptive Model Name -------------------------------------------------------------
  # The model name is constructed from:
  # - The dose configuration ("v1" for one dose, "vl" for multiple doses).
  # - Data identifier, pathogen, and model type.
  # - Additional suffixes based on the presence of extra variables (varname, varname2).
  doses_nm <- if (doses == "d1") "v1" else "vl"
  model_suffix <- case_when(model_nm %in% c("base", "cs", "cc") ~ "",
                            model_nm %in% c("cs2ni", "cs2i")    ~ glue("_{varname}_{varname2}"),
                            model_nm %in% c("cci", "csni", "csi") ~ glue("_{varname}"),
                            .default = "")
  
  names(form_mod) <- glue("bm{doses_nm}_{data}_{out_prefix}{pathogen}_{model_nm}{model_suffix}")
  
  # Return the named list containing the model formula.
  return(form_mod)
}

####################################################################################################
# Helper function for fun formulas for multipathogen models
# Constructs a combined model formula for multipathogen models by generating individual formulas for 
# each response subtype (as specified in 'multi_pat') using the fun_formulas function, and then 
# merging them into a joint formula suitable for use with brms.
####################################################################################################
fun_formulas_multipathogen <- function(pathogen = "multi_men", 
                                       data = "s08",
                                       doses = "d3", 
                                       model_nm = "base", 
                                       varname = "", 
                                       varname2 = "",
                                       others = "", 
                                       multi_pat = "",
                                       cens = FALSE,
                                       outseropos = FALSE) {
  # Arguments:
  # See fun_formulas
  
  # Output:
  # - form_mod: A named list containing the constructed BRMS model formula.
  
  # 1. Generate Individual Formulas for Each response ----------------------------------------------
  # For each element in 'multi_pat', generate a model formula using fun_formulas.
  # The pathogen name is augmented with the current response using glue().
  bf_formula <- flatten(lapply(multi_pat, function(x) {
    fun_formulas(pathogen = glue("{pathogen}_{x}"), data = data, doses = doses, model_nm = model_nm, 
                 varname = varname, varname2 = varname2, others = others, multi = x, cens = cens,
                 outseropos = outseropos)
  }))
  
  # 2. Combine the Individual Formulas and Set the Family Distribution -----------------------------
  # The individual formulas are wrapped with bf() and combined using the '+' operator.
  # The family distribution is chosen based on the 'cens' flag:
  #   - If censoring is not applied (cens == FALSE), use a Gaussian family with a log link.
  #   - If censoring is applied (cens == TRUE), assume a Gamma distribution.
  if(cens == FALSE) {
    name_cens <- "gauss"
    mbf_formula <- Reduce(`+`, lapply(bf_formula, function(x) bf(x))) + gaussian(link = "log")
  } else {
    name_cens <- "gamma"
    mbf_formula <- Reduce(`+`, lapply(bf_formula, function(x) bf(x))) 
  }
  
  # 3. Construct a Descriptive Model Name ----------------------------------------------------------
  # Generate a model name suffix by modifying the names from the individual formulas.
  # This step cleans up the names by removing subtype markers (multi_pat) and replacing double 
  # underscores, then appending the family distribution indicator.
  model_suffix <- paste0(str_replace(str_remove(names(bf_formula), multi_pat), "__", "_")[[1]],
                         "_", name_cens)
  
  # Wrap the combined formula in a list and assign the generated model name.
  mbf_formula <- list(mbf_formula)
  names(mbf_formula) <- model_suffix
  
  # Return the final multipathogen model formula.
  return(mbf_formula)
}

####################################################################################################
# END
####################################################################################################




