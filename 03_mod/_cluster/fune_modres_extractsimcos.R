################################################################################
# Purpose of script: Check converge of models
################################################################################

rm(list = ls())
library(tidyverse)
library(brms)
library(glue)
library(tidybayes)

dirs <- list()
#dirs$outputs <- "projects/_seas_imm/outputs"
dirs$outputs <- "outputs"
dirs$simcosres <- glue("{dirs$outputs}/simcosres")

# Consider the csi models
list.files(glue("{dirs$outputs}/simcos_models"))

rhats_l <- list()
for (mod_f in list.files(glue("{dirs$outputs}/simcos_models"))) {
  rhats_l[[mod_f]] <- pomp::bake(
    file = glue("{dirs$simcosres}/simcosres_rhats/rhats_{mod_f}"), {
      
      # Read the model -------------------------------------------------------
      mod <- readRDS(glue("{dirs$outputs}/simcos_models/{mod_f}"))
      
      # Extract summary and save it ------------------------------------------
      mod_coef <- summary(mod)
      saveRDS(mod_coef, glue("{dirs$simcosres}/simcosres_coef/coef_{mod_f}"))
      
      # Extract rhats and return them ----------------------------------------
      mod_rhats <- mod_coef$fixed %>%
        rownames_to_column("coefficient") %>%
        mutate(mod = mod_f) %>%
        select(mod, everything())
      
      # Extract posterior samples and save them ------------------------------
      set.seed(100)
      mod_sam <- tidy_draws(mod) %>%
        sample_n(1000) %>%
        select(starts_with("b_"), starts_with("sd_"), contains("shape"))
      ## Save it
      saveRDS(mod_sam, glue("{dirs$simcosres}/simcosres_samples/samples_{mod_f}"))
      
      # ----------------------------------------------------------------------
      return(mod_rhats)
    })
}

################################################################################
# End
################################################################################