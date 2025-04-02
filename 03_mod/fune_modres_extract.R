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

# Consider the csi models
list.files(glue("{dirs$outputs}/models"))

rhats_l <- list()
for (pat in list.files(glue("{dirs$outputs}/models"))) {
    for (mod_f in str_subset(list.files(glue("{dirs$outputs}/models/{pat}")),
    "archiv|climate|withre|noconverge", negate = TRUE)) {
        rhats_l[[mod_f]] <- pomp::bake(
            file = glue("{dirs$outputs}/modres_rhats/rhats_{mod_f}"), {

        # Read the model -------------------------------------------------------
        mod <- readRDS(glue("{dirs$outputs}/models/{pat}/{mod_f}"))

        # Extract summary and save it ------------------------------------------
        mod_coef <- summary(mod)
        saveRDS(mod_coef, glue("{dirs$outputs}/modres_coef/coef_{mod_f}"))

        # Extract rhats and return them ----------------------------------------
        mod_rhats <- mod_coef$fixed %>%
            bind_rows(mod_coef$spec_pars)
        for (ran in seq_len(length(names(mod_coef$random)))) {
        mod_rhats <- mod_rhats %>%
            bind_rows(mod_coef$random[[ran]] %>%
            ## Fix the row names
            rownames_to_column("rowname") %>%
            mutate(
                rowname = paste0(names(mod_coef$random)[ran], "_", rowname)) %>%
            column_to_rownames("rowname"))
        }
        mod_rhats <- mod_rhats %>%
            rownames_to_column("coefficient") %>%
            mutate(mod = mod_f) %>%
            select(mod, everything())

        # Extract waics and save them ------------------------------------------
        mod_waic <- data.frame(waic(mod)$estimates) %>%
            rownames_to_column("waic_name") %>%
            pivot_wider(names_from = waic_name, values_from = c(Estimate, SE),
                names_vary = "slowest") %>%
            mutate(mod = mod_f) %>%
            select(mod, everything())
        ## Save it
        saveRDS(mod_waic, glue("{dirs$outputs}/modres_waic/waic_{mod_f}"))

        # Extract conditional effects and save them ----------------------------
        ## Conditional effects for cs and csi models only
        if (str_detect(mod_f, "cs.rds|cs_gauss.rds|csi")) {
            ## Create variables vector
            var_vector <- c()
            ## Define the name of the cluster (if csi model)
            if (str_detect(mod_f, "csi")) {
                clu_name <- paste0(":", 
                str_extract(mod_f, "(?<=csi_).*(?=\\.rds)"))
            } else if (str_detect(mod_f, "cs.rds|cs_gauss.rds")) {
                clu_name <- ""
            }
            ## Extract conditional effects
            ## Conditional effects for MVN
            if (str_detect(mod_f, "multi")) {
                clu_name <- str_remove_all(clu_name, "_gauss")
                mod_ce <- lapply(conditional_effects(mod, resolution = 52,
                                    effects = glue("pi_vac_week_v1{clu_name}")),
                as.data.frame) %>%
                    bind_rows(.id = "imm_name") %>%
                    mutate(imm_name = str_extract(
                        imm_name, "(?<=\\.immvals1anti).+?(?=_pi)"))
                ### Attach imm_name to group vector
                var_vector <- c(var_vector, "imm_name")
            ## Conditional effects for UVN
            } else {
                mod_ce <- conditional_effects(mod, resolution = 52,
                    effects = glue("pi_vac_week_v1{clu_name}"))[[1]]
            }
            ## Select only necessary columns
            if (str_detect(clu_name, "reg")) {
                var_vector <- c(var_vector, str_remove_all(clu_name, ":"))
            }
            mod_ce <- select(mod_ce, pi_vac_week_v1, estimate__, se__,
                            lower__, upper__, all_of(var_vector))
            ## Save it
            saveRDS(mod_ce, glue("{dirs$outputs}/modres_ceffects/ce_{mod_f}"))
        }

        # Extract spaguetti conditional effects and save them ------------------
        ## Conditional effects for cs and csi models only
        if (str_detect(mod_f, "cs.rds|cs_gauss.rds|csi")) {
            ## Define the name of the cluster (if csi model)
            if (str_detect(mod_f, "csi")) {
                pref_cn <- ":"
                clu_name <- str_extract(mod_f, "(?<=csi_).*(?=\\.rds)")
                clu_name <- str_remove_all(clu_name, "_gauss")
            } else if (str_detect(mod_f, "cs.rds|cs_gauss.rds")) {
                pref_cn <- ""
                clu_name <- ""
            }
            ## Extract spaguetti conditional effects
            mod_spa <- conditional_effects(mod, resolution = 52,
                effects = glue("pi_vac_week_v1{pref_cn}{clu_name}"),
                spaghetti = TRUE, ndraws = 100, re_formula = NULL)
            ## Spaghetti 
            mod_spa <- sapply(mod_spa, function(subx) {
                attr(subx, "spaghetti") %>%
                group_by(all_of(clu_name), sample__) %>%
                mutate(est_norm = estimate__/mean(estimate__, na.rm = TRUE)) %>%
                ungroup()
            }, simplify = FALSE) %>%
            bind_rows(.id = "imm_name")
            ## Save it
            saveRDS(mod_spa, glue("{dirs$outputs}/modres_cespa/cespa_{mod_f}"))
        }

        # Extract posterior predictions and save them --------------------------
        pred <- posterior_predict(mod, ndraws = 100)
        ## Save it
        saveRDS(pred, glue("{dirs$outputs}/modres_pred/pred_{mod_f}"))

        # Extract posterior samples and save them ------------------------------
        set.seed(100)
        mod_sam <- tidy_draws(mod) %>%
            sample_n(1000) %>%
            select(starts_with("b_"), starts_with("sd_"), contains("shape"))
        ## Save it
        saveRDS(mod_sam, glue("{dirs$outputs}/modres_samples/samples_{mod_f}"))

        # ----------------------------------------------------------------------
        return(mod_rhats)
        })
    }
}

# bind_rows(rhats_l) %>%
#  group_by(mod) %>%
#  filter(Rhat == max(Rhat)) %>%
#  mutate(Rhat = round(Rhat, digits = 2),
#         Rhat = Rhat == 1) %>%
#  select(mod, Rhat)

################################################################################
# End
################################################################################