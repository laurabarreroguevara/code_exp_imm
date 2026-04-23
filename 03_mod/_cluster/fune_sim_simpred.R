################################################################################
# Purpose of script: To simulate of the models using posterior predict 
################################################################################

# Setup ------------------------------------------------------------------------
rm(list = ls())
#setwd("projects/_seas_imm")
source("00_pck.R")
dirs$outputs <- glue("outputs/modfit")
dirs$models <- glue("outputs/models")
dirs$predfit <- glue("outputs/simpredfit")

# Simulate data ----------------------------------------------------------------

# Retrieve the data and model names
args_pat <- Sys.getenv("arg_pat")
args_dat <- Sys.getenv("arg_dat")
args_fo <- Sys.getenv("arg_fo")

# Retrieve the data 
dat <- readRDS(file = glue("{dirs$outputs}/{args_dat}"))
# Retrieve the model 
mod <- readRDS(file = glue("{dirs$models}/{args_pat}/{args_fo}.rds"))
# Define cluster according to model name
clu <- str_extract(args_fo, "reg_6|reg_4|reg_3|reg_2")

# Create new data frame --------------------------------------------------------
# (leave only the vaccination week and the clusters)
nd <- dat %>%
  mutate(
    across(where(is.numeric)   & !any_of(c("pi_vac_week_v1", clu)), ~ 0),
    across(where(is.Date)      & !any_of(c("pi_vac_week_v1", clu)), ~ 0), 
    across(where(is.character) & !any_of(c("pi_vac_week_v1", clu)), ~ NA), 
    across(where(is.factor)    & !any_of(c("pi_vac_week_v1", clu)), ~ NA),
    across(where(is.factor)    & any_of(clu), ~ as.factor(as.character(.x)))
  )

# Simulate ---------------------------------------------------------------------
sim <- bake(file = glue("{dirs$predfit}/simpred_{args_fo}.rds"), {
  posterior_predict(mod, newdata = nd, 
                    re_formula = NA, summary = FALSE, ndraws = 100)
})

################################################################################
# End
################################################################################
