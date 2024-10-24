####################################################################################################
# Purpose of script: To compare models to all the studies
####################################################################################################

# Setup --------------------------------------------------------------------------------------------
rm(list = ls())
library(parallel)
source("00_pck.R")

n_cores <- detectCores() - 1

# Retrieve pathogen from the command line arguments
args_pat <- Sys.getenv("arg_pat")
args_df <- Sys.getenv("arg_df")
dirs$outputs <- glue("outputs/{args_pat}")

# Retrieve models ----------------------------------------------------------------------------------
mod_names <- list.files(glue("{dirs$outputs}")) %>%
  str_subset("rds") %>%
  str_subset("data", negate = TRUE) %>%
  str_subset("s08")

if(args_df == "") {
  mod_names <- str_subset(mod_names, "dfkgc", negate = TRUE)
  args_df_name <- ""
} else {
  mod_names <- str_subset(mod_names, args_df)
  args_df_name <- glue("_{args_df}")
  }

# Base model
mod_base <- readRDS(glue("{dirs$outputs}/{str_subset(mod_names, 'base')}"))
# CS model
mod_cs <- readRDS(glue("{dirs$outputs}/{str_subset(mod_names, '_cs.rds|_cs_')}"))
# Photoperiod models 
mod_csi_l <- sapply(str_subset(mod_names, "csi"), function(x) {
  readRDS(file = glue("{dirs$outputs}/{x}"))}, 
  simplify = FALSE)
mod_csni_l <- sapply(str_subset(mod_names, "csni"), function(x) {
  readRDS(file = glue("{dirs$outputs}/{x}"))}, 
  simplify = FALSE)
mod_cs2i_l <- sapply(str_subset(mod_names, "cs2i"), function(x) {
  readRDS(file = glue("{dirs$outputs}/{x}"))}, 
  simplify = FALSE)

# Run the comparisons and save them ----------------------------------------------------------------
# Compare cs from base
mcom_cs_base <- bake(file = glue("{dirs$outputs}/mcom_{args_pat}_cs_base{args_df_name}.rds"), {
  mclapply(1:10, function(x) {
    bayes_factor(mod_cs, mod_base)},
    mc.cores = n_cores)
})

# Compare csni from base
mcom_csni_base <- bake(file = glue("{dirs$outputs}/mcom_{args_pat}_csni_base{args_df_name}.rds"), {
  mcom_csni_base <- list()
  for(mod in names(mod_csni_l)) {
    mcom_csni_base[[mod]] <- mclapply(1:10, function(x) {
      bayes_factor(mod_csni_l[[mod]], mod_base)},
      mc.cores = n_cores)
  }
  return(mcom_csni_base)
})

# Compare csi from cs
mcom_csi_cs <- bake(file = glue("{dirs$outputs}/mcom_{args_pat}_csi_cs{args_df_name}.rds"), {
  mcom_csi_cs <- list()
  for(mod in names(mod_csi_l)) {
    mcom_csi_cs[[mod]] <- mclapply(1:10, function(x) {
      bayes_factor(mod_csi_l[[mod]], mod_cs)},
      mc.cores = n_cores)
  }
  return(mcom_csi_cs)
})

# Compare csi from csni
mcom_csi_csni <- bake(file = glue("{dirs$outputs}/mcom_{args_pat}_csi_csni{args_df_name}.rds"), {
  mcom_csi_csni <- list()
  for(mod in names(mod_csi_l)) {
    mod_csni <- str_replace(mod, "csi", "csni")
    mcom_csi_csni[[mod]] <- mclapply(1:10, function(x) {
      bayes_factor(mod_csi_l[[mod]], mod_csni_l[[mod_csni]])},
      mc.cores = n_cores)
  }
  return(mcom_csi_csni)
})

# Compare csi from base
mcom_csi_base <- bake(file = glue("{dirs$outputs}/mcom_{args_pat}_csi_base{args_df_name}.rds"), {
  mcom_csi_base <- list()
  for(mod in names(mod_csi_l)) {
    mcom_csi_base[[mod]] <- mclapply(1:10, function(x) {
      bayes_factor(mod_csi_l[[mod]], mod_base)},
      mc.cores = n_cores)
  }
  return(mcom_csi_base)
})

# Compare cs2i from csi
mcom_cs2i_csi <- bake(file = glue("{dirs$outputs}/mcom_{args_pat}_cs2i_csi{args_df_name}.rds"), {
  mcom_cs2i_csi <- list()
  for(mod in names(mod_cs2i_l)) {
    mod_csi <- str_replace(mod, "cs2i", "csi") %>%
      str_remove("kgc_1_|kgc_2_|kgc_3_")
    mcom_cs2i_csi[[mod]] <- mclapply(1:10, function(x) {
      bayes_factor(mod_cs2i_l[[mod]], mod_csi_l[[mod_csi]])},
      mc.cores = n_cores)
  }
  return(mcom_cs2i_csi)
})

####################################################################################################
# End
####################################################################################################
