# Test waic
rm(list = ls())
source("00_pck.R")
source("fun_ampphi.R")
library(ggridges)
library(tidybayes)
theme_set(theme_classic() + theme(strip.background = element_blank()))
dirs$figures <- "figures"

mod_l <- list()
list.files()
mod_l$base <- glue("{dirs$outputs}/models/bmvl_s08_rota_base.rds")

mod_l <- sapply(list.files(glue("{dirs$outputs}/models/")) %>%
                  str_subset("archiv", negate = TRUE), function(x) {
  readRDS(glue("{dirs$outputs}/models/{x}"))
}, simplify = FALSE)

w <- loo_compare(mod_l$bmvl_s08_rota_base.rds, mod_l$bmvl_s08_rota_cs.rds,
                 mod_l$bmvl_s08_rota_csi_reg_2.rds, mod_l$bmvl_s08_rota_csi_reg_3.rds,
                 mod_l$bmvl_s08_rota_csi_reg_4.rds, mod_l$bmvl_s08_rota_csi_reg_6.rds,
                 criterion = "waic")

waic_l <- lapply(mod_l, waic)

w <- loo_compare(waic_l)

cbind(waic_diff = w[, 1] * -2,
      se        = w[, 2] * 2)
