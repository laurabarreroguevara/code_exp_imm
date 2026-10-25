####################################################################################################
# Functions to obtain vaccine seroprotection new data 
####################################################################################################

fun_venewdat <- function(dat_i, cov_i, clu_name) {
  clu_vals <- unique(as.character(dat_i[[clu_name]]))
  
  grid_args <- list(vac_week_v1 = 1:52, sex = c("M", "F"), time_vl_s1 = cov_i$time_vl_s1_fixed[[1]])
  
  age_grid <- case_when(
    cov_i$age_fixed[[1]] == 8 ~ list(c(6, 7, 8)),
    cov_i$age_fixed[[1]] == 52 ~ list(c(38, 45, 52)),
    .default = list(cov_i$age_fixed[[1]]))[[1]]
    
  grid_args <- list(vac_week_v1 = 1:52, sex = c("M", "F"), time_vl_s1 = cov_i$time_vl_s1_fixed[[1]],
                    age = age_grid)
  
  if (clu_name != "cs") grid_args[[clu_name]] <- clu_vals
  
  fun_addgridvalue <- function(grid_args, nm, value) {
    if (!all(is.na(value))) grid_args[[nm]] <- value
    grid_args
  }
  
  grid_args <- fun_addgridvalue(grid_args, "time_v1_v2", cov_i$time_v1_v2_fixed[[1]])
  grid_args <- fun_addgridvalue(grid_args, "time_v2_v3", cov_i$time_v2_v3_fixed[[1]])
  grid_args <- fun_addgridvalue(grid_args, "vac_conc", cov_i$vac_conc[[1]])
  grid_args <- fun_addgridvalue(grid_args, "vac_name_v1", cov_i$vac_name_v1[[1]])
  grid_args <- fun_addgridvalue(grid_args, "doses", cov_i$doses[[1]])
  
  expand_grid(!!!grid_args) %>%
    mutate(pi_vac_week_v1 = 2 * pi * vac_week_v1 / 52)
}

####################################################################################################
# END
####################################################################################################