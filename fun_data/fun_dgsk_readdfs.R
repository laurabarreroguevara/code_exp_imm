####################################################################################################
# Function to read all or selected data frames from GSK Studies 
####################################################################################################

fun_dgsk_readdfs <- function(study, info_files = FALSE, cdi = FALSE) {
  # Arguments:
  # - study: (named vector) The study ID (name) and the corresponding folder classification (string).
  # - info_files: (logical) Whether to also read the information files (for experimental mode).
  # - cdi: (logical) Indicates whether the study data is in CDI format (common in newer studies).
  # Output:
  # - files_l: (named list of data frames) A list containing all or selected data frames from the 
  # study.

  # Read RDS data frames from the study folder -----------------------------------------------------
  files_dat <- sapply(str_subset(
    list.files(glue("{dirs$server_bg}/{study}/{names(study)}"), full.names = TRUE), "rds"),
    function(x) {
      readRDS(x) %>%
        mutate(across(.cols = everything(), .fns = as.character)) %>%
        rename_with(tolower) 
      }, simplify = FALSE) 
  names(files_dat) <- str_remove(names(files_dat), 
                                 glue("{dirs$server_bg}/{study}/{names(study)}/"))
  
  # Select files to return -------------------------------------------------------------------------
  
  ## Return all files (experimental mode)
  if(info_files == TRUE) {
    print("Reading all files")
    return(files_dat)
  
    ## Return only selected files
  } else if (info_files == FALSE) {
    # Filter out information data frames -------------------------------------------------
    files_dat <- files_dat[str_detect(names(files_dat), "info", negate = T)]
    
    # Define data frames to select based on CDI format -----------------------------------
    if(cdi == FALSE) { toselect <- c("pid", "demo", "dates", "expo", "sero") } else if(cdi == TRUE) {
      toselect <- c("adsl", "adex.rds", "adis")}
    # Select the relevant data frames and ensure all columns are character type ----------
    files_l <- sapply(toselect, function(x) {
      if(length(str_subset(names(files_dat), x)) == 1) { files_dat[[str_subset(names(files_dat), x)]]
      } else {c()}
    }, simplify = FALSE)
    # Rename selected data frames if in CDI format ---------------------------------------
    if(cdi == TRUE) names(files_l) <- c("pid", "expo", "sero")

    print(paste("Reading", study, names(study), "selected files. Missings:", 
                paste(names(files_l)[lapply(files_l, length) == 0], collapse = ", ")))
    return(files_l)
  }
}

####################################################################################################
# END
####################################################################################################
