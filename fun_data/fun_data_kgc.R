####################################################################################################
# Function to extract the Köppen-Geiger climate classification (KGC) for a spatial data point.
####################################################################################################

fun_data_kgc <- function(dat = dat_spatial_all) {
  # Arguments:
  # - dat: (data frame) A data frame with at least three columns: location, lon, and lat.
  # Output:
  # - dat_kgc: (data frame) The input data frame with KGC classification.
  
  # Attach necessary conflicting 'kgc' package                 
  library(kgc)
  library(plyr)
  library(dplyr)
  # Round the coordinates to the nearest values suitable for KGC lookup
  data_clim <- data.frame(location = dat$location,
                          rndCoord.lon = kgc::RoundCoordinates(dat$lon, latlong='lon'),
                          rndCoord.lat = kgc::RoundCoordinates(dat$lat, latlong='lat'))
  
  # Extract Köppen-Geiger Climate Classification based on the rounded coordinates
  data_clim <- data.frame(data_clim, ClimateZ = LookupCZ(data_clim))
  
  # Extract the uncertainty associated with the climate classification
  data_clim <- data.frame(data_clim, CZUncertainty(data_clim))
  
  # Detach the 'kgc' package to avoid conflicts with other packages
  detach("package:kgc", unload = TRUE)
  
  # Combine the climate classification data with the original data frame
  dat_kgc <- data.frame(dat, data_clim) %>%
    mutate(ClimateZ = as.character(ClimateZ)) %>%
    # Handle missing climate classification
    mutate(ClimateZ = case_when(
      ClimateZ == "Climate Zone info missing" ~ possible.cz,
      ClimateZ != "Climate Zone info missing" ~ ClimateZ)) %>%
    select(-c(location.1, rndCoord.lon, rndCoord.lat, uncertainty, possible.cz))
  
  # Reminder to restart the R session to avoid potential issues with 'kgc' package
  print("Please close R and open it again.")

  return(dat_kgc)
}

####################################################################################################
# End 
####################################################################################################