####################################################################################################
# Function to extract first-level administrative divisions of a country and the longitude and 
# latitude coordinates for their capital cities. 
# Data from Statoids http://www.statoids.com and google
####################################################################################################

fun_data_coord1lv <- function(country_url = "http://www.statoids.com/uco.html",
                              country_name = "Colombia",
                              latlon = FALSE,
                              api = "api") {
  # Arguments:
  # - country_url: (string) The URL of the Statoids webpage for the country.
  # - country_name: (string) The name of the country.
  # - latlon: (logical) Whether to extract latitude and longitude coordinates (default is FALSE).
  
  # Load the webpage content
  webpage <- rvest::read_html(country_url)
  # Extract the relevant table from the webpage
  country_div_df <- rvest::html_table(rvest::html_nodes(webpage, "table")[2], trim = T)
  
  # Clean up column names if they are incorrect
  if(colnames(country_div_df[[1]])[[1]] == "X1") {
    colnames(country_div_df[[1]]) <- country_div_df[[1]][1,]
    country_div_df[[1]] <- country_div_df[[1]][-1,]
  }
  # Remove empty columns if they exist
  if("" %in% colnames(country_div_df[[1]])) {
    empty_col <- which(colnames(country_div_df[[1]]) == "")
    country_div_df[[1]] <- country_div_df[[1]] %>%
      select(-all_of(empty_col))
  }
  # Handle cases where the table does not include a "Capital" column
  if ("Capital" %in% colnames(country_div_df[[1]]) == F) {
    country_div_df[[1]] <- country_div_df[[1]] %>%
      # Use the first-level division name as the capital
      rename(level1 = 1) %>%
      mutate(Capital = level1)
  }
  
  # Tidy up the data frame
  country_div_df <- country_div_df[[1]] %>%
    select(1, Capital, contains("km"), matches("population|Population|Pop")) %>%
    rename(level1 = 1,
           city = 2,
           area = 3,
           pop = 4) %>%
    mutate(area = suppressWarnings(as.numeric(str_remove_all(area, ","))),
           pop = suppressWarnings(as.numeric(str_remove_all(pop, ",")))) %>%
    filter(city != "") %>%
    mutate(country = country_name) %>%
    filter(str_detect(level1, "\n", negate = T))
  
  # If latlon is TRUE, retrieve latitude and longitude for each city
  if(latlon == TRUE) {
    # Register Google Maps API key
    ggmap::register_google(api)
  
    # Get latitude and longitude coordinates
    country_div_df <- country_div_df %>%
      # Create query string for geocoding
      mutate(location_query = glue("{city}, {level1}, {country}")) %>%
      # Obtain coordinates
      ggmap::mutate_geocode(location_query) %>%
      select(-location_query)
  }
  
  # Return the 1st level divisions and optionally geocoded data frame
  return(country_div_df)
  
}

####################################################################################################
# END
####################################################################################################
