####################################################################################################
# Function to convert GSK date formats to R date formats
####################################################################################################

fun_dgsk_tidydates <- function(x) {
  # Arguments:
  # - x: (string) A date in the GSK format, e.g., "21AUG2006".
  # Output:
  # - rdate: (date) A date converted to R's standard date format "%Y-%m-%d".
  
  rdate <- data.frame(gskdate = x) %>%
    # Extract day, month, and year components from the GSK date string
    mutate(day = str_sub(gskdate, 1, 2),
           month = str_sub(gskdate, 3, 5),
           year = str_sub(gskdate, -4)) %>%
    # Convert month abbreviation to numeric format
    mutate(month = str_to_title(month),
           month = as.character(match(month, month.abb)),
           month = sprintf("%02d", as.numeric(month)),
           # Combine year, month, and day into the R date format
           rdate = as.character(glue("{year}-{month}-{day}"))) %>%
    # Convert the combined string to a Date object
    mutate(rdate = as.Date(rdate, format = "%Y-%m-%d"))
  
  return(rdate$rdate)
}

####################################################################################################
# END
####################################################################################################