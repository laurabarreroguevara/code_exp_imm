####################################################################################################
# Function to set nice upper, and lower limits in ggplot
####################################################################################################
fun_limits <- function(x) {
  if (max(x) > 2) {
    if (((ceiling(max(x)) - floor(min(x))) %% 2) == 0) { # Check if even or odd
      c(floor(min(x)), ceiling(max(x)))
    } else {
      c(floor(min(x)), ceiling(max(x)+1))
    }
  }  else {
    if (((ceiling(max(x*10)) - floor(min(x*10))) %% 2) == 0) { # Check if even or odd
      c(floor(min(x*10)), ceiling(max(x*10)))/10
    } else {
      c(floor(min(x*10)), ceiling(max(x*10)+1))/10
    }
  }
}
####################################################################################################
# End
####################################################################################################