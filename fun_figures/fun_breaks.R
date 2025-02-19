####################################################################################################
# Function to set nice upper, middle, and lower breaks in ggplot
####################################################################################################
fun_breaks <- function(x) {
  if (max(x) > 2) {
    if (((ceiling(max(x)) - floor(min(x))) %% 2) == 0) { # Check if even or odd
      seq(floor(min(x)), ceiling(max(x)), length.out = 3)
    } else {
      seq(floor(min(x)), ceiling(max(x)+1), length.out = 3)
    }
  } else {
    if (((ceiling(max(x*10)) - floor(min(x*10))) %% 2) == 0) { # Check if even or odd
      seq(floor(min(x*10)), ceiling(max(x*10)), length.out = 3)/10
    } else {
      seq(floor(min(x*10)), ceiling(max(x*10)+1), length.out = 3)/10
    }
  }
}
####################################################################################################
# End
####################################################################################################
