####################################################################################################
# Function to check gaps in a 52 week cycle 
####################################################################################################
fun_checkgaps <- function(x, lim = 25) {
  # Sort the vector
  sorted_x <- sort(x)
  # Compute the differences between consecutive elements
  gaps <- diff(sorted_x)
  # Check the gap between the last element and the first element, considering the circularity (52 back to 1)
  circular_gap <- (52 - max(sorted_x)) + (min(sorted_x) - 1)
  # Check if any gap is larger than 25, including the circular gap
  check <- any(gaps > lim) || circular_gap > lim
}
####################################################################################################
# END
####################################################################################################