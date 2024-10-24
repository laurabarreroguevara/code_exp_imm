# Add new expression to list making sure it did not exist and the name is not repeated
add_new_expression = function(l_expressions,new_expressions, omit_exp = FALSE) {
  if(omit_exp == FALSE) {
    for (i in 1:length(new_expressions)) {
      if (new_expressions[i] %in% l_expressions) {stop(glue("error: repeated expression: {new_expressions[i]}"))} 
      else if (names(new_expressions[i]) %in% names(l_expressions)) {stop(glue("error: repeated name: {names(new_expressions[i])}"))} 
    }
    for (i in 1:length(new_expressions)) {
      l_expressions = c(l_expressions,new_expressions[i])
    }
  }
  else if (omit_exp == TRUE) {
    for (i in 1:length(new_expressions)) {
      if (names(new_expressions[i]) %in% names(l_expressions)) {stop(glue("error: repeated name: {names(new_expressions[i])}"))} 
    }
    for (i in 1:length(new_expressions)) {
      l_expressions = c(l_expressions,new_expressions[i])
    }
  }
  return (l_expressions)
}