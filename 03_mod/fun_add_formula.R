# Add new expression to list making sure it did not exist and the name is not repeated
fun_add_formula <- function(formula_l, new_formula, omit_exp = FALSE) {
  if(omit_exp == FALSE) {
    for (i in 1:length(new_formula)) {
      if (new_formula[i] %in% formula_l) {stop(glue("error: repeated expression: {new_formula[i]}"))} 
      else if (names(new_formula[i]) %in% names(formula_l)) {stop(glue("error: repeated name: {names(new_formula[i])}"))} 
    }
    for (i in 1:length(new_formula)) {
      formula_l = c(formula_l,new_formula[i])
    }
  }
  else if (omit_exp == TRUE) {
    for (i in 1:length(new_formula)) {
      if (names(new_formula[i]) %in% names(formula_l)) {stop(glue("error: repeated name: {names(new_formula[i])}"))} 
    }
    for (i in 1:length(new_formula)) {
      formula_l = c(formula_l,new_formula[i])
    }
  }
  return (formula_l)
}