####################################################################################################
# Function to add new expression to list making sure it did not exist and the name is not repeated

# This function appends new formulas to an existing list of formulas. It performs
# duplicate checks on both the formula expressions and their associated names.
# If duplicates are detected, the function stops execution with an error message.
####################################################################################################
fun_add_formula <- function(formula_l, new_formula, omit_exp = FALSE) {
  # Arguments:
  # - formula_l: (list) A list of existing formulas.
  # - new_formula: (string) A vector or list of new formulas to be added. 
  # Each element may have an associated name.
  # - omit_exp: (logical) Logical flag indicating whether to ignore duplicate formula expressions.
  
  # Output:
  # - formula_l: (list frame) A list that includes the original formulas along with the new formulas 
  # (if all checks pass).
  
  # When duplicate expressions are NOT allowed (omit_exp is FALSE):
  if(!omit_exp) {
    # Iterate over each new formula to perform duplicate checks.
    for (i in seq_along(new_formula)) {
      # Check if the formula expression already exists in the list.
      if (new_formula[i] %in% formula_l) {
        stop(glue("error: repeated expression: {new_formula[i]}"))
        # Check if the name associated with the formula already exists in the list.
      } 
      else if (names(new_formula[i]) %in% names(formula_l)) {
        stop(glue("error: repeated name: {names(new_formula[i])}"))
      } 
    }
    # Append each new formula to the list once all checks are passed.
    for (i in seq_along(new_formula)) {
      formula_l = c(formula_l,new_formula[i])
    }
  }
  # When duplicate expressions are allowed (omit_exp is TRUE):
  else if (omit_exp == TRUE) {
    for (i in seq_along(new_formula)) {
      # Only check for duplicate names.
      if (names(new_formula[i]) %in% names(formula_l)) {
        stop(glue("error: repeated name: {names(new_formula[i])}"))
      } 
    }
    # Append each new formula to the list once all checks are passed.
    for (i in seq_along(new_formula)) {
      formula_l = c(formula_l,new_formula[i])
    }
  }
  
  # Return the updated list of formulas.
  return (formula_l)
}
####################################################################################################
# End
####################################################################################################