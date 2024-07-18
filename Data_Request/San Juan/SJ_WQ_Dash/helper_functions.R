

# Function to evaluate formulas
evaluate_formula <- function(formula, hardness) {
  if (is.na(formula) || is.na(hardness)) return(NA)
  eval(parse(text = str_replace_all(formula,"hardness",as.character(hardness))))
}

#flag if needs hardness but doesnt have it:
depends_on_hardness <- function(formula) {
  if (is.na(formula)) return(FALSE)
  return(str_detect(formula, "hardness"))
}
