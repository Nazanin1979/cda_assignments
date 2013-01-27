agecount <- function(age = NULL) {
  if (is.null(age)) stop("age should not be NULL")
  reg <- sprintf("[^0-9]%d *years *old", age)
  homicides <- readLines("homicides.txt")
  length(grep(reg, homicides))
}