count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) stop("cause is NULL")
  ## Check that specific "cause" is allowed; else throw error
  if (!is.element(cause, c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown"))) stop("invalid cause")
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  ## Extract causes of death
  reg <- sprintf("[Cc]ause *: *[%s%s]%s", substr(cause,1,1), toupper(substr(cause,1,1)), substr(cause,2,nchar(cause)))
  ## Return integer containing count of homicides for that cause
  ## print(reg)
  length(grep(reg, homicides))
}