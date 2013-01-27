best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states <- unique(data[,"State"])
  if (!is.element(state, states)) stop("invalid state")
  if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  ## Heart Attack : 11
  ## Heart Failure : 17
  ## Pneumonia : 23
  ## eturn hospital name in that state with 30-day death rate
  col <- if (outcome == "heart attack") 11
        else if (outcome == "heart failure") 17
        else if (outcome == "pneumonia") 23
        else stop ("invalid outcome")        
  stateData <- data[data[,"State"] == state, c(2,col)]
  stateData[,2] <- as.numeric(stateData[,2])
  validData <- stateData[!is.na(stateData[,2]),]
  names <- validData[validData[,2] == min(validData[,2]), 1]
  sort(names)[1]
}