rankall <- function (outcome, num = 1) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  
  col <- if (outcome == "heart attack") 11
  else if (outcome == "heart failure") 17
  else if (outcome == "pneumonia") 23
  else stop ("invalid outcome") 
  
  if (num == "best") num = 1

  loop <- function(data, state, num) {
    stateData <- data[data[,"State"] == state, c(2,col)]
    stateData[,2] <- as.numeric(stateData[,2])
    validData <- stateData[!is.na(stateData[,2]),]
    
    if (num == "worst") num = nrow(validData)
    if (num > nrow(validData)) NA
    else {
      sortedData <- validData[order(validData[,2], validData[,1]), ]
      sortedData[num, 1]
    }
  }
  
  states <- unique(data[,"State"])
  rall = data.frame(hospital = c(1:length(states)), state = c(1:length(states)), row.names = states, stringsAsFactors = FALSE)
  rall[,2] <- states
  for (i in 1:length(states)) {
    rall[i,1] <- loop(data, states[i], num)
  }
  rall[order(rall[,2]),]
}