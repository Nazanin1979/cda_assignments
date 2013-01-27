corr <- function(directory, threshold=0) {
  completed <- complete(directory)
  completed <- completed[completed$nobs > threshold,]
  if (nrow(completed) == 0){
    vector(mode = "numeric")
  } else sapply(completed$id, function(id){
    data <- getmonitor(id, directory)
    data <- data[!is.na(data[,"sulfate"]),]
    data <- data[!is.na(data[,"nitrate"]),]
    cor(data[,c("sulfate", "nitrate")])[[2]]
  })
}