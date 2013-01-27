complete <- function(directory, id=1:332) {
  result <- matrix(0, length(id), 2)
  count <- 1
  for (id_ in id) {
    data <- getmonitor(id_, directory)
    result[count, 1] <- id_
    data <- data[!is.na(data[,"sulfate"]),]
    data <- data[!is.na(data[,"nitrate"]),]
    result[count, 2] <- nrow(data)
    count <- count + 1
  }
  colnames(result) <- c("id", "nobs")
  as.data.frame(result)
}