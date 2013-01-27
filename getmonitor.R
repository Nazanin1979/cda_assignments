getmonitor <- function(id, directory, summarize = FALSE) {
  if (typeof(id) == "character") id <- as(id, "numeric")
  fname <- sprintf("%s/%03d.csv", directory, id)
  
  data <- read.csv(fname)
  if (summarize) print(summary(data))
  
  data
}
