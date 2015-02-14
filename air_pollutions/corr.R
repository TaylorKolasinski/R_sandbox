corr <- function(directory, threshold = 0) {
  com <- complete(directory)
  data <- com[com$nobs > threshold, ]
  result <- numeric(0)
  
  for(id in data$id) {
    csv <- readPollutantCsv(directory, id)
    tf <- !is.na(csv$sulfate) & !is.na(csv$nitrate)
    x <- csv[tf, ]  
    
    result <- c(result, cor(x$sulfate, x$nitrate))
  }
  
  result
}

readPollutantCsv <- function(directory, id) {
  read.csv(paste(directory, "/", sprintf("%03d", id), ".csv", sep=''))
}

complete <- function(directory, id = 1:332) {  
  nobs = numeric(0)
  
  for(i in id) {
    csv <- readPollutantCsv(directory, i)
    nobs = c(nobs, sum(!is.na(csv$sulfate) & !is.na(csv$nitrate)))
  }
  
  data.frame(id=id, nobs=nobs)
}