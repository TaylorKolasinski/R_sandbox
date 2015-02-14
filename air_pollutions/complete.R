complete <- function(directory, id = 1:332) {
  nobs = numeric(0)
  
  filenames <- list.files(path = directory, full.names = TRUE)
  for (file in filenames[id]) {    
    dataset <- read.table(file, header=TRUE, sep = ",")
    nobs <- c(nobs, sum(!is.na(dataset$sulfate) & !is.na(dataset$nitrate)))
  }
  data.frame(id=id, nobs=nobs)
}