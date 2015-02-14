pollutantmean <- function(directory, pollutant, id = 1:332) {
  datalist <- multmerge(directory, id)
  pollutant_data <- datalist[pollutant]
  mean(pollutant_data[!is.na(pollutant_data)])
}

multmerge <- function(the_path, id) {
  filenames <- list.files(path = the_path, full.names = TRUE)
  for (file in filenames[id]) {    
    if (!exists("dataset")) {
      dataset <- read.table(file, header = TRUE, sep = ",")
    }
    
    if (exists("dataset")) {
      temp_dataset <- read.table(file, header=TRUE, sep = ",")
      dataset <- rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  dataset
}