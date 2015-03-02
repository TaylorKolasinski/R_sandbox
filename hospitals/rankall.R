rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")

  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}

  validState = sort(unique(data[,7]))

  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]

  hospital<-character(0)

  for (i in seq_along(validState)) {
    data.state <- data[data$State==validState[i],]

    sorted.data.state <- data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]

    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(sorted.data.state)

    hospital[i] <- sorted.data.state[this.num,"Hospital.Name"]
  }

  data.frame(hospital=hospital,state=validState,row.names=validState)
}
