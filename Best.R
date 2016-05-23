## best.R function

best <- function(state, outcome) {
  
  ## Read outcome data
  hospitaldata <- read.csv("c:/users/berkpdh/documents/outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  validState = unique(hospitaldata[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name into column name
  ColumnName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- ColumnName[match(outcome,validOutcome)]
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- hospitaldata[hospitaldata$State==state,]
  index <- which.min(as.double(data.state[,colName]))
  data.state[index,"Hospital.Name"]
}