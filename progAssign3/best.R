suppressWarnings ("best")
best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
  ## Check that state and outcome are valid
  stateData <- data[data[, "State"]==state, ]
  if (nrow(stateData) == 0) {
    stop("invalid state")
  }
  
  mort <- numeric(0)
  if (outcome == 'heart attack') {
    mort <- as.numeric(stateData[, 11])
    
  } else if (outcome == 'heart failure') {
    mort <- as.numeric(stateData[, 17])
    
  } else if (outcome == 'pneumonia') {
    mort <- as.numeric(stateData[, 23])
    
  } else {
    stop ("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  minMort <- min(mort, na.rm=T)
  minMortIndex <- which (mort==minMort)
  hName <- stateData[minMortIndex, 2]
  hName
}
