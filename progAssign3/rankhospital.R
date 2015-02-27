rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
  ## Check that state and outcome are valid
  if(!state %in% data$State) {
    stop('invalid state')
  }

  stateData <- data[data[, "State"]==state, ]
  if (outcome == 'heart attack') {
    cindex = 11
  } else if (outcome == 'heart failure') {
    cindex = 17
  } else if (outcome == 'pneumonia') {
    cindex = 23
  } else {
    stop ("invalid outcome")
  }
  
  stateData[, cindex] <- as.numeric(x=stateData[, cindex])
  stateData <- stateData [complete.cases(stateData), ]
  
  ## Return hospital name in that state with given rank
  rindex = 1
  if (num == 'best') {
    rindex = 1
  } else if (num == 'worst') {
    rindex = nrow(stateData)
  } else if (is.numeric(num)) {
    rindex = num
    # print(num)
    if(num < 1 || num > nrow(stateData)) {
      return(NA)
    }
  }
  stateData <- stateData[order(stateData[, cindex], stateData$Hospital.Name), ]
  
  hname <- stateData[rindex, ]$Hospital.Name
  hname

  
}
