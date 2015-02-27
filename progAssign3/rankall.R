rankhospital <- function(stateData, cindex, num = "best") {
  sortedData <- stateData[order(stateData[cindex], stateData$Hospital.Name, na.last=NA), ]
  ## Return hospital name in that state with given rank
  rindex = 1
  if (num == 'best') {
    rindex = 1
  } else if (num == 'worst') {
    rindex = nrow(sortedData)
  } else if (is.numeric(num)) {
    rindex = num
    # print(num)
    if(num < 1 || num > nrow(stateData)) {
      return(NA)
    }
  }
 
  sortedData[rindex, 2]
}


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (outcome == 'heart attack') {
    cindex = 11
  } else if (outcome == 'heart failure') {
    cindex = 17
  } else if (outcome == 'pneumonia') {
    cindex = 23
  } else {
    stop ("invalid outcome")
  }
  
  data[, cindex] <- as.numeric (data[, cindex])
  sdata <- split (data, data$State)
  hnames <- lapply (sdata, rankhospital, cindex, num)
  data.frame (hospital=unlist(hnames), state=names(hnames), row.names = names(hnames))
  
}
