complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
  
  
    nobs = numeric()
    for (i in id) {
        mdata = getmonitor (i, directory)
        n = nrow(mdata[complete.cases(mdata),])
        nobs = c (nobs, n)    
    }
    df = data.frame (id, nobs)
    return (df)
}

getmonitor <- function(id, directory, summarize = FALSE) {
  te1 <- formatC(id, width=3, flag="0")
  filename = paste(directory, te1, sep = "/")
  filename1 = paste(filename, "csv", sep = ".")
  test <- read.table(file = filename1, header=T, sep=",")
  if(summarize) {
    print(summary(test))
    return (test)
  } else {
    return (test)
  }
}