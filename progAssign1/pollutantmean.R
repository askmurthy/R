pollutantmean <- function(directory, pollutant, id = 1:332) {
  
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'pollutant' is a character vector of length 1 indicatingv
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    a = numeric ()
    for (i in id) {
        mdata = getmonitor (i, directory)
        pdata = mdata[ , pollutant]
        a = c (a, pdata)
    }
  
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    mean(a, na.rm=TRUE);
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
