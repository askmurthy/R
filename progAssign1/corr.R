corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    files_list <- list.files(directory, full.names=TRUE) 
    monitors <- vector(mode = "list", length = length(files_list))
    for (i in seq_along(files_list)) {
      monitors[[i]] <- read.csv(files_list[[i]])
    }
    # get all complete cases
    monitors <- lapply(monitors, getComplete)
    # trim monitor data to those which meet 'thresold' observation points
    monitors <- lapply (monitors, meetsThreshold, t = threshold)
    # trim away empty daya
    monitors <- monitors[sapply(monitors, function(x) dim(x)[1]) > 0]
    
    # initiliaze empty vector for cor result
    cr <- numeric(0)
    if (length(monitors) > 0) {
       # compute cor of sulphate to nitrate for each monitor
       cr <- sapply (monitors, computeCor)
    }
    
    cr
    #ccForId <- complete (directory)
    #ccForId <- subset (ccForId, nobs>=threshold)
    ## Return a numeric vector of correlations
}

getComplete <- function(x) x[complete.cases(x),]

meetsThreshold <- function(x, t=0) x[nrow(x) > t,]

computeCor <- function (x) cor (x$sulfate, x$nitrate)

