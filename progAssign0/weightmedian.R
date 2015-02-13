weightmedian <- function(directory, day)  {
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  dat <- data.frame()                             #creates an empty data frame
  for (i in 1:5) {                                
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  dat_subset <- dat[which(dat[, "Day"] == day),]  #subsets the rows that match the 'day' argument
  median(dat_subset[, "Weight"], na.rm=TRUE)      #identifies the median weight 
  #while stripping out the NAs
}

weightmedian2 <- function(directory, day)  {
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  tmp <- vector(mode = "list", length = length(files_list))
  for (i in seq_along(files_list)) {
    tmp[[i]] <- read.csv(files_list[[i]])
  }
  dat <- do.call(rbind, tmp)
  dat_subset <- dat[which(dat[, "Day"] == day),] 
  median(dat_subset[, "Weight"], na.rm=TRUE)      #identifies the median weight 
  #while stripping out the NAs
}