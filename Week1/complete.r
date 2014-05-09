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
  library(stringr)
  #directory="./"
  #pollutant="sulfate"
  #id=1:3
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  pollutantResult <- data.frame()
  for (i in id){
    idPadded <- str_pad(i,width=3,pad="0",side="left")
    filename <- paste(directory,paste(idPadded,".csv",sep=""),sep="/")
    monitorData <- read.csv(filename,header=TRUE)
    numCompletedObs  <- nrow(na.omit(monitorData))
    pollutantResult <- rbind(pollutantResult,c(id=i,nobs=numCompletedObs))
  }
  names(pollutantResult) <- c("id","nobs")
  pollutantResult
}