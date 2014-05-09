pollutantmean <- function(directory, pollutant, id = 1:332) {
  library(stringr)
  #directory="./"
  #pollutant="sulfate"
  #id=1:3
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  if(!file.exists(directory)){
    return(NA)
  }
  pollutantData <- data.frame()
  for (i in id){
    idPadded <- str_pad(i,width=3,pad="0",side="left")
    filename <- paste(directory,paste(idPadded,".csv",sep=""),sep="/")
    pollutantData <- rbind(pollutantData,read.csv(filename,header=TRUE))
  }
  mean(pollutantData[pollutant][,],na.rm=TRUE)
  #pollutantData <- subset(pollutantData,(!is.na(ID) & !is.na(pollutantData[pollutant])))
  #pollutantById  <- split(pollutantData[pollutant][,],pollutantData$ID)
  #lapply(pollutantById,mean)
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
}