corr <- function(directory, threshold = 0,id=1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  completeData <- complete(directory,id)
  dataIdChoosed <- subset(completeData,nobs>threshold)[,"id"]
  corrVector = c()
  for (i in dataIdChoosed){
    idPadded <- str_pad(i,width=3,pad="0",side="left")
    filename <- paste(directory,paste(idPadded,".csv",sep=""),sep="/")
    pollutantData <- read.csv(filename,header=TRUE)
    #pollutantData <- subset(pollutantData,!is.na(sulfate)&!is.na(nitrate))
    corrVector = append(corrVector,cor(pollutantData$nitrate
                                       ,pollutantData$sulfate
                                       ,use="pairwise.complete.obs"))
  }
  corrVector
}