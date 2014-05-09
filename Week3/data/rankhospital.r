#library(data.table)
rankhospital <- function(state, outcome,num="best") {
  ##Assig arguments for evaluating easily
  ## Read outcome data
  data  <- read.csv("./outcome-of-care-measures.csv"
                    ,header=TRUE
                    ,na.strings="Not Available"
                    ,stringsAsFactors=FALSE)
  data_State  <- data[,"State"]
  
  ## Check that state and outcome are valid
  allAvailOutcomes  <- c("heart attack","heart failure","pneumonia")
  if (! outcome %in% allAvailOutcomes){
    stop("invalid outcome")
  }
  if (!state %in% data_State){
    stop("invalid state")
  }
  
  ##Find and show right columns
  #grep("^hospital.*30.*day.*mortality.*(heart|pneumonia)",tolower(names(data)),value=TRUE)
  #grep("^hospital.*30.*day.*mortality.*(heart|pneumonia)",tolower(names(data)))
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  names(data)[c(11,17,23)]=c("heart attack","heart failure","pneumonia")
  #Convert data[outcome] to numeric 
  data[,outcome] <- as.numeric(data[,outcome])
  
  #Rename outcome to Rate
  names(data)[which(names(data)==outcome)] <- c("Rate")
  
  #Filter data by State & having outcome values
  data <- data[data_State==state & !is.na(data[,"Rate"]),]
  data <- data[order(data[,"Hospital.Name"],decreasing=FALSE),]
  data[,"Rank"] <- rank(data[,"Rate"],ties.method="first")
  if (is.numeric(num)){
    result <- data[data[,"Rank"]==num,"Hospital.Name"]  
  }
  else if(is.character(num)){
    if (num=="best") 
      result <- data[data[,"Rank"]==1,"Hospital.Name"] 
    else if (num=="worst") 
      result <- data[data[,"Rank"]==max(data[,"Rank"])
                                ,"Hospital.Name"]
  }
  if (length(result)==0)
    return(NA)
  return(result)
}


#data.table version
#data  <- fread("./outcome-of-care-measures.csv",header=TRUE,na.strings="Not Available")
#setnames(data,
#         old=c(11,17,23),
#         c("heart attack","heart failure","pneumonia"))
#data[[outcome]] <- as.numeric(data[[outcome]])
#data <- data[State==state & !is.na(data[[outcome]])]
#data[data[[outcome]]==min(data[[outcome]]),`Hospital Name`]
