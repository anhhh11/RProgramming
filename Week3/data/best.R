library(data.table)
best <- function(state, outcome) {
  ##Assig arguments for evaluating easily
  #state="TX"
  #outcome="heart attack"
  
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

  #Filter data by State & having outcome values
  data <- data[data_State==state & !is.na(data[,outcome]),]
  data[data[,outcome]==min(data[,outcome]),"Hospital.Name"]
  
}


  #data.table version
  #data  <- fread("./outcome-of-care-measures.csv",header=TRUE,na.strings="Not Available")
  #setnames(data,
  #         old=c(11,17,23),
  #         c("heart attack","heart failure","pneumonia"))
  #data[[outcome]] <- as.numeric(data[[outcome]])
  #data <- data[State==state & !is.na(data[[outcome]])]
  #data[data[[outcome]]==min(data[[outcome]]),`Hospital Name`]
