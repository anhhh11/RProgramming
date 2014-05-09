#library(data.table)
rankall <- function(outcome,num="best") {
  ##Assig arguments for evaluating easily
  ## Read outcome data
  data  <- read.csv("./outcome-of-care-measures.csv"
                    ,header=TRUE
                    ,na.strings="Not Available"
                    ,stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  allAvailOutcomes  <- c("heart attack","heart failure","pneumonia")
  if (! outcome %in% allAvailOutcomes){
    stop("invalid outcome")
  } 
  ##Find and show right columns
  #grep("^hospital.*30.*day.*mortality.*(heart|pneumonia)",tolower(names(data)),value=TRUE)
  #grep("^hospital.*30.*day.*mortality.*(heart|pneumonia)",tolower(names(data)))
  #outcome <- "heart attack"
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  names(data)[c(11,17,23)]=c("heart attack","heart failure","pneumonia")
  #Convert data[outcome] to numeric 
  data <- subset(data,!is.na(data[outcome]))
  data[,outcome] <- as.numeric(data[,outcome])
  
  #Rename outcome to Rate
  names(data)[which(names(data)==outcome)] <- c("Rate")
  data <- data[order(data["Hospital.Name"],decreasing=FALSE),]
  
  data <- transform(data,
                 Rank=ave(Rate,State,
                          FUN=function(x) rank(x,
                                               ties.method="first",
                                               na.last=FALSE)))
  
  #stateMinMax <- aggregate(data[c("State","Rank")],
  #                         by=list(data$State),FUN=function(x){c(min(x),max(x))})

  if (is.character(num) & num=="best"){
    num  <- 1
  }
  if (is.character(num) & num=="worst"){
    result <- transform(data,
                      Worst=ave(Rank,State
                                ,FUN=function(x) { x == max(x)}))
    result <- subset(result[c("Hospital.Name","State","Worst")],Worst==TRUE)
  } else {
    result <- subset(data[c("Hospital.Name","State","Rank")],Rank == num)
  }
  
  #Filter data by State & having outcome values
  result <- merge(x=unique(data["State"]),y=result,by="State",all.x=TRUE)
  rownames(result) <- result[,"State"]

  #library(RPostgreSQL)
  #?attach
  #detach("package:RPostgreSQL", unload = TRUE)
  #sqldf("select count(*),rank over (partition by 'Rate') from data")
  names(result)[c(1,2)] <- c("state","hospital")
  return(result[,c(1,2)])
}

