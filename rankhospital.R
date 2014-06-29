
## Return hospital name in that state with the given rank
## 30-day death rate 
rankhospital <- function(state, outcome, num = "best") {

  ## To do
  ## 1. insert comment or replace 
  
  
  
  ## Read outcome data
  dat_outcome<-
    read.csv("outcome-of-care-measures.csv", colClasses = "character") 
#   dat_state<-
#     read.csv("outcome-of-care-measures.csv", colClasses = "character") 
#   
  ## Check that state and outcome are valid
  #   The outcomes can
  #   be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
  #   outcome should be excluded from the set of hospitals when deciding the rankings.
  #   
  if(!(state %in% dat_outcome$State )) stop("invalid state")
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia") )) stop("invalid outcome")
  
  ##
  names_translate_vector=character(0)
  names_translate_vector["heart attack"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  names_translate_vector["heart failure"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"      
  names_translate_vector["pneumonia"] <-  "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"    
  
  field_name <- names_translate_vector[outcome]
## Return hospital name in that state with the given rank
## 30-day death rate
  
  state_outcome <-  # copy only the columns and the rows  that are needed  
       dat_outcome[dat_outcome$State==state & dat_outcome[[field_name]]!="Not Available",
                  c("Hospital.Name",field_name)] 
  colnames(state_outcome)<-c("Hospital.Name","indicator")

  state_outcome[,"indicator"] <- as.numeric(state_outcome[,"indicator"])
  if(is.numeric(num)) {
    rownum<- num
  } else if (num=="best") {
    rownum <-1 
  } else if (num=="worst") {
    rownum <- nrow(state_outcome)
  } else stop("invalid num")
  
  rank_hospitals<- state_outcome[order( state_outcome$indicator, state_outcome$Hospital.Name),]
  rank_hospitals$Hospital.Name [ rownum ]

}
