best <- function(state, outcome) {
  ## Read outcome data
  dat_outcome<-
    read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  dat_state<-
    read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  
  ## Check that state and outcome are valid
  #   The outcomes can
  #   be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
  #   outcome should be excluded from the set of hospitals when deciding the rankings.
  #   
  
  
  
  ## Return hospital name in that state with lowest 30-day death
  
  ## rate
  
}