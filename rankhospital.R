###############################################################################
## Coursera R Programming                                                    ##
## Programming Assignment #3                                                 ##
## Wayne Heller                                                              ##
## 12/31/2016                                                                ##
###############################################################################

## The rankhospital function take 3 arguments: 
# the 2-character abbreviated name of a state, an outcome name, and the rank
# of the hosiptal within the state.  The rank may be a number or 'best' or 'worst'
# The function reads the outcome-of-care-measures.csv le and returns a character vector
# with the name of the hospital that has the specified ranking of 30-day mortality for the specified outcome
# in that state. The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
# Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set is be chosen 


rankhospital <- function(state, outcome, num = 'best') {
  
  ## Read outcome data
  outcomeDf <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Get list of valid states
  statesDf <- unique(df['State'])
  
  # validate state abbreviation
  if (!(state %in% statesDf$State)) {
    stop("invalid state")
  }
  
  # validate outcome
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  # validate num (ranking)
  if (!(num %in% c('best', 'worst'))) {
    if (!(is.numeric(num))) {
      stop("invalid rank")
    }
  }
  
  
  ## Return hospital name in that state with 30-day death rate matching rank
  # map the valid outcomes to their column index in the data file
  valid_outcome_colidx <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  # create data.frame with just the columns necessary for the analysis
  # Hospital Names is col index : 2
  # State is col index : 7
  # Outcome is col index ; valid_outcome_colidx[outcome]
  analysisDf <- outcomeDf[ , c(2, 7, valid_outcome_colidx[outcome])]
  
  # rename the columns for convenience
  names(analysisDf) <- c("Hospital", "State", "Outcome")
  
  # drop the NA's
  analysisDf <- analysisDf[complete.cases(analysisDf),]
  
  # select rows for the chosen state
  analysisDf <- analysisDf[analysisDf$State == state, ]
  
  # sort the data.frame by outcome, then by hospital name
  analysisDf <- analysisDf[order(analysisDf$Outcome, analysisDf$Hospital),]
  
  # determine which row index to return
  if (num == 'best') {
    rowidx = 1 }
  else if (num == 'worst') {
    rowidx = nrow(analysisDf) }
  else {
    rowidx = as.integer(num)
  }
  
  # return the hospital on the sorted list corresponding to the specified rank
  # if the rowidx is beyond the end of the list, return na
  if (rowidx > nrow(analysisDf)){
    return(NA)
  }
  else {
    analysisDf$Hospital[rowidx]
  }
}


