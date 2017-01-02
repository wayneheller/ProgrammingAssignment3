###############################################################################
## Coursera R Programming                                                    ##
## Programming Assignment #3                                                 ##
## Wayne Heller                                                              ##
## 12/31/2016                                                                ##
###############################################################################

## The Best function take two arguments: 
# the 2-character abbreviated name of a state and an outcome name. 
# The function reads the outcome-of-care-measures.csv le and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
# in that state. The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
# Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set is be chosen 


best <- function(state, outcome) {
  
  ## Read outcome data
  outcomeDf <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Get list of valid states
  statesDf <- unique(df['State'])
  
  if (!(state %in% statesDf$State)) {
    stop("invalid state")
  }
  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
## Return hospital name in that state with lowest 30-day death rate
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
  
  # determine the best outcome value
  minOutcome <- min(analysisDf$Outcome)
  
  # create a results data.frame with the hospital(s) with the max outcome
  resultsDf <- analysisDf[analysisDf$Outcome == minOutcome, ]
  
  # sort the results by hospital name alphabetically
  resultsDf <- resultsDf[order(resultsDf$Hospital),]
  
  # choose the first hospital on the sorted list and return it
  bestHospital <- resultsDf$Hospital[1]
  
  bestHospital
  
  
}


