###############################################################################
## Coursera R Programming                                                    ##
## Programming Assignment #3                                                 ##
## Wayne Heller                                                              ##
## 12/31/2016                                                                ##
###############################################################################

## The rankall function takes 2 arguments: 
# an outcome name, and the rank of the hosiptal within the state.  
# The rank may be a number or 'best' or 'worst'
# The function reads the outcome-of-care-measures.csv and returns a 
# character vector with the name of the hospital and the state abbreviation
# that has the specified ranking of 30-day mortality for the specified outcome
# in that state. 
# The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
# Hospitals that do not have data on a particular
# outcome are excluded from the set of hospitals when deciding the rankings.
# Handling ties: If there is a tie for the best hospital for a given outcome, 
# then the hospital names are sorted in alphabetical order and the 
# first hospital in that set is chosen. 
# This function returns a data.frame with columns 'hospital' and 'state'

rankall <- function(outcome, num = 'best') {
  
  ## Read outcome data
  # setting stringsAsFactors to false is necessary; otherwise, the output will
  # include a summary of the levels of the hospital.name factor
  outcomeDf <- read.csv("outcome-of-care-measures.csv", 
                        na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Get list of valid states
  statesDf <- unique(df['State'])
  
  # sort the data.frame by state abbreviation so the output matches expected
  # NOTE: this operation converts this data.frame to a factor
  statesDf <- statesDf[order(statesDf$State),]
  
  
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
  
  
  ## Return a data.frame with hospital and state in that state with 30-day 
  ## death rate matching rank
  
  # map the valid outcomes to their column index in the data file
  valid_outcome_colidx <- c("heart attack" = 11, "heart failure" = 17, 
                            "pneumonia" = 23)
  
  # create data.frame with just the columns necessary for the analysis
  # Hospital Names is col index : 2
  # State is col index : 7
  # Outcome is col index ; valid_outcome_colidx[outcome]
  analysisDf <- outcomeDf[ , c(2, 7, valid_outcome_colidx[outcome])]
  
  # rename the columns for convenience
  names(analysisDf) <- c("Hospital", "State", "Outcome")
  
  # drop the NA's
  analysisDf <- analysisDf[complete.cases(analysisDf),]
  
  # initialize 2 character vectors to store the interim results
  hospitals <- vector(mode = "character")
  states <- vector(mode = "character")
  
  # loop through all the states and populate the two vectors with the matching
  # hospital for each state or an NA for the state if there is no matching
  # hospital
  for (state in statesDf) {
    
    # select rows for the chosen state
    resultsDf <- analysisDf[analysisDf$State == state, ]
  
    # sort the data.frame by outcome, then by hospital name
    resultsDf <- resultsDf[order(resultsDf$Outcome, resultsDf$Hospital),]
  
    # determine which row index to return
    if (num == 'best') {
      rowidx = 1 }
    else if (num == 'worst') {
      rowidx = nrow(resultsDf) }
    else {
      rowidx = as.integer(num)
    }
  
    # return the hospital on the sorted list corresponding to the specified rank
    # if the rowidx is beyond the end of the list, return na
    if (rowidx > nrow(resultsDf)){
      hospitals <- c(hospitals, NA)
      states <- c(states, state)
    }
    else {
      hospitals <- c(hospitals, resultsDf$Hospital[rowidx])
      states <- c(states, state)
    }
  }
  # create the data.frame to be returned using the two interim vectors
  resultsDF <- data.frame(hospital = hospitals, state = states)
  resultsDF
}

