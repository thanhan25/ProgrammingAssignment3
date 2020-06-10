# 4 Ranking hospitals in all states

# The function reads the outcome-of-care-measures.csv file and 
# returns a 2-column data frame containing the hospital in each state 
# that has the ranking specified in num. 

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  hospital.data = read.csv("outcome-of-care-measures.csv",
                           header = TRUE,
                           colClasses = "character",
                           na.strings = "Not Available")
  ## Check that outcome is valid
  if (sum(outcome %in% c("heart attack", "heart failure", "pneumonia")) == 0) {
    stop("invalid outcome")
  }

  # Only take the columns we need:
  # 2 = Hospital Name
  # 7 = State
  # 11 = heart attack 30-day mortality rate
  # 17 = heart failure 30-day mortality rate
  # 23 = pneumonia  30-day mortality rate
  hospital.data = hospital.data[, c(2, 7, 11, 17, 23)]
  hospital.data[, 3:5] <- mapply(as.numeric, hospital.data[, 3:5])
  
  if (outcome == "heart attack") {
    hospital.data <- hospital.data[, 1:3]
  }
  if (outcome == "heart failure") {
    hospital.data <- hospital.data[, c(1, 2, 4)]
  }
  if (outcome == "pneumonia") {
    hospital.data <- hospital.data[, c(1, 2, 5)]
  }
  
  ## For each state, find the hospital of the given rank
  rankhospital <- function(state) {
    hospital.data <- hospital.data[hospital.data$State == state, ]
    
    ## Return hospital name in that state with the given rank
    if (num == "best") {
      Result <- min(hospital.data[, 3], na.rm = T)
      show <- sort(hospital.data[which(hospital.data[, 3] == Result), 1])[1]
    }
    
    else if (num == "worst") {
      Result <- max(hospital.data[, 3], na.rm = T)
      show <- sort(hospital.data[which(hospital.data[, 3] == Result), 1],
                   decreasing = T)[1]
    }
    
    else if (num >= 1 & num <= length(hospital.data[, 1])) {
      hospital.data <- hospital.data[order(hospital.data[, 3], 
                                           hospital.data[, 1], 
                                           na.last = NA), ]
      show <- hospital.data[num, 1]
    }
    
    else {
      show <- NA
    }
    show
  }
  state <- sort(unique(hospital.data$State))
  hospital <- sapply(state, rankhospital)
  df <- cbind.data.frame(hospital, state)
  df
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}

