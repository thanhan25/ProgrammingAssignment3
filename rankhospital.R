# Ranking hospitals by outcome in a state

# The function reads the outcome-of-care-measures.csv file and 
# returns a character vector with the name of the hospital 
# that has the ranking specified by the num argument

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  hospital.data = read.csv("outcome-of-care-measures.csv",
                           header = TRUE,
                           colClasses = "character",
                           na.strings = "Not Available")
  ## Check that state and outcome are valid
  if (sum(state %in% unique(hospital.data$State)) == 0) {
    stop("invalid state")
  }
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
  hospital.data <- hospital.data[hospital.data$State == state, ]
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
