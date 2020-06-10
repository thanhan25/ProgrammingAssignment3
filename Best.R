# 1 Plot the 30-day mortality rates for heart attack


# Read the outcome data into R via the read.csv function and 
# look at the first few rows

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
View(outcome)
str(outcome)


outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

# 2 Finding the best hospital in a state
best <- function(state, 
                 outcome) {
  ## Read outcome data
  hospital.data = read.csv("outcome-of-care-measures.csv",
                           header = TRUE,
                           colClasses = "character",
                           na.strings = "Not Available")
  # Only take the columns we need:
  # 2 = Hospital Name
  # 7 = State
  # 11 = heart attack 30-day mortality rate
  # 17 = heart failure 30-day mortality rate
  # 23 = pneumonia  30-day mortality rate
  hospital.data = hospital.data[, c(2, 7, 11, 17, 23)]
  hospital.data <- hospital.data[hospital.data$State == state, ]
  hospital.data[, 3:5] <- mapply(as.numeric, hospital.data[, 3:5])
  
  ## Check that state and outcome are valid
  if (sum(state %in% unique(hospital.data$State)) == 0) {
    stop("invalid state")
  }
  if (sum(outcome %in% c("heart attack", "heart failure", "pneumonia")) == 0) {
    stop("invalid outcome")
  }

  ## Return hospital name in that state with lowest 30-day death
  Results <- mapply(min, hospital.data[, 3:5], na.rm = T)
  Results

  if (outcome == "heart attack") {
    winner <- sort(hospital.data[which(hospital.data[, 3] == Results[1]), 1])[1]
  }

  if (outcome == "heart failure") {
    winner <- sort(hospital.data[which(hospital.data[, 4] == Results[2]), 1])[1]
  }

  if (outcome == "pneumonia") {
    winner <- sort(hospital.data[which(hospital.data[, 5] == Results[3]), 1])[1]
  }
  winner
}


