
best <- function(state, outcome) {
        ## Read outcome data
	outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	if(!(state %in% outcome.df[, 7])) {stop("invalid state")}
	if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) {stop("invalid outcome")}

	## Return hospital name in that state with lowest 30-day death
	## rate
	
	if (outcome=="heart attack" ) {
		outcome.col <- 11
	} else if (outcome=="heart failure") {
		outcome.col <- 17
	} else if (outcome=="pneumonia") {
		outcome.col <- 23
	}

	state.data <- subset(outcome.df, State == state, select = c("hospital" = 2, outcome = outcome.col))
	
	state.data[, 2] <- as.numeric(state.data[, 2])

	return((state.data[ order(state.data[,2], state.data[,1]), ])[1, 1])
}
