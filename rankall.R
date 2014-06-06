rankall <- function(outcome, num = "best") {
	## Read outcome data
        outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        ## Check that state and outcome are valid
        if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) {stop("invalid outcome")}

	## Return a data frame with the hospital names and the
	## (abbreviated) state name

        if (outcome=="heart attack" ) {
                outcome.col <- 11
        } else if (outcome=="heart failure") {
                outcome.col <- 17
        } else if (outcome=="pneumonia") {
                outcome.col <- 23
        }

	bystate.df <- data.frame()
	for (state in unique(outcome.df[, 7])) {
		state.data <- subset(outcome.df, State == state, select = c("hospital" = 2, outcome = outcome.col))

        	state.data[, 2] <- as.numeric(state.data[, 2])
        	state.data <- na.omit(state.data)

        	if (num=="best")  {
                	hospital <- ((state.data[order(state.data[,2], state.data[,1]),])[1, 1])
        	} else if (num=="worst") {
                	hospital <- ((state.data[order(state.data[,2], state.data[,1]),])[nrow(state.data), 1])
        	} else {
                	hospital <- ((state.data[order(state.data[,2], state.data[,1]),])[num, 1])
        	}

		newrow <- data.frame(hospital, state)

		bystate.df <- rbind(bystate.df, newrow)

	}
	bystate.df
}

