	#This function takes two args, name of state, and outcome. The fuction reads "outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state.
	#The hospital name is the name provided in the "Hospital.Name" variable.
	#The outcomes can be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
	
	#If there is a "tie" for the best hospital for a given outcome, then the hospital names should be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c", and "f" are tied for best, then hospital "b" should be returned).
	
	#Additionally, if an invalid state value is passed to "best" function, the function should throw an error via the "stop" function with the exact message "invalid state". If an invalid oustcome value is passed, the function should throw an error via the "stop" function with the exact message "invalid outcome".

best <- function(state, outcome) {
	# 1. Read outcome data
	#	"ocd" - outcome data
	ocd <- read.csv("outcome-of-care-measures.csv")
	oclist <- c("heart attack", "heart failure", "pneumonia")
	statelist <- unique(ocd$State)
	
	# 2. Check that state and outcome are valid
	#	a. Check for state
	if(!(any(statelist == state)))	stop("invalid state")
	#	b. Check for outcome
	if(!(any(oclist == outcome))) stop("invalid outcome")
	
	# 3. Return hospital name in that state with lowest 30-day death rate
	coln <- numeric()
	if(outcome == "heart attack") coln <- 11
	else if(outcome == "heart failure") coln <- 17
	else if(outcome == "pneumonia") coln <- 23
		
	stData <- ocd[ocd$State == state, ]
	minNo <- min(as.numeric(as.character(stData[ ,coln])), na.rm=TRUE)
	
	hnArray <- stData[stData[ ,coln] == minNo, 2]
	hnArray_s <- hnArray[order(hnArray)]
	hospName <- hnArray_s[1]
	
	hospName
}
