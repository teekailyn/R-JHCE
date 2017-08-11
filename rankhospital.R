# 'rankhospital' function reads the data file and returns a character vector with the name of the hospital that has the ranking specified by the 'num' argument. 

# The 'num' argument can take values "best", "worst", or an integer indicating the ranking (smaller numbers are better). If the number given by 'num' is larger than the number of hospitals in that state, then the function should return NA. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

# It may occur that multiple hospitals have the same 30-day mortality rate for a given cause of death. In those cases ties should be broken by using the 'order' function to sort multiple vectors in alphabetical order.

# The function should check the validity of its arguments. If an invalid 'state' value is passed, the function should throw an error via the stop function with the exact message "invalid state". If an invalid outcome value is passed, the function should throw an error via the stop function with the exact message "invalid outcome".
rankhospital <- function(state, outcome, num) {
	
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
	
	# 3. Return hospital name in the state with the given rank of 30-day death rate
	coln <- numeric()
	if(outcome == "heart attack") coln <- 11
	else if(outcome == "heart failure") coln <- 17
	else if(outcome == "pneumonia") coln <- 23
	
	numn <- numeric()
	if(num == "worst") numn <- 1
	else if(num == "best") numn <- 2
	else numn <- 0
	
	#filter state to start a new set of data for categorising
	stData <- ocd[as.character(ocd$State) == state, ]
	#	removing all "na" data from filtered dataset
	stData <- stData[complete.cases(as.numeric(as.character(stData[ ,coln]))), ]
	#rearrange stData in ascending order using the order function
	newstData <- stData[order(as.numeric(as.character(stData[ ,coln]))), ]
	
	if(numn == 0) {
		finale <- newstData[num,coln]
		arrayData <- newstData[as.numeric(as.character(newstData[, coln])) == finale, ]
	}
	else if(numn == 1) {
		finale <- newstData[length(newstData[ ,coln]), coln]
		arrayData <- newstData[as.numeric(as.character(newstData[, coln])) == finale, ]
	}
	else if(numn == 2) {
		finale <- newstData[1, coln]
		arrayData <- newstData[as.numeric(as.character(newstData[, coln])) == finale, ]
	}
	
	#reordering arrayData in ascending order according to hosptial name using the "reorder" function coded below
	reorderData <- reorder(arrayData)
		
	#create variable "Dataline" for final data filtering
	Dataline <- reorderData[1, ]
	
	print(Dataline[ ,2])
	
}

#create another function that orders the data according to hospital name in alphabetical order
reorder <- function(data) {
	order(as.character(data[ ,2]))
	return(data)
}
