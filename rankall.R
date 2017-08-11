#Write a function called "rankall" that takes two arguments: an outcome (outcome) and a hospital ranking (num). The function reads the "outcome-of-care-measures.csv" file and returns a 2-column data frame containing the hospital in each state that has the ranking specified in num. 

#The first column in the data frame is named "hospital", which contains the hospital name, and the second column is named "state", which contains a 2-character abbreviation for the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

#The "rankall" function should handle ties in the 30-day mortality rates in the same way as the "rankhospital" function handle ties.

rankall <- function(outcome, num = "best") {
	
	#1. Read outcome data
	#	"ocd" - outcome data
	ocd <- read.csv("outcome-of-care-measures.csv")
	oclist <- c("heart attack", "heart failure", "pneumonia")
	statelist <- unique(ocd$State)
	statelist <- statelist[order(statelist)]
	
	#2. Check that outcome is valid
	if(!(any(oclist == outcome))) stop("invalid outcome")
	
	#3. For each state, find the hospital of the given rank
	#	Identify column number according to outcome stated in argument
	coln <- numeric()
	if(outcome == "heart attack") coln <- 11
	else if(outcome == "heart failure") coln <- 17
	else if(outcome == "pneumonia") coln <- 23
	
	numn <- numeric()
	if(num == "worst") numn <- 1
	else if(num == "best") numn <- 2
	else numn <- 0

	#	Create a list to store all hospital names according to rank
	dfList <- data.frame(hospital = character(0), state = character(0))

	#	for loop to sort out ranked mortality for every state	
	for(x in statelist) {
		# "stData" - filtered "ocd" by state "x"
		stData <- ocd[as.character(ocd$State) == x, ]
		#removing NAs from filtered dataset
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
		
		#reordering "arrayData" in ascending order according to hospital name
		reorderData <- arrayData[order(as.character(arrayData[ ,2])), ]
		
		dataLine <- data.frame(hospital = reorderData[1, 2], state = x)
		
		dfList <- rbind(dfList, dataLine)
	}
	
	#4. Return a data frame with the hospital names and the (abbreviated) state name
	
	return(dfList)
	
}
