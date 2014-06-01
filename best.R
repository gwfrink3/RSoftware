best <- function(state, Outcome) {   
	## Copyright George W. Frink III, June 1, 2014
	# Written by George W. Frink III 
	## Contact: frink@southernconnections.net
	library(data.table)                                         
          ## Read outcome data                                                        
          ## Check that state and outcome are valid                                   
          ## Return hospital name in that state with lowest 30-day death rate  
	HospitalData <- read.csv("outcome-of-care-measures.csv",  colClasses = "character",check.names = TRUE)
	suppressWarnings(HospitalData[, 11] <- as.numeric(HospitalData[, 11])) #heart attack
	suppressWarnings(HospitalData[, 17] <- as.numeric(HospitalData[, 17])) # heart failure
	suppressWarnings(HospitalData[, 23] <- as.numeric(HospitalData[, 23])) # penumonia
	StateList <- c( "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA",
	 "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
	 "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY", "GU")
	##
	## Check input against the list of states.
	##

	if(!state %in% StateList) { 
		stop("invalid state") 
	}else{
		HospitalData <-  na.omit(HospitalData)
	}
	##
	## The three valid outcomes are prescribed.
	##
	diseases <- c("pneumonia", "heart failure" , "heart attack")
	if(!Outcome %in% diseases) {
     		stop("invalid outcome")
	}
	## State and Outcome are valid.
	## Create the data tables
		HDT <- data.table(HospitalData)
		HDT <- subset(HDT, State == state)	
	if(Outcome == "pneumonia"){
		 HDT <- HDT[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)]
	}else if(Outcome == "heart failure"){
		HDT <- HDT[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)]
	}else if(Outcome == "heart attack"){
		HDT <- HDT[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)]
	}	
HDT[1]$Hospital.Name	 
  }
