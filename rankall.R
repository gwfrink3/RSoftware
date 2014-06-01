best <- function(state, outcome) {
  ## Programmer - George W. Frink, III
  ## copyright 2014 George W. Frink, III
  ## Checks input and throws errors upon detection of a problem
  ##    Specifically, a valid state name is required.
  ##    Mispellings are not corrected.
  ##    The software halts after throwing an "Invalid state name error"
  ##    A valid cause of treatment is required. "Heart Failure," for example.
  ##    If the stipulation is invalid, the software halts on an "Invalid
  ##    outcome name" error.
  ##
  library(list)
  library(pairlist)
  library(debug)
  library(codetools)
  library(rstudio)
  library(data.table)
  ## Read outcome data
  HospitalData <- read.csv("outcome-of-care-measures.csv",  colClasses = "character",check.names = TRUE,  )
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
