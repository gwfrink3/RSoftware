rankhospital <- function(state, outcome, num = "best"){
          ## Read outcome data                                                        
          ## Check that state and outcome are valid                                   
          ## Return hospital name in that state with lowest 30-day death rate  
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
	if(!outcome %in% diseases) {
     		stop("invalid outcome")
	}else{
			switch(outcome, 
				 "pneumonia" = {
					outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
					},
				"heart failure" =  {
					outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
					},
				 "heart attack"  =  {
					outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
					}
					)
		}
		
	HDT <- data.table(HospitalData)
	HDTsub <- subset(HDT,State == state)
		# Sort the state-specific data by outcome value and in case of a tie, by hospital name
		setkey(HDTsub,State)
		HDTsub <- HDTsub[order(HDTsub[[outcome]],Hospital.Name)]
	# Format the return/display data.table, first with blank spaces and them with column names
		HDTreturn <- data.table("                                ")
		#setnames(HDTreturn,c( "Hospital Name"))
	
		if(length(HDTsub) <= 0){
			return("NA")
		}	
	if(num == "best") {
		return(HDTsub$Hospital.Name[1])

	} else if(num == "worst") {
		len <- length(HDTsub$Hospital.Name)
		return(HDTsub$Hospital.Name[len])
	} else if(is.numeric(num)) {
		if(num > length(HDTsub$Hospital.Name)){
			return("NA")
		}else{ 
			return(HDTsub$Hospital.Name[num])
		}
	} else {
		stop("invalid enumeration") 
	}	
	}
	
