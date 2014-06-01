rankall <- function(outcome, num = "best"){
library(data.table)
    	HospitalData <- read.csv("outcome-of-care-measures.csv",  colClasses = "character",check.names = TRUE)
	suppressWarnings(HospitalData[, 11] <- as.numeric(HospitalData[, 11])) #heart attack
	suppressWarnings(HospitalData[, 17] <- as.numeric(HospitalData[, 17])) # heart failure
	suppressWarnings(HospitalData[, 23] <- as.numeric(HospitalData[, 23])) # penumonia
	StateList <- c("AK", "AL","AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
	 "MA", "ME", "MD", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR",
	 "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
	##
	## Check input against the list of states.
	##

	if(!state %in% StateList) { 
		stop("invalid state") 
	}else{
		HospitalData <-  na.omit(HospitalData)
	}
	##
	## A list of the three valid outcomes are prescribed.

	diseases <- c("pneumonia", "heart failure" , "heart attack")
	## Match the variable 'outcome' against the list
	## and if it is not found, return an error.
	## Otherwise, reset 'outcome' to the data.table column name
	## for the hospital statistics sought.
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
	## Convert the data frame to a data.table
	## The payback is speed, especially speed of sorting
	##
	HDT <- data.table(HospitalData)
	HDT <- na.omit(HDT)
	len <- length(StateList)
 	HDTreturn <- data.table("hospital" = "                                ",  "state" = "      ")
	##setnames(HDTreturn,c("Hospital Name", "State"))
	for(i in 1:len){
		HDTsub <- subset(HDT,State == StateList[i])
		setkey(HDTsub,State)	
		HDTsub <- HDTsub[order(HDTsub[[outcome]],Hospital.Name)]
		 	if(nrow(HDTsub) <= 0){ ## No data for this state? Return NA
				newRow <- data.table("hospital" = NA,"state" = HDTsub$State)
				HDTreturn <- rbind(HDTreturn,newRow)
			}else{					## Otherwise by outcome data and break ties with sort by name	
				newRow <- data.table( "hospital" = HDTsub$Hospital.Name, "state" = HDTsub$State)	
				if(num == "best") {
					HDTreturn <- rbind(HDTreturn,newRow[1])
				} else if(num == "worst") {
						HDTreturn <- rbind(HDTreturn,newRow[nrow(newRow)])
				} else if(num > nrow(newRow)) {
						tempT <- data.table("hospital" = NA,"state" = StateList[i])
						HDTreturn <- rbind(HDTreturn,tempT)
				##	print(HDTsub$State)
				##	print(i)
				} else if(num <= nrow(newRow)) { 
						HDTreturn <- rbind(HDTreturn,newRow[num])
				} else {
					stop("invalid enumeration") 
				}
			}		## End enclosing else		
		}	## End for loop
			return(HDTreturn)		
	}			## End rankall
