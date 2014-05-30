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
setwd()
  library(list)
  library(pairlist)
  library(debug)
  library(codetools)
  library(rstudio)
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  suppressWarnings(outcome[, 11] <- as.numeric(outcome[, 11]))
  suppressWarnings(outcome[, 17] <- as.numeric(outcome[, 17]))
  suppressWarnings(outcome[, 23] <- as.numeric(outcome[, 23]))
  ## Check user's "state" and "outcome" entries for validity;dz90
  
  theDiseases <- pairlist("pneumonia" = 23, "heart failure" = 17, "heart attack" = 11 )
  theStates <- c( "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
                  "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI",
                  "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
                  "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT",
                  "VT", "VI", "VA", "WA", "WV", "WI", "WY", "GU")
  if(state %in% theStates) {
    stop("invalid outcome")
 if(theDiseases[[outcome]] == 0) {
     stop("invalid outcome")
  }
 stateSubFR[order(stateSubFR[,theDiseases[[outcome]]],na.last = TRUE),]
  ## Still running? Filter the data, winnowing hospitals
  ## with no record of treatment for the stipulated condition.stat\
 stateSubFR <- subset( outcomes, State == state )
order(stateSubFR[,theDiseases[[outcome]]],stateSubFR[,2], na.last = TRUE)
 stateSubFR[order(theDiseases[[outcome]] ])
  comparison_to_U_S_Rate_Hospital_30_Day_Death__Mortality__Rates_from_Heart_Attack
  sqldf("select * from outcome$Comparison_to_U_S__Rate_Hospital_30_Day_Death_Mortality_Rates_from_Heart_Attack limit 5")
  sqldf("select iris.Species '[Species]', 
+       avg(Sepal_Length) '[Avg of SLs > avg SL]'
+    from iris, 
+         (select Species, avg(Sepal_Length) SLavg 
+         from iris group by Species) SLavg
+    where iris.Species = SLavg.Species 
+       and Sepal_Length > SLavg
+    group by iris.Species")
  sqldf("select outcome.State ['State'], 
outcome.Hospital.Name ['HName'],
Hospital_30_Day_Death_Mortality_Rates_from_Heart_Attack ['HMR'] 
from outcome,
(select max(HMR)) desc limit 5)

sqldf("select State, Hospital_Name,
        'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
         from outcome where State = state
        and 
        'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia' != NotAvailable
         group by State
        order by 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'" )

x <- stateSubFR[order(stateSubFR[17],stateSubFR[2]) ]
x[1,2]
  ## Winnow the remaining candidates
  
  
  ## Return the name of the hospital with the lowest 30-death rate in that state
  ## for treatment of the stipulated condition
  
  
}