## R Programming, Week 4, Programming Exercise 3
##

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
str(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


best <- function(state, outcome) {

        ## Read outcome data
        file_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        hospital_outcomes <- as.data.frame(cbind(
                                        file_data[, 2],   # hospital
                                        file_data[, 7],   # state
                                        file_data[, 11],  # heart attack
                                        file_data[, 17],  # heart failure
                                        file_data[, 23],  # pneumonia
                                        stringsAsFactors = FALSE
                                ))
        
        ## create list of valid outcomes and add column names to dataframe
        outcomes_valid <- c("heart attack", "heart failure", "pneumonia")
        colnames(hospital_outcomes) <- c(
                                "hospital", 
                                "state", 
                                outcomes_valid
                                )
        
        ## convert outcomes from char to numeric 
        hospital_outcomes[,"heart attack"] <- as.numeric(as.character(hospital_outcomes[,"heart attack"]))
        hospital_outcomes[,"heart failure"] <- as.numeric(as.character(hospital_outcomes[,"heart failure"]))
        hospital_outcomes[,"pneumonia"] <- as.numeric(as.character(hospital_outcomes[,"pneumonia"]))
        states_valid <- unique(hospital_outcomes$state)
 
        ## Check that state and outcome are valid
        if(!any(state %in% as.character(states_valid))) stop("invalid state")
        if(!any(outcome %in% as.character(outcomes_valid))) stop("invalid outcome")

        ## Return the one hospital name in the state with lowest 30-day death rate
        state_hospitals <- hospital_outcomes[which(hospital_outcomes$state == state),]
        best_hospital <- state_hospitals[which.min(state_hospitals[,outcome]),]
        return(as.character(best_hospital$hospital))
}

## TEST CASES
best("TX", "heart attack")  ## should return Cypress Fairbanks Medical Center
best("TX", "heart failure") ## should return Fort Duncan Medical Center
best("MD", "heart attack")  ## should return Johns Hopkins Hospital, The
best("MD", "pneumonia")     ## should return Greater Baltimore Mecial Center
best("BB", "heart attack")  ## should return error="invalid state"
best("NY", "hert attack")   ## should return error="invalid outcome"


## FINAL QUIZ
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

## 19. Outcome of Care Measures.csv
## The Outcome of Care Measures.csv table contains forty seven (47) fields. 
## This table provides each hospital’s risk-adjusted 30-Day Death (mortality) 
## and 30-Day Readmission category and rate.
## 
## ================================
## HOSPITAL INFO 
## --------------------------------
##  1   provider_num   Provider Number: varchar (6) Lists the hospitals by their provider identification number.
##  2   hospital       Hospital Name: varchar (50) Lists the name of the hospital.
##  3   address_1      Address 1: varchar (50) Lists the first line of the street address of the hospital.
##  4   address_2      Address 2: varchar (50) Lists the second line of the street address of the hospital.
##  5   address_3      Address 3: varchar (50) Lists the third line of the street address of the hospital.
##  6   city           City: varchar (28) Lists the city in which the hospital is located.
##  7   state          State: varchar (2) Lists the 2 letter State code in which the hospital is located.
##  8   zip            ZIP Code: char (5) Lists the 5 digit numeric ZIP for the hospital.
##  9   country        County Name: char (15) Lists the county in which the hospital is located.
##  10  phone          Phone Number: char (10) Lists the 10-digit numeric telephone number, including area code, for the Hospital.
## 
## ================================
## 30-DAY RISK-ADJUSTED DEATH (MORTALITY) 
## --------------------------------
##  11  heart attack   Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the risk adjusted rate (percentage) for each hospital.
##  12                 Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Attack: varchar (50) Lists the mortality and readmission category in which the hospital falls. The values are:  (a. Better than U.S. National Average, b. No Different than U.S. National Average, c. Worse than U.S. National Average, d. Number of Cases too Small).
##  13                 Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  14                 Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  15                 Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Attack: varchar (5) Lists the number of Medicare patients treated for Heart Attack by the Hospital.
##  16                 Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the footnote value when appropriate, as related to the Heart Attack Outcome of Care at the hospital.
## --------------------------------
##  17  heart failure  Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the risk adjusted rate (percentage) for each hospital.
##  18                 Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Failure: varchar (50) Lists the mortality and readmission category in which the hospital falls. The values are: (a. Better than U.S. National Average , b. No Different than U.S. National Average, c. Worse than U.S. National Average, d. Number of Cases too Small*)
##  19                 Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  20                 Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  21                 Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Failure: varchar (5) Lists the number of Medicare patients treated for Heart Failure by the Hospital.
##  22                 Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the footnote value when appropriate, as related to the Heart Failure Outcome of Care at the hospital.
## --------------------------------
##  23  pneumonia      Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the risk adjusted rate (percentage) for each hospital.
##  24                 Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Pneumonia: varchar (50) Lists the mortality and readmission category in which the hospital falls. The values are: (a. Better than U.S. National Average, b. No Different than U.S. National Average • Worse than U.S. National Average, c. Number of Cases too Small*)
##  25                 Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  26                 Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  27                 Number of Patients - Hospital 30-Day Death (Mortality) Rates from Pneumonia: varchar (5) Lists the number of Medicare patients treated for Pneumonia by the Hospital.
##  28                 Footnote - Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the footnote value when appropriate, as related to the Pneumonia Outcome of Care at the hospital.
## 
## ================================
## 30-DAY READMISSION CATEGORY AND RATE
## --------------------------------
##  29  readmission - heart attack        Hospital 30-Day Readmission Rates from Heart Attack: Lists the risk adjusted rate (percentage) for each hospital.
##  30                 Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Attack: varchar (50) Lists the mortality and readmission category in which the hospital falls. The values are: (a. Better than U.S. National Average, b. No Different than U.S. National Average c. Worse than U.S. National Average, d. Number of Cases too Small*)
##  32                 Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack: Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  33                 Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack: Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  34                 Number of Patients - Hospital 30-Day Readmission Rates from Heart Attack: varchar (5) Lists the number of Medicare patients treated for Heart Attack.
##  35                 Footnote - Hospital 30-Day Readmission Rates from Heart Attack: Lists the footnote value when appropriate, as related to the Heart Attack Outcome of Care at the hospital.
## --------------------------------
##  36  readmission - heart failure        Hospital 30-Day Readmission Rates from Heart Failure: Lists the risk adjusted rate (percentage) for each hospital.
##  37                Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Failure: varchar (50) Lists the mortality and readmission category in which the hospital falls. The values are: (a. Better than U.S. National Average, b. No Different than U.S. National Average • Worse than U.S. National Average, c. Number of Cases too Small*)
##  38                Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure: Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  39                Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure: Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  40                Number of Patients - Hospital 30-Day Readmission Rates from Heart Failure: varchar (5) Lists the number of Medicare patients treated for Heart Failure.
##  41                Footnote - Hospital 30-Day Readmission Rates from Heart Failure: Lists the footnote value when appropriate, as related to the Heart Failure Outcome of Care at the hospital.
## --------------------------------
##  42  readmission - pneumonia        Hospital 30-Day Readmission Rates from Pneumonia: Lists the risk adjusted rate (percentage) for each hospital.
##  43                Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Pneumonia: varchar (50) Lists the mortality and readmission category in which the hospital falls. The values are:  (a. Better than U.S. National Average, b. No Different than U.S. National Average • Worse than U.S. National Average, c. Number of Cases too Small*)
##  44                Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia: Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  45                Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia: Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
##  46                Number of Patients - Hospital 30-Day Readmission Rates from Pneumonia: varchar (5) Lists the number of Medicare patients treated for Pneumonia.
##  47                Footnote - Hospital 30-Day Readmission Rates from Pneumonia: Lists the footnote value when appropriate, as related to the Pneumonia Outcome of Care at the hospital.
## --------------------------------
##  
