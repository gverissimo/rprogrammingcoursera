## R Programming, Week 4, Programming Exercise 3
##

rankall <- function(outcome, num="best") {
        
        ## Read outcome data
        file_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        hospital_outcomes <- cbind.data.frame(
                file_data[, 2],   # hospital
                file_data[, 7],   # state
                as.numeric(as.character(file_data[, 11])),  # heart attack
                as.numeric(as.character(file_data[, 17])),  # heart failure
                as.numeric(as.character(file_data[, 23])),  # pneumonia\
                stringsAsFactors = FALSE
        )
        
        ## create list of valid outcomes and add column names
        ## (for whatever reason, adding column names when defining dataframe resulted in 
        ## "heart attack" becing "heart.attack". Workaround: add names after definition)
        outcomes_valid <- c("heart attack", "heart failure", "pneumonia")
        colnames(hospital_outcomes) <- c(
                "hospital", 
                "state", 
                outcomes_valid
        )
         
        states_all <- unique(hospital_outcomes$state)
        states_all <- sort(states_all)
        
        ## validate  (state, outcome, num)
        if(!any(outcome %in% as.character(outcomes_valid))) stop("invalid outcome")
        if(is.numeric(num) & num>0) {
                r0 <- num
        } else if(num=="best" | num=="Best") {
                r0 <- 1
        } else if(num=="worst" | num=="Worst") {
                r0 <- -1 ## use -1 as numeric flag for 'last'
        } else {
                stop("invalid rank")
        }
        
        ## sort by (1) state and then (2) outcome 
        ## ...and split by state
        hospitals_ordered <- hospital_outcomes[order(hospital_outcomes[,"state"], hospital_outcomes[,outcome], hospital_outcomes[,"hospital"]), ]
        hospitals_state <- with(hospitals_ordered, split(hospitals_ordered,state))
        
        ##initialize output frame
        output_ordered <- data.frame(
                hospital=character(), 
                state=character(), 
                stringsAsFactors = FALSE
        )
        
        for(i in states_all) {
                state_ordered <- hospitals_state[[i]][order(hospitals_state[[i]][,outcome], hospitals_state[[i]][,"hospital"], na.last=NA),]
                h<-nrow(state_ordered) ## number of resulting hospitals
                r <- r0 ## reset r
                if(r<0) r <- h   ## check for r flag "last" (ie, -1)
                if(r>h) { ## check rank doesn't exceed number of resulting hospsitals (h)
                        x <- data.frame(hospital=NA, state=hospitals_state[[i]][1,"state"])
                        output_ordered <- rbind.data.frame(output_ordered, x)
                }
                else  {
                        x <- cbind(hospital=hospitals_state[[i]][r,"hospital"], state=hospitals_state[[i]][r,"state"])
                        output_ordered <- rbind.data.frame(output_ordered, x)
                }
        }
        return(output_ordered)
}

## TEST CASES
head(rankall("heart failure", 1) , 5)      ## should return 
head(rankall("heart failure", 2) , 5)      ## should return 
head(rankall("heart failure", "best"), 5)  ## should return
tail(rankall("heart failure", "worst"), 5) ## should return
tail(rankall("heart failure", 5000), 5)    ## should return NA
head(rankall("heart failure", -5), 5)      ## should error "invalid rank"
head(rankall("heart failure", -1), 5)      ## should error "invalid rank"


head(rankall("heart attack", 20), 10) 
## should return:
##                                  hospital state
##                                      <NA> AK
##            D W MmMillan Memorial Hospital AL
##         Arkansas Methodist Medical Center AR
##       John C Lincoln Deer Valley Hospital AZ
##                     Sherman Oaks Hospital CA
##                  Sky Ridge Medical Center CO
##                   MidState Medical Center CT
##                                      <NA> DC
##                                      <NA> DE
##            South Florida Baptist Hospital FL

tail(rankall("pneumonia", "worst"), 3)
## should return:
##                                  hospital state
##  Mayo Clinic Health System - Northland... WI
##                    Plateau Medical Center WV
##          North Big Horn Hospital District WY

tail(rankall("heart failure"), 10) 
## should return:
##                                  hospital state
##  Welmont Hawkins County Memorial Hospital TN
##                Fort Duncan Medical Center TX
##  VA Salt Lake City Healthcare, George...  UT
##                  Santara Potomac Hospital VA
##    Gov Juan F Luis Hospital & Medical Ctr VI
##                      Springfield Hospital VA
##                 Harborview Medical Center WA
##            Aurora St Lukes Medical Center WI
##                 Fairmont General Hospital WV
##                Cheyenne VA Medical Center WY


## FINAL QUIZ
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)



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
