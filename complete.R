# ********************************
# FILE STRUCTURE
# 
# specdata.zip [2.4MB] contains 332 comma-separated-value (CSV) files containing 
# pollution monitoring data for fine particulate matter (PM) air pollution at 332 
# locations in the United States. Each file contains data from a single monitor 
# and the ID number for each monitor is contained in the file name. For example, 
# data for monitor 200 is contained in the file "200.csv". Each file contains 
# three variables:
# 
#    * Date: the date of the observation in YYYY-MM-DD format 
#      (year-month-day)
#    * sulfate: the level of sulfate PM in the air on that date 
#      (measured in micrograms per cubic meter)
#    * nitrate: the level of nitrate PM in the air on that date 
#      (measured in micrograms per cubic meter)
# 
# Note that in each file you'll notice that there are many days where either 
# sulfate or nitrate (or both) are missing (coded as NA). This is common with air
# pollution monitoring data in the United States.
# ********************************

# ********************************
# FUNCTION COMPLETE()
# complete() reads a directory full of files and reports the number of completely 
# observed cases in each data file. 
# 
# The function complete() takes three arguments: 
#    * 'directory': character vector of length 1 indicating the location of 
#       the CSV files.
#    * 'id': an integer vector indicating the monitor ID numbers to be used.
# 
# Given a vector monitor ID numbers, complete() should return a data frame where 
# the first column is the name of the file and the second column is the number of 
# complete cases. 
# ********************************


complete <- function(directory, id = 1:332) {
        
        file_list <- list.files(directory, full.names=TRUE)
        
        nobs_list <- data.frame()
        for(i in id) {
                monitor <- read.csv(file_list[i])
                good <- complete.cases(monitor[,"sulfate"], monitor[,"nitrate"])
                nobs <- length(which(good))
                nobs_list <- rbind(nobs_list, data.frame("id"=i,"nobs"=nobs))
        }
        nobs_list
}
        
print(R.version.string)
complete
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

# programming quiz
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

