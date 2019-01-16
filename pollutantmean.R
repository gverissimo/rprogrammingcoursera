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
# FUNCTION POLLUTANTMEAN()
# pollutantmean()  calculates the mean of a pollutant (sulfate or nitrate) across 
# a specified list of monitors. 
# 
# The function pollutantmean() takes three arguments: 
#    * 'directory': character vector of length 1 indicating the location of 
#       the CSV files.
#    * 'pollutant' : character vector of length 1 indicating the name of the 
#       pollutant for which we will calculate the mean(ie - "sulfate" | "nitrate")
#    * 'id': an integer vector indicating the monitor ID numbers to be used.
# 
# Given a vector monitor ID numbers, pollutantmean() reads that monitors' 
# particulate matter data from the directory specified in the 'directory' 
# argument and returns the mean of the pollutant across all of the monitors, 
# ignoring any missing values coded as NA.
# ********************************

pollutantmean <- function(directory, pollutant, id = 1:332) {
 
        file_list <- list.files(directory, full.names=TRUE)
        
        monitor_list <- data.frame()
        for(i in id) {
                monitor_list <- rbind(monitor_list, read.csv(file_list[i]))
        }
        mean(monitor_list[, pollutant], na.rm=TRUE)
}

print(R.version.string)
pollutantmean
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

# programming quiz
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")