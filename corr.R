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
# FUNCTION CORR()
# corr() takes a directory of data files and a threshold for complete cases and 
# calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater 
# than the threshold. 
# 
# The function complete() takes three arguments: 
#    * 'directory': character vector of length 1 indicating the location of 
#       the CSV files.
#    * 'threshold': a numeric vector of length 1 indicating the number of 
#      completely observed observations (on all variables) required to compute 
#      the correlation between nitrate and sulfate; the default is 0.

# corr() should return a vector of correlations for the monitors that meet the 
# threshold requirement. If no monitors meet the threshold requirement, then the 
# function should return a numeric vector of length 0. 
# ********************************


corr <- function(directory, threshold=0) {
        
        file_list <- list.files(directory, full.names=TRUE)

        monitor_list <- data.frame()
        nobs_list <- data.frame()
        
        # load monitor_list from files generate matching list good samples
        for(i in 1:length(file_list)) {
                monitor <- read.csv(file_list[i])
                monitor_list <- rbind(monitor_list, monitor)
                
                good <- complete.cases(monitor[,"sulfate"], monitor[,"nitrate"])
                nobs <- length(which(good))
                nobs_list <- rbind(nobs_list, data.frame("ID"=i,"nobs"=nobs))
        }
        
        # filter good list against threshold
        good_monitors <- nobs_list[which(nobs_list$nobs>=threshold),]$ID
        
        # step through good list and generate correlation value for each
        cr_list <- vector("numeric")
        if(length(good_monitors)>0) {
                for(i in 1:length(good_monitors)) {
                        x <- monitor_list[which(monitor_list[,"ID"] == good_monitors[i]),]$sulfate
                        y <- monitor_list[which(monitor_list[,"ID"] == good_monitors[i]),]$nitrate
                        if(sum(complete.cases(x,y))>1) {
                                cr <- cor(x,y, use = "complete")
                                cr_list <- c(cr_list, cr) 
                        } else {
                                cr_list <- c(cr_list, NA) 
                        }
                }
        }
        
        # return good list of correllation values (len=0 if none)
        cr_list
}


print(R.version.string)
corr

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)


# programming quiz
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
