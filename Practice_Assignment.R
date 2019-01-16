
andy <- read.csv("diet_data/Andy.csv")
head(andy)

length(andy$Day)
dim(andy)

str(andy) # structure
summary(andy) # summary
names(andy) # names

# look into Andy's weight
andy[1,"Weight"]
andy[30, "Weight"]

andy[which(andy$Day==30),"Weight"]
andy[which(andy[,"Day"]==30),"Weight"]

subset(andy$Weight, andy$Day==30)

andy_start <- andy[1, "Weight"]
andy_end <- andy[30, "Weight"]
andy_loss <- andy_start - andy_end
andy_loss

# looking at files 
files <- list.files("diet_data")
files
files[1]
files[2]
files[3:5]

head(read.csv(files[3]))

files_full <- list.files("diet_data", full.names=TRUE)
files_full
head(read.csv(files_full[3]))

andy_david <- rbind(andy, read.csv(files_full[2]))
head(andy_david)
tail(andy_david)

day_25 <- andy_david[which(andy_david$Day==25,),]
day_25

# playing with looping and files
files_full <- list.files("diet_data", full.names=TRUE)
files_full

dat <- data.frame()
for(i in 1:5) {
  dat <- rbind(dat, read.csv(files_full[i]))
}
str(dat)

for(i in 1:5) {
  dat2 <- data.frame()
  dat2 <- rbind(dat, read.csv(files_full[i]))
}
str(dat2)

median(dat$Weight)
dat

median(dat$Weight, na.rm=TRUE)

dat_30 <- dat[which(dat[,"Day"] == 30),]
dat_30

weightmedian <- function(directory, day) {
  files_list <- list.files(directory, full.names=TRUE) #creates a list of files
  dat <- data.frame() #creates an empty data frame
  for (i in 1:5) { # loops through the files, rbinding them together
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  dat_subset <- dat[which(dat[,"Day"]==day),] #subsets the rows that match the 'day' argument
  median(dat_subset[, "Weight"], na.rm=TRUE) #identifies the median weight while stripping na
}

weightmedian(directory = "diet_data", day = 20)
weightmedian(directory = "diet_data", day = 4)
weightmedian(directory = "diet_data", day = 17)