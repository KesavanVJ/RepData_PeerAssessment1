# Load Libraries - plyr (for data set operations) & lattice (for plotting)
library(plyr)
library(lattice)

#unzip and read the data file. Data file already downloaded
acdat <- read.csv(unz("./activity.zip", "activity.csv"))

# Total number of steps taken per Day
sumbyday <- aggregate(steps ~ date, FUN=sum, data = acdat, na.action = na.omit)

# Histogram of Total number of steps  taken per Day
hist(sumbyday$steps, col = "lightblue", xlab = "Steps", main = "Total Steps")

# Mean total number of steps taken per day
mean(sumbyday$steps, na.rm = T)
# Median total number of steps taken per day
median(sumbyday$steps, na.rm = T)

# Average daily activity pattern
acavgnonull <- aggregate(steps ~ interval, FUN = mean, data=acdat, na.action = na.omit)

# Add a time series factor by formatting and converting the interval 
acavgnonull$time <- as.POSIXlt(sprintf("%04s", as.character(acavgnonull$interval)),
                                       format = "%H%M")

# Time series plot of the 5-minute interval and the average number of steps taken
plot(acavgnonull$time, acavgnonull$steps, 
     type="l", col = "blue",
     xlab = "Time of Day", ylab = "Average number of Steps")

# Time interval contains the maximum number of steps
acavgnonull[acavgnonull$steps == max(acavgnonull$steps),"interval"]

# Total number of missing values in the dataset 
length(which(is.na(acdat$steps))) # - 2304 rows

# Complete Records
# length(which(!is.na(acdat$steps))) # - 15264 rows

## Tidy data set - Missing values filled with mean for that 5-minute interval
acdatNull <- acdat[which(is.na(acdat$steps)), ]
acdatTidy <- join(acdatNull, acavgnonull, by = "interval", type = "left")
acdatTidy$steps <- acdatTidy[,4]
acdatTidy <- acdatTidy[,1:3]
# Adding Complete records to Tidy data set. 
acdatTidy <- rbind(acdatTidy, acdat[which(!is.na(acdat$steps)), ])
acdatTidy$date <- as.Date(acdatTidy$date, format="%Y-%m-%d")

sumbydayTidy <- aggregate(steps ~ date, FUN=sum, data = acdatTidy)

# Histogram of Total number of steps  taken per Day
hist(sumbydayTidy$steps, col = "lightgreen", xlab = "Steps", main = "Total Steps")

# Y-axis different - due to frequencies change since no missing values now

# Mean total number of steps taken per day
mean(sumbydayTidy$steps)
# Median total number of steps taken per day
median(sumbydayTidy$steps)

# Weekend / Weekday compares
acdatTidy$wd <- ifelse(weekdays(acdatTidy$date) %in% c("Saturday", "Sunday"), 
                       "Weekend", 
                       "Weekday")
acavgwd <- aggregate(steps ~ interval + wd, FUN = mean, data = acdatTidy)
xyplot(steps ~ interval | wd, data = acavgwd, layout = c(1,2), type = "l", col = "red")

