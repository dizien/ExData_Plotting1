## "plot3.R" function creates a chart of "3 sub metering lines vs time" in PNG format
## data source: UC Irvine Machine Learning Repository, electric power consumption
## date range over 2 day period is used, i.e. from 2007-02-01 to 2007-02-02

plot3 <- function(){
        x <- getfile()
        makegraph(x)
}

## "getfile" function to read rawfile and return dataframe filtered by dates in range
getfile <- function(){
        
        ## specify name of directory and file
        directory <- "C:/Users/A1411/Desktop/DataScience/04 Exploratory Data Analysis/Coursework"
        filename <- "exdata_data_household_power_consumption.zip"
        
        ## set working directory and extract file 
        setwd(directory)
        x <- read.table(unzip(filename), header = TRUE, sep=";", na.strings="?")
        
        ## add a "datetime" column at the end with combined Date & Time in POSIXlt format 
        x <- within(x, datetime <- as.POSIXlt(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))
        
        ## specify start and end date
        datestart <- as.Date("2007-02-01")
        dateend <- as.Date("2007-02-02")
        
        ## find the rows that is within range
        rightrow <- as.Date(x$datetime) >= datestart & as.Date(x$datetime) <= dateend
        
        ## reduce size of data frame and return dataframe with data from selected rows
        x <- x[rightrow,]
        remove(rightrow)  ## remove row identifier that is not required anymore (about 7MB)
        return(x)
}

## "makegraph" function creates a chart of "3 sub metering lines vs time" in PNG format
makegraph <- function(x){
        
        png (filename = "plot3.png", width = 480, height = 480)
        
        ## set up empty plotting area
        plot(x$datetime, x$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")

        ## plot line 1 in black using sub_merering_1
        lines(x$datetime, x$Sub_metering_1, col="black")
        
        ## plot line 2 in red using sub_merering_2
        lines(x$datetime, x$Sub_metering_2, col="red")
        
        ## plot line 3 in red using sub_merering_3
        lines(x$datetime, x$Sub_metering_3, col="blue")

        ## add legend to topright corner
        legend("topright", col=c("black", "red", "blue"), lty=c(1,1,1),
               legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        
        dev.off()
}
