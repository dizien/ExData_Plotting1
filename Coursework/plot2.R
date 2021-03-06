## "plot2.R" function creates a line chart of "Global Active Power vs time" in PNG format
## data source: UC Irvine Machine Learning Repository, electric power consumption
## date range over 2 day period is used, i.e. from 2007-02-01 to 2007-02-02

plot2 <- function(){
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

## "makegraph" function plots line chart of "Global Active Power vs time" in PNG format
makegraph <- function(x){
        
        png (filename = "plot2.png", width = 480, height = 480)
        
        plot(x$datetime, x$Global_active_power, type="l",
             xlab="", ylab="Global Active Power (kilowatts)")
        
        dev.off()
}
