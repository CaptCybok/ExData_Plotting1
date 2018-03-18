#Initializes the libraries required for this script
library(dplyr)
library(datasets)

initSecondPlot <- function() {
    #Sets the working directory for this script
    setwd("D:/Workspace/Coursera/DOST Data Science/R Workspace/Module 4/Week 1")
    
    #Loads the data that will be used on this script
    powerData <- read.table(file = "household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)
    powerData <- setNames(powerData, c(powerData[1,]))
    powerData <- powerData[-1,]
    
    #Formats the date to the proper format
    dateData <- powerData[,1]
    dateData <- strptime(dateData, "%d/%m/%Y")
    dateData <- format(dateData, "%Y-%m-%d")
    
    
    #Filters the data based on the requirements
    fPowerData <- powerData
    fPowerData[ ,1] <- dateData
    fPowerData <- filter(fPowerData, grepl("2007-02-01|2007-02-02", fPowerData[,1]))
    
    #Gathering the data for day values
    dayData <- weekdays(as.Date(fPowerData[,1]))
    fPowerData <- mutate(fPowerData, dayData)
    
    #Plots the line graph and saves it
    par(mar = c(2,4,4,4))
    plot(as.double(fPowerData[,3]), xaxt = "n", type = "n", ylab = "Global Active Power (kilowatts)")
    lines(as.double(fPowerData[,3]), type = "l")
    perDay <- fPowerData[,10]
    axis(1, at=c(1,length(perDay)), lab=c(perDay[1], perDay[length(perDay)]))
    dev.copy(png, "plot2.png")
    dev.off()
}