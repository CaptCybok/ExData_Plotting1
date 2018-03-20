#Initializes the libraries required for this script
library(dplyr)
library(datasets)

initSecondPlot <- function() {
    #Sets the working directory for this script
    setwd("D:/Workspace/Coursera/DOST Data Science/R Workspace/Module 4/Week 1")
    
    #Loads the data that will be used on this script
    powerData <- read.table(file = "household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors = FALSE)
    
    #Formats the date to the proper format
    dateData <- powerData$Date
    dateData <- strptime(dateData, "%d/%m/%Y")
    dateData <- as.Date(dateData, format="%Y-%m-%d") 
    
    #Filters the data based on the requirements
    fPowerData <- powerData
    fPowerData$Date <- dateData
    fPowerData <- filter(fPowerData, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
    
    #Combines date and time
    fPowerData$DateTime <- as.POSIXct(paste(fPowerData$Date, fPowerData$Time), format="%Y-%m-%d %H:%M:%S")
    
    #Plots the line graph and saves it
    par(mar = c(2,4,4,4))
    plot(fPowerData$DateTime, as.numeric(fPowerData$Global_active_power), ylab = "Global Active Power (kilowatts)", type = "l")
    dev.copy(png, "plot2.png")
    dev.off()
}