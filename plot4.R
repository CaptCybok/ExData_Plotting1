#Initializes the libraries required for this script
library(dplyr)
library(datasets)

initFourthPlot <- function() {
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
    
    #Allows 4 plots
    par(mfrow = c(2,2))
    par(mar = c(4,2,2,2))
    
    #Do 1st plot
    plot(fPowerData$DateTime, as.double(fPowerData$Global_active_power), type = "l", xlab = "", ylab = "Global Active Power")
    
    #Do 2nd plot
    plot(fPowerData$DateTime, as.double(fPowerData$Voltage), type = "l", xlab = "datetime", ylab = "Voltage");
    
    #Do 3rd plot
    plot(fPowerData$DateTime, as.double(fPowerData$Sub_metering_1), type = "n", xlab = "", ylab = "Energy Sub-metering")
    lines(fPowerData$DateTime, as.double(fPowerData$Sub_metering_1), col = "black")
    lines(fPowerData$DateTime, as.double(fPowerData$Sub_metering_2), col = "red")
    lines(fPowerData$DateTime, as.double(fPowerData$Sub_metering_3), col = "blue")
    legend("topright", legend = c("Sub-metering 1", "Sub-metering 2", "Sub-metering 3"), lty = 1, col = c("black", "red", "blue"))
    
    #Do 4th plot
    plot(fPowerData$DateTime, as.double(fPowerData$Global_reactive_power), type = "l", xlab = "datetime", ylab = "Global Reactive Power")

    #Saves the plot
    dev.copy(png, "plot4.png")
    dev.off()
}