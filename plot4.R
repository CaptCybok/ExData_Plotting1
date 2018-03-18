#Initializes the libraries required for this script
library(dplyr)
library(datasets)

initFourthPlot <- function() {
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
    
    #Allows 4 plots
    par(mfrow = c(2,2))
    perDay <- fPowerData[,10]
        
    #Do 1st plot
    plot(as.double(fPowerData[,3]), xaxt = "n", type = "n", ylab = "Global Active Power")
    lines(as.double(fPowerData[,3]), type = "l")
    axis(1, at=c(1,length(perDay)), lab=c(perDay[1], perDay[length(perDay)]))
    
    #Do 2nd plot
    plot(as.double(fPowerData[,5]), xaxt = "n", type = "n", ylab = "Voltage");
    lines(as.double(fPowerData[,5]), type = "l");
    axis(1, at=c(1,length(perDay)), lab=c(perDay[1], perDay[length(perDay)]))
    
    #Do 3rd plot
    plot(as.double(fPowerData[,7]), xaxt = "n", type = "n", ylab = "Energy Sub-metering")
    lines(as.double(fPowerData[,9]), col = "blue")
    lines(as.double(fPowerData[,8]), col = "red")
    lines(as.double(fPowerData[,7]), col = "black")
    axis(1, at=c(1,length(perDay)), lab=c(perDay[1], perDay[length(perDay)]))
    legend("topright", legend = c("Sub-metering 1", "Sub-metering 2", "Sub-metering 3"), lty = 1, col = c("black", "red", "blue"))
    
    #Do 4th plot
    plot(as.double(fPowerData[,4]), xaxt = "n", type = "n", ylab = "Global Reactive Power")
    lines(as.double(fPowerData[,4]), type = "l")
    axis(1, at=c(1,length(perDay)), lab=c(perDay[1], perDay[length(perDay)]))
    
    #Saves the plot
    dev.copy(png, "plot4.png")
    dev.off()
}