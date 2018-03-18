initFirstPlot <- function() {
#Initializes the libraries required for this script
    library(dplyr)
    library(datasets)
    
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
    
    #Plots the histogram and saves it
    png("plot1.png")
    hist(as.numeric(fPowerData[,3]), xlab = "Global Active Power (kilowatts)", col = "orange", main = "Global Active Power")
    dev.off()
}