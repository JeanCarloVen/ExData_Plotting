library(lubridate)
library(dplyr)

##rm(list = ls())

## Exploratory Data Analysis


setwd("C:/Users/Lenovo Y50/Downloads/PERSONAL/CURSOS/ESPECIALIZACION DATA SCIENCE JOHN HOPKINGS/Coursera/MODULO4/HOMEWORKS")

temp <- tempfile()

ArchivoZip = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

download.file(ArchivoZip, temp)

ArchivoTXT = unz(temp, "household_power_consumption.txt")

dataPowerConsuption <- read.csv(file=ArchivoTXT, header=TRUE, sep=";")


remove(ArchivoTXT)
unlink(temp)
remove(temp)
remove(ArchivoZip)


head(dataPowerConsuption)
dim(dataPowerConsuption)
tail(dataPowerConsuption)


head(dataPowerConsuption$Date)
str(dataPowerConsuption$Date)

## We use Lubridate to parsing date and Time
dataPowerConsuption$Date<-dmy(dataPowerConsuption$Date)
class(dataPowerConsuption$Date) ## Class Date


dataPowerConsuption$Date[which(dataPowerConsuption$Date == "?"),]
PowerConsuption01 <- filter(dataPowerConsuption, dataPowerConsuption$Date == "2007-02-01")
PowerConsuption02 <- filter(dataPowerConsuption, dataPowerConsuption$Date == "2007-02-02")
dim(PowerConsuption01) ## 1440 x 9
dim(PowerConsuption02) ## 1440 x 9

joinPowerConsuption<- bind_rows(PowerConsuption01, PowerConsuption02)
dim(joinPowerConsuption) ## 2880 x 9
head(joinPowerConsuption)
View(joinPowerConsuption)


## Cambio de nombre a las columnas
## Formas:
## colnames(x) <- c(""No.","Value"")
## colnames(x) <- gsub("_", "", colnames(x))

colnames(joinPowerConsuption)<- gsub("_", ".", colnames(joinPowerConsuption))

colnames(joinPowerConsuption)


## Fixing Data Classes
joinPowerConsuption$Time<- as.character(joinPowerConsuption$Time)
joinPowerConsuption$Global.active.power<- as.numeric(joinPowerConsuption$Global.active.power)
joinPowerConsuption$Global.reactive.power<- as.numeric(joinPowerConsuption$Global.reactive.power)
joinPowerConsuption$Voltage<- as.numeric(joinPowerConsuption$Voltage)
joinPowerConsuption$Global.intensity<- as.numeric(joinPowerConsuption$Global.intensity)
joinPowerConsuption$Sub.metering.1<- as.numeric(joinPowerConsuption$Sub.metering.1)
joinPowerConsuption$Sub.metering.2<- as.numeric(joinPowerConsuption$Sub.metering.2)

## Remove incomplete observation
joinPower<- joinPowerConsuption[complete.cases(joinPowerConsuption),]
str(joinPowerConsuption)

# Paste Time and Date

DateTime <- paste(joinPower$Date, joinPower$Time)

## Name de Column 

dateTime <- setNames(DateTime, "Date.Time")

## Remove time and date column

View(joinPower)
joinPower <- joinPower[,!(names(joinPower) %in% c("Date", "Time"))]

## Add DateTime column

joinPower <- cbind(dateTime, joinPower)
str(joinPower)

## Format DateTime

joinPower$dateTime <- as.POSIXct(dateTime)

## Plotting One (Historigram)

hist(joinPower$Global.active.power, main = "Global Actve Power", xlab = "Global Active Power (kilowatts)", col = "red")
abline(h = 1000, col = "gray60", lty = 3)

## Save file 
dev.copy(png,"plot1.png", width=480, height=480)

## and close device
dev.off()

## Plotting Two (General Draw)

?plot
plot(joinPower$Global.active.power~joinPower$dateTime,type ="l",  ylab = "Global Active Power (kilowatts)", xlab = "")


## Plotting Three (ScatterPlot)
?with

with(joinPower, {
  plot(Sub.metering.1 ~ dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub.metering.2~ dateTime,col='Red')
  lines(Sub.metering.3~ dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

## Saving to file
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()


## Create Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(joinPower, {
  plot(Global.active.power ~ dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage ~ dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub.metering.1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub.metering.2 ~ dateTime,col='Red')
  lines(Sub.metering.3 ~ dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub metering 1", "Sub metering 2", "Sub metering 3"))
  plot(Global.reactive.power ~ dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})


## Saving to file
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()