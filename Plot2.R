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

## Plotting Two (General Draw)

?plot
plot(joinPower$Global.active.power~joinPower$dateTime,type ="l",  ylab = "Global Active Power (kilowatts)", xlab = "")

## Saving to file
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()