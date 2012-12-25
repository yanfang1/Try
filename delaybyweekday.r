#reference: "delaybyairline.R" by David Leen
library(stringr)
library(lubridate)

flightHistory1 <- read.csv("I:\\GE Flight Quest Data\\InitialTrainingSet_rev1\\2012_11_12\\FlightHistory\\flighthistory.csv")
flightHistory2 <- read.csv("I:\\GE Flight Quest Data\\InitialTrainingSet_rev1\\2012_11_13\\FlightHistory\\flighthistory.csv")
flightHistory3 <- read.csv("I:\\GE Flight Quest Data\\InitialTrainingSet_rev1\\2012_11_14\\FlightHistory\\flighthistory.csv")
flightHistory4 <- read.csv("I:\\GE Flight Quest Data\\InitialTrainingSet_rev1\\2012_11_15\\FlightHistory\\flighthistory.csv")
flightHistory5 <- read.csv("I:\\GE Flight Quest Data\\InitialTrainingSet_rev1\\2012_11_16\\FlightHistory\\flighthistory.csv")
flightHistory6 <- read.csv("I:\\GE Flight Quest Data\\InitialTrainingSet_rev1\\2012_11_17\\FlightHistory\\flighthistory.csv")
flightHistory7 <- read.csv("I:\\GE Flight Quest Data\\InitialTrainingSet_rev1\\2012_11_18\\FlightHistory\\flighthistory.csv")

flightHistoryTotal=rbind(flightHistory1,flightHistory2,flightHistory3,flightHistory4,flightHistory5,flightHistory6,flightHistory7)

# Replace missing values with NA
flightHistoryTotal[flightHistoryTotal == "MISSING"] = NA

# Remove rows where any of the flight times are NA, other columns
# being NA is ok for this file.
flightHistoryTotal <- flightHistoryTotal[complete.cases(flightHistoryTotal[,c(3,9:18)]),]

for(i in 9:18) {
  flightHistoryTotal[,i] <- ymd_hms(str_sub(flightHistoryTotal[,i], 1, 19))
}

Runway.delayMins <- int_length(
  as.interval(
    flightHistoryTotal$scheduled_runway_arrival, 
    flightHistoryTotal$actual_runway_arrival
  )
)/60

Gate.delayMins <- int_length(
  as.interval(
    flightHistoryTotal$scheduled_gate_arrival, 
    flightHistoryTotal$actual_gate_arrival
  )
)/60

meanDelayByWeekday <- 
  aggregate(Runway.delayMins ~ weekdays(flightHistoryTotal$published_departure), FUN = mean)

names(meanDelayByWeekday) <- c("Weekday", "Runway.delayMins")

meanDelayByWeekday=meanDelayByWeekday[order(meanDelayByWeekday$Weekday,decreasing=T),]

mde <- meanDelayByWeekday$Runway.delayMins
nam <- meanDelayByWeekday$Weekday

barplot(height=mde, names.arg=nam, las=1, horiz=T)

plot(nam,mde)