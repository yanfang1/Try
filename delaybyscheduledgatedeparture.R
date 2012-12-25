#reference: "delaybyairline.R" by David Leen
library(stringr)
library(lubridate)

flightHistory <- read.csv("flighthistory.csv")
# Replace missing values with NA
flightHistory[flightHistory == "MISSING"] = NA

# Remove rows where any of the flight times are NA, other columns
# being NA is ok for this file.
flightHistory <- flightHistory[complete.cases(flightHistory[,c(3,9:18)]),]

for(i in 9:18) {
  flightHistory[,i] <- ymd_hms(str_sub(flightHistory[,i], 1, 19))
}

Runway.delayMins <- int_length(
  as.interval(
    flightHistory$scheduled_runway_arrival, 
    flightHistory$actual_runway_arrival
  )
)/60

Gate.delayMins <- int_length(
  as.interval(
    flightHistory$scheduled_gate_arrival, 
    flightHistory$actual_gate_arrival
  )
)/60

delayByScheduledGateDeparture <- as.data.frame(cbind(hour(flightHistory$scheduled_gate_departure),Runway.delayMins,Gate.delayMins))

names(delayByScheduledGateDeparture)[1] <- "scheduled_gate_departure"

delayByScheduledGateDeparture <- delayByScheduledGateDeparture[order(delayByScheduledGateDeparture$scheduled_gate_departure, decreasing=FALSE),]


#plot
plot(delayByScheduledGateDeparture$scheduled_gate_departure,delayByScheduledGateDeparture$Runway.delayMins)

glm(delayByScheduledGateDeparture$Runway.delayMins~delayByScheduledGateDeparture$scheduled_gate_departure)


delayByScheduledGateDeparture.Ten <- delayByScheduledGateDeparture[delayByScheduledGateDeparture$scheduled_gate_departure <= 10,]

plot(delayByScheduledGateDeparture.Ten$scheduled_gate_departure,delayByScheduledGateDeparture.Ten$Runway.delayMins)

glm(delayByScheduledGateDeparture.Ten$Runway.delayMins~delayByScheduledGateDeparture.Ten$scheduled_gate_departure)


delayByScheduledGateDeparture.GreaterThanTen <- delayByScheduledGateDeparture[delayByScheduledGateDeparture$scheduled_gate_departure > 10,]

plot(delayByScheduledGateDeparture.GreaterThanTen$scheduled_gate_departure,delayByScheduledGateDeparture.GreaterThanTen$Runway.delayMins)

glm(delayByScheduledGateDeparture.GreaterThanTen$Runway.delayMins~delayByScheduledGateDeparture.GreaterThanTen$scheduled_gate_departure)
