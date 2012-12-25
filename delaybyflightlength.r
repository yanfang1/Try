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

#categorizing flight_length 
flight_length <- int_length(
  as.interval(
    flightHistory$scheduled_gate_departure, 
    flightHistory$scheduled_gate_arrival
  )
)/60

delayByFlightLength <- as.data.frame(cbind(flight_length,Runway.delayMins,Gate.delayMins))

delayByFlightLength <- delayByFlightLength[order(delayByFlightLength$flight_length, decreasing=FALSE),]


#plot
plot(delayByFlightLength$flight_length,delayByFlightLength$Runway.delayMins)

lm(delayByFlightLength$Runway.delayMins~delayByFlightLength$flight_length)


delayByFlightLength.OneHundred <- delayByFlightLength[delayByFlightLength$flight_length <= 100,]

plot(delayByFlightLength.OneHundred$flight_length,delayByFlightLength.OneHundred$Runway.delayMins)

glm(delayByFlightLength.OneHundred$Runway.delayMins~delayByFlightLength.OneHundred$flight_length)


delayByFlightLength.TwoHundred <- delayByFlightLength[delayByFlightLength$flight_length <= 200,]

plot(delayByFlightLength.TwoHundred$flight_length,delayByFlightLength.TwoHundred$Runway.delayMins)

glm(delayByFlightLength.TwoHundred$Runway.delayMins~delayByFlightLength.TwoHundred$flight_length)


delayByFlightLength.TwoToFourHundred <- delayByFlightLength[(delayByFlightLength$flight_length > 200)&(delayByFlightLength$flight_length <= 400),]

plot(delayByFlightLength.TwoToFourHundred$flight_length,delayByFlightLength.TwoToFourHundred$Runway.delayMins)

glm(delayByFlightLength.TwoToFourHundred$Runway.delayMins~delayByFlightLength.TwoToFourHundred$flight_length)


delayByFlightLength.ThreeToFourHundred <- delayByFlightLength[(delayByFlightLength$flight_length >= 300)&(delayByFlightLength$flight_length <= 400),]

plot(delayByFlightLength.ThreeToFourHundred$flight_length,delayByFlightLength.ThreeToFourHundred$Runway.delayMins)

glm(delayByFlightLength.ThreeToFourHundred$Runway.delayMins~delayByFlightLength.ThreeToFourHundred$flight_length)
