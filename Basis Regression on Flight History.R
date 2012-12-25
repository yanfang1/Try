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



###

start=ymd_hms(paste(str_sub(flightHistory[,9], 1, 10),"00:00:00"))

#RegModel=matrix(0,dim(flightHistory)[1],3+6+6)
RegModel=flightHistory[,c(2,5,7,9,11,12,16,18,14)]
colnames(RegModel)=c("Airline Code","Departure Airport Code","Arrival Airport Code","Published Departure",
                     "Scheduled Gate Departure","Actual Gate Departure","Actual Runway Departure","Actual Runway Arrival",
                     "Actual Gate Arrival")
dim(RegModel)


for(i in 4:9)
{
  RegModel[,i]=int_length(as.interval(start,RegModel[,i]))/60
}

for(i in 1:3)
{
  RegModel[,i]=strtoi(RegModel[,i],35L)
}

Reg.Runway=glm(RegModel[,8]~RegModel[,1]+RegModel[,2]+RegModel[,3]+RegModel[,4]+RegModel[,6]+RegModel[,7])
Reg.Runway

#lm(RegModel[,8]~RegModel[,5])


Reg.Gate=glm(RegModel[,9]~RegModel[,1]+RegModel[,2]+RegModel[,3]+RegModel[,4]+RegModel[,6]+RegModel[,7])
Reg.Gate


