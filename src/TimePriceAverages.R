##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

###Price Index
StatesList <- data.frame(State=unique(DB$State))
Months <- as.character(unique(DB$Month)) ##Get Month List
Months <- sort(Months)

Averages <- function(Month){ ## Function to get monthly price average
  t <- Month
  tmp <- DB
  Ounces <- subset(tmp, tmp$Month == t)
  StateAverage <- ldply(tapply(Ounces$FinalAdjustedOuncePrice, Ounces$State, mean))
  colnames(StateAverage) <- c("State", "Price_Average")
  StateAverage <- merge(StateAverage, StatesList, by="State", all=TRUE)
  colnames(StateAverage) <- c("State", paste("AveragePrice_", Month, sep=""))
  return(StateAverage)
} 

###Get Monthly Averages
TimeSeries <- lapply(Months, Averages)
PriceTimeSeries <- ldply(TimeSeries[1])
for (i in 2:33){
  tmp <- ldply(TimeSeries[i])
  PriceTimeSeries <- merge(PriceTimeSeries, tmp, by=1, all=TRUE)
  remove(tmp)
}

###Write Price Time Series
write.csv(PriceTimeSeries, file="data-out/PriceTimeSeries.csv", row.names=FALSE)
write.csv(StateAverages, file="data-out/StatePrices.csv", row.names=FALSE)
remove(Months, TimeSeries, i, Averages)