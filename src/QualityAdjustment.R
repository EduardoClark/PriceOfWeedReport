##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

### Quality Adjustment

StateAverages$HQ_LQ <- StateAverages$HQ / StateAverages$LQ #  High Quality to Low Quality price difference
StateAverages$HQ_MQ <- StateAverages$HQ / StateAverages$MQ #  High Quality to Mid Quality price difference
QualityAdjustment <- data.frame(State=StateAverages$State, HQLQ=StateAverages$HQ_LQ,
                                HQMQ=StateAverages$HQ_MQ) ##DF with only conversion 

### Merge QA with DB
DB <- merge(DB, QualityAdjustment, by="State", all=TRUE)

### Create Adjusted Ounce Prices
DB$AdjustedOuncePrice <- ifelse(DB$Quality=="low quality", DB$OuncePrice * DB$HQLQ, DB$OuncePrice)
DB$AdjustedOuncePrice <- ifelse(DB$Quality=="medium quality", DB$OuncePrice * DB$HQMQ, DB$AdjustedOuncePrice)

### Get Adjusted State Averages 

StateAverage <- function(Location){
  tmp <- Location
  tmp1 <- subset(DB, DB$State == tmp)
  tmp2 <- mean(tmp1$AdjustedOuncePrice)
  tmp5 <- data.frame(State=tmp, PriceAverage=tmp2)
  return(tmp5)
}

### Apply State Average Function to all states
AdjustedStateAverages <- ldply(lapply(StatesList, StateAverage))

###Write CSV
write.csv(QualityAdjustment, file="data-out/StateQualityAdjustment.csv", row.names=FALSE)
write.csv(AdjustedStateAverages, file="data-out/QualityAdjustmentStateAverages.csv", row.names=FALSE)
remove(QualityAdjustment, StateAverage, StateAverages)

