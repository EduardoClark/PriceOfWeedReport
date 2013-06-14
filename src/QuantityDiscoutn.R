##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

### Quantity Adjustment

Quantities <- as.character(unique(DB$Quantity))


### Quantity Bought & Price
DB$GramsXTransaction <-  28.3495231 / DB$Equivalent  ###Grams per Transaction
z.out <- zelig(OuncePrice ~ GramsXTransaction + as.factor(State) + as.factor(Quality),
               model = "ls", data = DB)
summary(z.out)

### Adjust Prices due to quantity bought

DB$FinalAdjustedOuncePrice <- DB$AdjustedOuncePrice - (5.5313 * DB$GramsXTransaction)
remove(Quantities, AdjustedStateAverages)
 
### Final State Averages
  
StateAverageQuantities <- function(State){
    tmp <- State
    tmp1 <- subset(DB, DB$State== tmp)
    tmp4 <- mean(tmp1$FinalAdjustedOuncePrice, na.rm=TRUE)
    tmp6 <- data.frame(State=tmp, PriceAverage=tmp4)
  }

StateAverages <- ldply(lapply(StatesList, StateAverageQuantities))
remove(StateAverageQuantities, z.out)