##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

### Quantity Adjustment

Quantities <- as.character(unique(DB$Quantity))

###Create New Column With Price per Kilo

### Quantity Bought & Price
DB$GramsXTransaction <-  28.3495231 / DB$Equivalent  ###Grams per Transaction
DB$GramsXTransaction1 <- 1 / DB$GramsXTransaction
z.out <- zelig(Price  ~ as.factor(GramsXTransaction1) + as.factor(Quality) + as.factor(State) ,
               model = "ls", data = DB)
summary(z.out)

### Adjust Prices due to quantity & Quality bought
Quality <- data.frame(Quality=c("high quality", "medium quality", "low quality"), QualAdjust=c(1,43.2181,87.6484))
Quantity <- data.frame(GramsXTransaction1= unique(DB$GramsXTransaction1),
                       QuantAdjust = c(0,-57.5851,102.3888, 112.1085, 102.6041, 
                                       154.5043, 162.4507, 173.1734,196.3259,
                                       227.4090 ))
DB <- merge(DB, Quality, by="Quality", all.x=TRUE, all.y=FALSE)
DB <- merge(DB, Quantity, by="GramsXTransaction1", all.x=TRUE, all.y=FALSE) 
DB$FinalAdjustedOuncePrice <- DB$Price + DB$QualAdjust + DB$QuantAdjust

Cairo(600, 600, file="plots/Mordor.png", type="png", bg="white")
ggplot(DB, aes(x = FinalAdjustedOuncePrice)) + geom_density(alpha = 0.5) +
  ylab("Densidad") + xlab("Adjusted Ounce Price") + xlim(0,500)
dev.off()


### Final State Averages
  
StateAverageQuantities <- function(State){
    tmp <- State
    tmp1 <- subset(DB, DB$State== tmp)
    tmp4 <- mean(tmp1$FinalAdjustedOuncePrice, na.rm=TRUE)
    tmp6 <- data.frame(State=tmp, PriceAverage=tmp4)
  }

StateAverages <- ldply(lapply(StatesList, StateAverageQuantities))
remove(StateAverageQuantities, z.out, Quality, Quantity)