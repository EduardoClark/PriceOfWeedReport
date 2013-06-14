##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

### Create State Average Functions

DB$State <- substr(DB$State, 2, 1000) ##Trim leading Zero
DB <- subset(DB, DB$OuncePrice < 1000 & DB$OuncePrice > 50)
StatesList <- as.character(unique(DB$State)) #Create a State List
StatesList <- sort(StatesList)


###State Average Function

StateAverage <- function(Location){
  tmp <- Location
  tmp1 <- subset(DB, DB$State == tmp)
  tmp2 <- subset(tmp1, tmp1$Quality=="high quality")
  tmp3 <- subset(tmp1, tmp1$Quality=="medium quality")
  tmp4 <- subset(tmp1, tmp1$Quality=="low quality")
  tmp2 <- mean(tmp2$OuncePrice)
  tmp3 <- mean(tmp3$OuncePrice)
  tmp4 <- mean(tmp4$OuncePrice)
  tmp5 <- data.frame(State=Location, HQ=tmp2, MQ=tmp3,LQ=tmp4)
  return(tmp5)
}

### Apply State Average Function to all states
StateAverages <- ldply(lapply(StatesList, StateAverage))

### Graph State Average density plots
dens1 <- data.frame(Price=c(StateAverages$HQ, StateAverages$MQ, StateAverages$LQ),
                    Quality=c(rep("HQ", 50), rep("MQ", 50), rep("LQ", 50)))
Cairo(600, 600, file="plots/StateAverageDensity.png", type="png", bg="white")
ggplot(dens1, aes(x = Price, fill = Quality)) + geom_density(alpha = 0.5) +
  ylab("Densidad") + xlab("Average Price") + xlim(0,500)
dev.off()

remove(StateAverage, dens1)
