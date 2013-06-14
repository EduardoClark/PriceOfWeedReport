##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

####Price Index
#State Use
MJIncidence <- read.csv("http://www.samhsa.gov/data/NSDUH/2k11State/NSDUHsaeExcelTab2-2011.csv", header=TRUE) ### Read NSDUH State Prevalence Estimates
MJIncidence$Table.2..Marijuana.Use.in.the.Past.Year..by.Age.Group.and.State..Percentages..Annual.Averages.Based.on.2010.and.2011.NSDUHs.. <- NULL
MJIncidence <- subset(MJIncidence, MJIncidence$X != (unique(MJIncidence$X)[1]))
MJIncidence <- subset(MJIncidence, select=1:2)
colnames(MJIncidence) <- c("State", "UserPer")
MJIncidence$State <- as.character(MJIncidence$State)

#State Population (From wikipedia)
StatePopulation <- ldply(readHTMLTable("http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population")[5])
StatePopulation <- subset(StatePopulation, select=c(4,5,13))
colnames(StatePopulation) <- c("State", "Population", "PercentagePopulation")
StatePopulation$Population <- gsub(",", "", StatePopulation$Population)
StatePopulation$Population <- as.numeric(StatePopulation$Population)
StatePopulation$State <- as.character(StatePopulation$State)
StatePopulation$State <- substring(StatePopulation$State, 2, 1000)

StatePopulation <- merge(MJIncidence, StatePopulation, by="State", all.x=TRUE, all.y=FALSE)
StatePopulation <- subset(StatePopulation, is.na(StatePopulation$Population )==FALSE)
StatePopulation$UserPer <- gsub("%", "", StatePopulation$UserPer)
StatePopulation$PercentagePopulation <- gsub("%", "", StatePopulation$PercentagePopulation)
StatePopulation$UserPer <- as.numeric(StatePopulation$UserPer)
StatePopulation$PercentagePopulation <- as.numeric(StatePopulation$PercentagePopulation)
StatePopulation$Weight <- StatePopulation$UserPer * StatePopulation$PercentagePopulation
StatePopulation$Weight <- StatePopulation$Weight / sum(StatePopulation$Weight)
remove(MJIncidence)

#Compute Weighted Averages
PriceTimeSeries$State <- substring(PriceTimeSeries$State, 2) ##Trim Leading Zero
PriceTimeSeries <- merge(PriceTimeSeries, StatePopulation, by="State", all.x=TRUE, all.y=FALSE)
remove(StatePopulation)
Index <- weighted.mean(unlist(PriceTimeSeries[2]), PriceTimeSeries$Weight, na.rm=TRUE)
for (i in 3:34){ ###Create Price Index Time Series
  tmp <- weighted.mean(unlist(PriceTimeSeries[i]), PriceTimeSeries$Weight, na.rm=TRUE)
  Index <- c(Index, tmp)
  remove(tmp)
}
remove(i)

### Compare Time Series of legalized States
Colorado <- t(subset(PriceTimeSeries, PriceTimeSeries$State=="Colorado", select=2:34))
Colorado <- as.vector(Colorado)

Washington <- t(subset(PriceTimeSeries, PriceTimeSeries$State=="Washington", select=2:34))
Washington <- as.vector(Washington)

California <- t(subset(PriceTimeSeries, PriceTimeSeries$State=="California", select=2:34))
California <- as.vector(California)

### TimE Series Graphs
Months <- sort(as.character(unique(DB$Month)))
G1 <- data.frame(Month=rep(Months, 4), State=c(rep("National", 33), rep("Colorado", 33),
                                                rep("Washington", 33), rep("California", 33)), PriceIndex=c(Index, Colorado, Washington, California))

## Smooth Curve
HPFilter <- c(unlist(hpfilter(G1$PriceIndex[1:33],freq=1)[2]), ## Include adjusted series using Hodrick-Prescott Filer
              unlist(hpfilter(G1$PriceIndex[34:66],freq=1)[2]),
              unlist(hpfilter(G1$PriceIndex[67:99],freq=1)[2]),
              unlist(hpfilter(G1$PriceIndex[100:132],freq=1)[2]))
G1$Adjusted <- HPFilter
remove(HPFilter)

ggplot(data=G1, aes(x=Month, y=Adjusted, group=State, color=State )) +
  geom_line() + ylim(0,400) + geom_point()
