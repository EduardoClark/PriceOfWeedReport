##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

### County Prices
# Read Location-County Data
Counties <- read.csv("data/CountyInfo.csv")
Counties$X <- NULL
Counties <- subset(Counties, is.na(Counties$County)==FALSE)
Counties <- unique(Counties)

##Remove Inconsistencies
Counties$County <- gsub("\\[1\\]", "", Counties$County)
Counties$County <- gsub("\\[2\\]", "", Counties$County)
Counties$County <- gsub("\\[3\\]", "", Counties$County)
Counties$County <- gsub("\\[4\\]", "", Counties$County)
Counties$County <- gsub("\\[\\]", "", Counties$County)
Counties$County <- gsub("County", "", Counties$County)
Counties$County <- gsub("Parish", "", Counties$County)
Counties$County <- gsub("Counties", "", Counties$County)
Counties$County <- gsub("Seat", "", Counties$County)
Counties$County <- gsub("City Type: City", "", Counties$County)


Counties$County <- gsub("City Type: Rural", "", Counties$County)
Counties$County <- gsub("City Type: Urban/Rural", "", Counties$County)
Counties$County <- gsub("City Type: City/Rural", "", Counties$County)
Counties$County <- gsub("/City", "", Counties$County)
Counties1 <- subset(Counties, grepl(" and ",Counties$County)==TRUE)
DoubleCounty <- strsplit(Counties1$County, " and ")
Counties1$County2 <- Counties1$County
for (i in 1:23){
  tmp1 <- DoubleCounty[[i]][1]
  tmp2 <- DoubleCounty[[i]][2]
  Counties1$County[i] <- tmp1
  Counties1$County2[i] <- tmp2
  remove(tmp1, tmp2)
}
Counties1 <- melt(Counties1, id.vars="City")
Counties1$variable <- NULL
Counties1$value <- as.character(Counties1$value)
Counties1 <- subset(Counties1, Counties1$value!="")
colnames(Counties1) <- c("City", "County")
Counties <- rbind(Counties, Counties1)
Counties <- subset(Counties, grepl(" and ",Counties$County)==FALSE)
remove(Counties1, DoubleCounty, i)
Counties$County <- str_trim(Counties$County, side="both")

letras <- expand.grid(letters,LETTERS)
letras$patterns1 <- paste(letras$Var1, letras$Var2, sep="")
patterns <- paste(letras$patterns1, collapse="|")

Counties1 <- subset(Counties, nchar(Counties$County) > 10)
Counties1 <- subset(Counties1, grepl(pattern=patterns, Counties1$County, ignore.case=FALSE))
splitter <- regexpr(pattern=patterns, Counties1$County)
Counties1$County1 <- substr(Counties1$County , splitter + 1, 100 )
Counties1$County <- substr(Counties1$County, 0, splitter)
splitter <- regexpr(pattern=patterns, Counties1$County1)
Counties1$County2 <- substr(Counties1$County1 , splitter + 1, 100 )
Counties1$County1 <- substr(Counties1$County1, 0, splitter)
splitter <- regexpr(pattern=patterns, Counties1$County2)
Counties1$County3 <- substr(Counties1$County2 , splitter + 1, 100 )
Counties1$County2 <- substr(Counties1$County2, 0, splitter)
Counties1 <- melt(Counties1, id.vars="City")
Counties1$variable <- NULL
colnames(Counties1) <- c("City", "County")
Counties1 <- subset(Counties1, Counties1$County != "")
Counties1$County <- as.character(Counties1$County)
Counties <- rbind(Counties, Counties1)
remove(Counties1, letras, patterns, splitter, let, let1)

UnMatched <- subset(Counties, Counties$County=="NULL" | County=="Orleans")
Counties <- subset(Counties, Counties$County != "NULL" & Counties$County !="Orleans")
Counties <- rbind(Counties, data.frame(City="Pekin, Illinois", County="Tazewell"))
Counties1 <- Counties
splitter <- strsplit(Counties1$County, " ")

Joiner <- data.frame(City=Counties1$City[1], County=splitter[[1]][1])
for (i in 1:9142){
  tmp <- data.frame(City=Counties1$City[i], County=splitter[[i]][1])
  Joiner <- rbind(Joiner, tmp)
  City=Counties1$City[i]
  tmp <- splitter[[i]]
  if (length(tmp > 1)) for (j in 2:length(tmp)){
    County <- tmp[j]
    tmp1 <- data.frame(City=City, County=County)
    Joiner <- rbind(Joiner, tmp1)
  }
}
remove(tmp, i , j, splitter, City, County, tmp1)
Joiner <- unique(Joiner)
Joiner <- subset(Joiner, is.na(Joiner$County) == FALSE)

Joiner$City <- as.character(Joiner$City)
Joiner$County <- as.character(Joiner$County)
Counties <- rbind(Counties, Joiner)
Counties <- unique(Counties)
Counties <- arrange(Counties, Counties$City)
remove(Joiner)

IndependentCities <- subset(Counties, grepl("Independent City", Counties$County, ignore.case=TRUE)==TRUE)
IndependentCities$County <- as.character(IndependentCities$County)
IndependentCities$County <- as.character(IndependentCities$City)
IndependentCitiesSplitter <- strsplit(IndependentCities$County, ",")
for (i in 1:8){
  tmp <- IndependentCitiesSplitter[[i]]
  tmp1 <- tmp[1]
  IndependentCities$County[i] <- tmp
}
remove(i, tmp, tmp1, IndependentCitiesSplitter)

Counties <- rbind(Counties, IndependentCities)
remove(IndependentCities, Counties1, UnMatched, Index)


# Asign each purchase a county of purchase
DBwCounties <- merge(DB, Counties, by.x="Location", by.y="City", all.x=TRUE, all.y=FALSE)
DBwCounties$CountyComplete <- paste(DBwCounties$County, ", ", DBwCounties$State, sep="")
##Get County Price Averages
#For Ounce Purchases
CountyAveragesO <- subset(DBwCounties, DBwCounties$Quantity == "an ounce")
CountyAverages <- ldply(tapply(CountyAveragesO$AdjustedOuncePrice, CountyAveragesO$CountyComplete, mean))
CountyAverages1 <- ldply(table(CountyAveragesO$CountyComplete)) ## Number of purchases per county
colnames(CountyAverages) <- c("County", "PriceAverage")
CountyAverages$Ns <- CountyAverages1$V1
remove(CountyAverages1, CountyAveragesO)
write.csv(DBwCounties, file="data-out/DBwCounties.csv", row.names=FALSE)
write.csv(CountyAverages, file="data-out/CountyPriceAverages.csv", row.names=FALSE)
