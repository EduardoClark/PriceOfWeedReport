##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

### Add County Prices to Shapefile
CountiesSH = readOGR(dsn="data/CountyShapes/", layer="UScounties") ##Read County SHP file
CountiesSH@data$County <- paste(CountiesSH@data$NAME, ", ", CountiesSH@data$STATE_NAME, sep="") ## Add county column for merger
CountiesSH@data <- merge(CountiesSH@data, CountyAverages, by="County", all.x=TRUE, all.y=FALSE) ## Merge with county price averages
writeOGR(CountiesSH,dsn="data-out/CartoDB/", layer="UScounties", driver="ESRI Shapefile", overwrite_layer=TRUE) ## Write new Shapefile
system("cd data-out/; zip -r CountySH.zip CartoDB") ## Create Zip with Data 
CountySHData <- CountiesSH@data
CountySHData$PriceAverage <- round(CountySHData$PriceAverage, 1)
CountySHData$PriceAverage1 <- paste("$", round(CountiesSH$PriceAverage, 0), " per Ounce", sep="")
CountySHData$Ns <- paste(CountiesSH$Ns, " observations registered", sep="")
StateAverages$State <- as.character(StateAverages$State)
CountySHData <- merge(CountySHData, StateAverages,by.x="STATE_NAME" , by.y="State", all.x=TRUE, all.y=FALSE)
CountySHData$PriceAverage1 <- ifelse(is.na(CountySHData$PriceAverage.x)==TRUE, 
                                     paste( "$", round(CountySHData$PriceAverage.y, 0),
                                            " per Ounce (State Average)", sep=""),
                                     CountySHData$PriceAverage1 )
CountySHData$Ns <- ifelse(is.na(CountySHData$PriceAverage.x)==TRUE, 
                          "No observations registered. Price is State Average",
                          CountySHData$Ns )
CountySHData$PriceAverage.x <- ifelse(is.na(CountySHData$PriceAverage.x)==TRUE, CountySHData$PriceAverage.y, CountySHData$PriceAverage.x)



write.csv(CountySHData,"data-out/CountyDataForShape.csv", row.names=FALSE)

CountiesSH@data <- CountySHData
writeOGR(CountiesSH,dsn="data-out/CartoDB/", layer="UScounties", driver="ESRI Shapefile", overwrite_layer=TRUE) ## Write new Shapefile
system("cd data-out/; zip -r CountySH.zip CartoDB") ## Create Zip with Data 

remove(Counties, CountyAverages, CountiesSH)

