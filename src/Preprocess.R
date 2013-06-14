##################################
## Author:Eduardo Clark
## Proyect: Price of Weed Report
## Date: June, 2013
##################################

###Remove Outliers and Create Ounce Equivalencies

### Read Database
DB <- read.csv("data/CompleteDBMay.csv")
DB$X <- NULL ##Remove row name
DB$Equivalency <- NULL
DB$OuncePrice <- NULL
### Remove non-credible observations
QQ <- unique(paste(DB$Quantity, DB$Quality, sep="/")) ## Create unique Quality/Quantity Combinations
DB$QQ <- paste(DB$Quantity, DB$Quality, sep="/")
for (i in 1:length(QQ)){# Remove all those observation further than 3 SDs from the group mean
  ss <- QQ[i]
  tmp <- subset(DB, DB$QQ == ss)
  tmpSD <- mean(tmp$Price, na.rm=TRUE) + (3* sd(tmp$Price, na.rm=TRUE))
  DB$Drop <- ifelse(DB$QQ==ss & DB$Price >= tmpSD, 1, 0)
  DB <- subset(DB, DB$Drop==0) 
  remove(tmp, ss, tmpSD)
} 
remove(i)
DB$Drop <- NULL

### Include Ounce Equivalency

Quantities <- unique(DB$Quantity)
WeirdQuantity <- as.character(Quantities[10])
DB <- subset(DB, DB$Quantity != WeirdQuantity)
Quantities <- unique(DB$Quantity)
remove(WeirdQuantity)
DB <- subset(DB, DB$Price != 0) 

## Create a DF with ounce conversion for each quantity
Equivalency <- data.frame(Quantity=Quantities, Equivalent=c(2.83495,1.889968, 
                                                            1.417476,1.133981,5.669904,
                                                            2,8,1,4,28.3495231))
###Merge with DB
DB <- merge(DB, Equivalency, all=TRUE, by="Quantity")
DB$OuncePrice <- DB$Price * DB$Equivalent ###This Column has the ounce price for all quantities
remove(QQ, Quantities, Equivalency)

### Write New CSV with Equivalency prices and Droped Outliers
write.csv(DB, file="data-out/CleanDB.csv", row.names=FALSE)

###Density Comparison Graphs
#for(i in 1:31){
#  name <- paste("P",i,sep="")
#  tmp2 <- QQ[i]
#  tmp <- subset(DB, DB$QQ==tmp2)
#  tmp1 <-   ggplot(tmp, aes(x=OuncePrice)) + geom_density() + labs(title=tmp2) + xlim(0,600)
#  assign(name, tmp1)
#}
#remove(tmp, tmp1, tmp2, name, i)
#multiplot( P21, P22, P23)



