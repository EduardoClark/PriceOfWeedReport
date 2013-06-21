###Run All for PoW Report
source("src/LoadLibraries.R", echo=FALSE, verbose=FALSE) ##Load Libraries
source("src/Preprocess.R", echo=FALSE, verbose=FALSE) ## Preprocess and Include Equivalencies
source("src/StateAverages.R") ## Get state averages over quality
source("src/QualityAdjustment.R") ## Quality Adjustment
source("src/QuantityDiscoutn.R") ## Quantity Adjustment
source("src/TimePriceAverages.R") ## State Price Averages and TimeSeries for State Price Averages
source("src/PriceIndex.R") ##Generate Price Index
source("src/CountyPrices.R") ## Agregate County Prices
source("src/CountyPriceMap.R") ## Create SHP for County Price Map


