
library(rgdal)

shp = "inst/extdata/Snelwegen/NDWApril2016WGS84/NDW April 2016 WGS84.shp"
tmp <- readOGR(shp, layer = "NDW April 2016 WGS84")

wegnummer <- c("001", "004", "006", "007", "009",
               "012", "015",
               "027", "028", "027",
               "032",
               "050", "059",
               "067",
               "073")

plot(tmp[tmp$WEGNUMMER %in% wegnummer, ])
