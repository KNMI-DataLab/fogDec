
library(rgdal)

shp = "inst/extdata/Snelwegen/NDWApril2016WGS84/NDW_April_2016_WGS84.shp"
tmp <- readOGR(shp, layer = "NDW_April_2016_WGS84")

wegnummer <- c("001", "004", "006", "007", "009",
               "012", "015",
               "027", "028", "027",
               "032",
               "050", "059",
               "067",
               "073")

plot(tmp[tmp$WEGNUMMER %in% wegnummer, ])
