require(rhbase)
library(lubridate)
library(digest)
library(data.table)




hostLoc = 'xxx.xxx.xxx.xxx'  #Give your server IP
port = 9090  #Default port for thrift service

hb.init(hostLoc, port,serialize=c("character")) # Important to have serialization as char otherwise the "raw bytes" are dumped

TABLE_NAME <- "fogDecDB"

hb.list.tables()
hb.describe.table(TABLE_NAME)


dataScotland <- readRDS("~/Dropbox/KNMIWork/scotland/ResultFeatures7_709.rds")
dataScotland[, fileName:=sapply(strsplit(filePath, '/'), "[[",8)]
dataScotland[, year:= lubridate::year(dateTime)]
dataScotland[, month:= lubridate::month(dateTime)]
dataScotland[, day:= lubridate::day(dateTime)]
dataScotland[, hour:= lubridate::hour(dateTime)]
dataScotland[, minute:= lubridate::minute(dateTime)]


dataScotland[, c("filePrefix","fileLocation","filePattern","id", "stationID"):=NULL]


dataScotland<-data.frame(dataScotland)


setnames(dataScotland, "filePath",as.character("FileProperties:FilePath"))
setnames(dataScotland, "imageFormat",as.character("FileProperties:FileType"))
#setnames(dataScotland, "LocationDescription","Location:LocationDescription")
setnames(dataScotland, "locationID",as.character("Location:LocationID"))
setnames(dataScotland, "lat",as.character("Location:Latitude"))
setnames(dataScotland, "lon",as.character("Location:Longitude"))
setnames(dataScotland, "dateTime",as.character("Time:DateTime"))
#setnames(dataScotland, "Dawn","Time:Dawn")
#setnames(dataScotland, "Dusk","Time:Dusk")
#setnames(dataScotland, "SunriseTime","Time:SunriseTime")
#setnames(dataScotland, "SunsetTime","Time:SunsetTime")
setnames(dataScotland, "year",as.character("Time:Year"))
setnames(dataScotland, "month",as.character("Time:Month"))
setnames(dataScotland, "day",as.character("Time:Day"))
setnames(dataScotland, "hour",as.character("Time:Hour"))
setnames(dataScotland, "minute",as.character("Time:Minute"))
setnames(dataScotland, "meanEdge",as.character("ImageFeatures:MeanEdge"))
setnames(dataScotland, "changePoint",as.character("ImageFeatures:ChangePoint"))
setnames(dataScotland, "smoothness",as.character("ImageFeatures:Smoothness"))
setnames(dataScotland, "fractalDim",as.character("ImageFeatures:FractalDim"))
setnames(dataScotland, "meanHue",as.character("ImageFeatures:MeanHue"))
setnames(dataScotland, "meanSaturation",as.character("ImageFeatures:MeanSaturation"))
setnames(dataScotland, "meanBrightness",as.character("ImageFeatures:MeanBrightness"))



rownames(dataScotland) <- lapply(dataScotland$fileName, digest, algo = "sha256")

dataScotland$fileName<-NULL


hb.insert.data.frame(TABLE_NAME, dataScotland)

#hb.insert(TABLE_NAME, list(list(digest(dataScotland$fileName[[3]], "sha256"), 
                                 #  c("FileProperties:FilePath"), list(dataScotland$filePath[[3]]))))
