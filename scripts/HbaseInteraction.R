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


dataFeatures <- readRDS("~/Dropbox/KNMIWork/scotland/ResultFeatures7_709.rds")
dataFeatures[, fileName:=sapply(strsplit(filePath, '/'), "[[",8)]
dataFeatures[, year:= lubridate::year(dateTime)]
dataFeatures[, month:= lubridate::month(dateTime)]
dataFeatures[, day:= lubridate::day(dateTime)]
dataFeatures[, hour:= lubridate::hour(dateTime)]
dataFeatures[, minute:= lubridate::minute(dateTime)]


dataFeatures[, c("filePrefix","fileLocation","filePattern","id", "stationID"):=NULL]


dataFeatures<-data.frame(dataFeatures)


setnames(dataFeatures, "filePath",as.character("FileProperties:FilePath"))
setnames(dataFeatures, "imageFormat",as.character("FileProperties:FileType"))
setnames(dataFeatures, "LocationName","Location:LocationDescription")
setnames(dataFeatures, "locationID",as.character("Location:LocationID"))
setnames(dataFeatures, "lat",as.character("Location:Latitude"))
setnames(dataFeatures, "lon",as.character("Location:Longitude"))
setnames(dataFeatures, "dateTime",as.character("Time:DateTime"))
#setnames(dataFeatures, "Dawn","Time:Dawn")
#setnames(dataFeatures, "Dusk","Time:Dusk")
#setnames(dataFeatures, "SunriseTime","Time:SunriseTime")
#setnames(dataFeatures, "SunsetTime","Time:SunsetTime")
setnames(dataFeatures, "year",as.character("Time:Year"))
setnames(dataFeatures, "month",as.character("Time:Month"))
setnames(dataFeatures, "day",as.character("Time:Day"))
setnames(dataFeatures, "hour",as.character("Time:Hour"))
setnames(dataFeatures, "minute",as.character("Time:Minute"))
setnames(dataFeatures, "meanEdge",as.character("ImageFeatures:MeanEdge"))
setnames(dataFeatures, "changePoint",as.character("ImageFeatures:ChangePoint"))
setnames(dataFeatures, "smoothness",as.character("ImageFeatures:Smoothness"))
setnames(dataFeatures, "fractalDim",as.character("ImageFeatures:FractalDim"))
setnames(dataFeatures, "meanHue",as.character("ImageFeatures:MeanHue"))
setnames(dataFeatures, "meanSaturation",as.character("ImageFeatures:MeanSaturation"))
setnames(dataFeatures, "meanBrightness",as.character("ImageFeatures:MeanBrightness"))



rownames(dataFeatures) <- lapply(dataFeatures$fileName, digest, algo = "sha256")

dataFeatures$fileName<-NULL


hb.insert.data.frame(TABLE_NAME, dataFeatures)

#hb.insert(TABLE_NAME, list(list(digest(dataFeatures$fileName[[3]], "sha256"), 
                                 #  c("FileProperties:FilePath"), list(dataFeatures$filePath[[3]]))))
