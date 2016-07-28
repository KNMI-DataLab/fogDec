library(data.table)
library(visDec)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=8)
library(imager)
library(changepoint) # functionality should be included in imager
library(maptools)
#library(lubridate)



detect.edges <- function(im,sigma=1) {
  # adapted from http://dahtah.github.io/imager/foreground_background.html
  isoblur(im,sigma) %>% imgradient("xy") %>% llply(function(v) v^2) %>%
    add %>% imsplit("c") %>% add 
}



runScript <-function() {

path <- "../inst/extdata/Meetterrein"
path2 <- "/mnt/disks/dataDisk/data/twente/"
filenames <- list.files(path2, recursive = T,
                        #pattern=glob2rx("Meetterrein_201510*.jpg"),
                        pattern=glob2rx("EHTW_201512*.jpg"),
                        full.names=TRUE)


imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
  FileNameParser(file, "na*me_yyyymmddhhmm.jpg")#Twente pattern
}

daylightImages <- FilterDayLightHours(imageSummary)[isDay == TRUE]


ReturnFeatures <- function(filePath) {
  im <- subim(load.image(filePath), y > 16) 
  imT <- RGBtoHSV(im)
  list(meanEdge = detect.edges(im, 3) %>% sqrt %>% mean,
       changePoint = cpts(cpt.mean(GetHorizAvgTrans(im), penalty = "None")),
       meanHue = mean(channel(imT, 1)),
       meanSaturation = mean(channel(imT, 2)),
       meanBrightness = mean(channel(imT, 3)) )
}

featureNames <- c("meanEdge", "changePoint", "meanHue", "meanSaturation",
                  "meanBrightness")


daylightImages[, id := 1:.N]
setkey(daylightImages, id)

imageSummary <- foreach(id = iter(daylightImages[, id]), .packages = c('data.table'), .combine = rbind) %dopar% {
  tmp <- daylightImages[id, ]
  tmp[, eval(featureNames) := ReturnFeatures(filePath), by = dateTime]
}


sensorFiles <- list.files("inst/extdata/Sensor",
                          pattern=glob2rx("Twente*.csv"),
                          full.names=TRUE)
sensorData <- ReadMORSensorData(sensorFiles)
setkey(sensorData, dateTime)
setkey(imageSummary, dateTime)
imageSummary <- merge(imageSummary, sensorData)
imageSummary[, MOR := TOA.MOR_10, by = dateTime]

stopImplicitCluster()

save(imageSummary, file = "/mnt/disks/dataDisk/data/twenteResults2016.RData")
return(imageSummary)
}
