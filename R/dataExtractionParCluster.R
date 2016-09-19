library(data.table)
library(visDec)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=2)
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
  
#createParallelCluster()

path1 <- "~/efs/data"
path2 <- "/mnt/disks/dataDisk/data/twente/"
filenames <- list.files(path1, recursive = T,
                        pattern=glob2rx("Meetterrein_2015*.jpg"),
                        #pattern=glob2rx("EHTW_201512*.jpg"),
                        full.names=TRUE)


imageSummary <- foreach(file = iter(filenames), .combine = rbind, .packages = "visDec") %dopar% {
  FileNameParser(file, "na*me_yyyymmdd_hhmm.jpg")#DeBilt pattern
}

daylightImages <- FilterDayLightHours(imageSummary, properties, 0, 0)

ReturnFeatures <- function(filePath) {
  im <- subim(load.image(filePath), y > 16) 
  imT <- RGBtoHSV(im)
  transmission <- GetHorizAvgTrans(im)
  list(meanEdge = DetectMeanEdges(im, 3),
       changePoint = TransmissionChangepoint(transmission),
       smoothness = TransmissionSmoothness(transmission),
       meanHue = mean(channel(imT, 1)),
       meanSaturation = mean(channel(imT, 2)),
       meanBrightness = mean(channel(imT, 3)) )
}

featureNames <- c("meanEdge", "changePoint", "smoothness",
                  "meanHue", "meanSaturation", "meanBrightness")



daylightImages[, id := 1:.N]
setkey(daylightImages, id)

imageSummary <- foreach(id = iter(daylightImages[, id]), .packages = c('data.table', 'imager','visDec'), .combine = rbind) %dopar% {
  tmp <- daylightImages[id, ]
  tmp[, eval(featureNames) := ReturnFeatures(filePath), by = dateTime]
}



path <- system.file("extdata/Sensor", package="visDec")
sensorFiles <- list.files(path,
                          pattern=glob2rx("DeBilt*.csv"),
                          full.names=TRUE)
sensorData <- ReadMORSensorData(sensorFiles)
setkey(sensorData, dateTime)
setkey(imageSummary, dateTime)
imageSummary <- merge(imageSummary, sensorData)
imageSummary[, MOR := TOA.MOR_10, by = dateTime]

stopImplicitCluster()

save(imageSummary, file = "~/code/output/ResultsParallelTestDeBilt2015.RData")
return(imageSummary)
}



createParallelCluster <- function()
{

user    <- 'ubuntu'
primary <- '172.31.45.30'
machineAddresses <- list(
  list(host=primary,user=user,
       ncore=1),
  list(host='172.31.38.73',user=user,
       ncore=1)
)
spec <- lapply(machineAddresses,
               function(machine) {
                 rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)
parallelCluster <- parallel::makeCluster(type='PSOCK',
                                         master=primary,
                                         spec=spec,
                                         port=11000)
print(parallelCluster)
registerDoParallel(parallelCluster)
}






