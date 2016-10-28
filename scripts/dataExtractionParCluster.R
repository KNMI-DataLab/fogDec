library(data.table)
library(visDec)
library(ggplot2)
library(doParallel)
registerDoParallel(cores=3)
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

path1 <- "~/efs/data/twente"
path2 <- "/mnt/disks/dataDisk/data/twente/"
filenames <- list.files(path1, recursive = T,
                        #pattern=glob2rx("Meetterrein_2015*.jpg"),
                        pattern=glob2rx("EHTW_2015*.jpg"),
                        full.names=TRUE)


imageSummary <- foreach(file = iter(filenames), .combine = rbind, .packages = "visDec") %dopar% {
  #FileNameParser(file, "na*me_yyyymmdd_hhmm.jpg")#DeBilt pattern
  FileNameParser(file, "na*me_yyyymmddhhmm.jpg")#Twente pattern
}

daylightImages <- FilterDayLightHours(imageSummary, properties, 180, 180)

ReturnFeatures <- function(filePath) {
  #im <- subim(load.image(filePath), y > 16) 
  im <- load.image(filePath)
  #check if the image is a RGB or grayscale (converted to grayscale when dark by GIMP rectification)
  if(dim(im)[4] == 1){
    im<-add.colour(im, TRUE)
  }
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

imageSummary <- foreach(id = iter(daylightImages[, id]), .packages = c('data.table','visDec'), .combine = rbind) %dopar% {
  tmp <- daylightImages[id, ]
  tmp[, eval(featureNames) := ReturnFeatures(filePath), by = dateTime]
}



path <- system.file("extdata/Sensor", package="visDec")
sensorFiles <- list.files(path,
                          pattern=glob2rx("Twente*.csv"),
                          full.names=TRUE)
sensorData <- ReadMORSensorData(sensorFiles)
setkey(sensorData, dateTime)
setkey(imageSummary, dateTime)
imageSummary <- SynchronizeSensorPicture(sensorData, imageSummary)
#imageSummary <- merge(imageSummary, sensorData)
imageSummary[, MOR := TOA.MOR_10, by = dateTime]

stopImplicitCluster()

save(imageSummary, file = "ResultsTwente2015_3hSun.RData")
return(imageSummary)
}



createParallelCluster <- function()
{
  i<-0
machines<-list()
user    <- 'ubuntu'
primary <- '172.31.45.30'

IPs<-paste0("172.31.46.", seq(from = 157, to = 174))
IPs<-c(IPs, "172.31.38.73")
for (ip in IPs){
  i<-i+1
  machines[[i]]<-list(host=ip, user = user, ncore=1)
}

machineAddresses <- list(
  list(host=primary,user=user,
       ncore=1)
)
machineAddresses<-c(machineAddresses,machines)
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

#clusterEvalQ(parallelCluster, library(imager), FileNameParser())
clusterEvalQ(parallelCluster, c(library(imager),library(data.table)))
clusterExport(parallelCluster,"FileNameParser")
registerDoParallel(parallelCluster)
}






