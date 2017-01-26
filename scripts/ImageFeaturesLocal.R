## script: compute image features

## Load Libraries
library(data.table)
library(visDec)
library(fogDec)
library(ggplot2)
library(doParallel)
library(maptools)

registerDoParallel(cores=5)

system.time({
# properties <- fread(system.file("extdata/properties.csv", package="visDec"))
# basePath <- "/net/bhw420/nobackup/users/haijde/DATA/AXIS214/Meetterrein/"
# output   <- "/nobackup/users/roth/processedImages/Meetterrein/"
# properties <- fread("properties.csv")
# basePath <- "/net/bhw420/nobackup/users/wauben/CAMERA/EHTW/"
# output   <- "/nobackup/users/roth/processedImages/AWSTwente/"
properties <- fread("properties.csv")
basePath <- "/net/bhw510/nobackup/users/pagani/cabauw"
output   <- "/nobackup/users/roth/processedImages/AWSTwenteRect/"

directories <- dir(basePath,
                   pattern = glob2rx("20*"))

foreach(directory = iter(directories), .combine = "rbind") %do% {
  message(paste0("Directory ", directory, " is being processed."))
  filenames <- list.files(paste0(basePath, directory),
                          # pattern=glob2rx("Meetterrein_*.jpg"),
                          pattern=glob2rx("CW1*.jpg"),
                          full.names = TRUE)
  
  imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
    FileNameParser(file, "na*me_yyyymmdd_hhmm.jpg")
  }
  
  setkey(imageSummary, filePrefix, dateTime)
  
  imageSummary <- merge(imageSummary, properties,
                        by.x = "filePrefix", by.y = "filePrefix")
  
  daylightImages <- imageSummary[IsDayLightImage(dateTime, lon, lat), ]
  
  invisible(daylightImages[, id := 1:.N])
  setkey(daylightImages, id)
  
  imageFeatures <- foreach(id = iter(daylightImages[, id]), .combine = rbind) %dopar% {
    daylightImages[id, ImageFeatures(filePath)] # FIXME: x > 120 results in NA 
    # daylightImages[id, ImageFeatures(filePath, y > 16)]
  }
  
  setkey(daylightImages, filePath)
  setkey(imageFeatures, filePath)
  
  imageSummary <- merge(daylightImages, imageFeatures)
  saveRDS(imageSummary, file = paste0(output, directory, "_summary.rds"))
}
})

##stop the cluster
stopImplicitCluster()

# output <- "/nobackup/users/roth/processedImages/Meetterrein"
# files  <- list.files(output, full.names = TRUE)
# result <- foreach(f = iter(files), .combine = "rbind") %do% {
#   readRDS(f)
# }
# 
# sensorData <- ReadMORSensorData("inst/extdata/Sensor/DeBiltMOR2015-2016.csv")
# sensorData <- sensorData[DS_CODE == "260_A_a", .(dateTime, TOA.MOR_10)]
# setnames(sensorData, c("dateTime", "MOR"))
# 
# setkey(result, dateTime)
# setkey(sensorData, dateTime)
# values <- merge(result, sensorData)
# values <- na.omit(values)







