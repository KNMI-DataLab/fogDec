## script: compute image features

## Load Libraries
library(data.table)
library(fogDec)
library(visDec)
library(ggplot2)
library(doParallel)
library(maptools)

registerDoParallel(cores=2)

basePath <- "/net/bhw420/nobackup/users/haijde/DATA/AXIS214/Meetterrein"
# basePath <- "~/Academia/"
# output   <- "/nobackup/users/roth/processedImages/Meetterrein"

directories <- dir(basePath,
                   pattern = glob2rx("*nmi*"))

foreach(directory = iter(directories), .combine = "rbind") %do% {
  message(paste0("Directory ", directory, " is being processed."))
  filenames <- list.files(paste0(basePath, directory),
                          pattern=glob2rx("Meetterrein_*.jpg"),
                          full.names = TRUE)
}

# filenames <- list.files(paste0(basePath, directory),
#                         pattern=glob2rx("Meetterrein_*.jpg"),
#                         full.names = TRUE)

# properties <- fread(system.file("extdata/properties.csv", package="visDec"))
# 
# path <- system.file("extdata/Meetterrein", package="visDec")
# filenames <- list.files(path,
#                         pattern=glob2rx("Meetterrein_201510*.jpg"),
#                         full.names=TRUE)
# 
# imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
#   FileNameParser(file, "na*me_yyyymmdd_hhmm.jpg")
# }
# 
# 
# setkey(imageSummary, filePrefix, dateTime)
# 
# imageSummary <- merge(imageSummary, properties,
#                       by.x = "filePrefix", by.y = "filePrefix")
# 
# daylightImages <- imageSummary[IsDayLightImage(dateTime, lon, lat), ]
# 
# 
# invisible(daylightImages[, id := 1:.N])
# setkey(daylightImages, id)
# 
# imageFeatures <- foreach(id = iter(daylightImages[, id]), .combine = rbind) %dopar% {
#   daylightImages[id, ImageFeatures(filePath, y > 16)]
# }
# 
# setkey(daylightImages, filePath)
# setkey(imageFeatures, filePath)
# 
# imageSummary <- merge(daylightImages, imageFeatures)

##stop the cluster
stopImplicitCluster()










