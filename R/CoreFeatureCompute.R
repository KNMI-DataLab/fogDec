#' Computes the features of the images
#' @param filePath String of the file location
#' @import data.table
#' @export
ReturnFeatures <- function(filePath) {
  if(grepl("Meetterrein",filePath) == TRUE){
    im <- subim(load.image(filePath), y > 16) #cut the upper black band of DeBilt
  } else {
    im <- load.image(filePath)
  }
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


#' Saves the features of the images
#' @param propertiesLocationsVect vector containing the configuration/properties
#' of the location of the images
#' @import data.table
#' @export
featureExtraction <- function(propertiesLocationsVect) {
   propertiesLocations$fileLocation <- propertiesLocationsVect[9]
   print(propertiesLocations$fileLocation)
   propertiesLocations$imagePrefix <- propertiesLocationsVect[7]
   propertiesLocations$imageFormat <- propertiesLocationsVect[8]
   propertiesLocations$filePattern <- propertiesLocationsVect[6]
   propertiesLocations$stationID <- propertiesLocationsVect[2]
   propertiesLocations<-data.table(propertiesLocations)
   print(str(propertiesLocations))
  
  filenames <- list.files(propertiesLocations$fileLocation, recursive = T,
                          pattern=paste0(propertiesLocations$imagePrefix, ".*.", 
                                         propertiesLocations$imageFormat, "$"),
                          full.names=TRUE)
  
  
  ##This is run in parallel
  imageSummary <- foreach(file = iter(filenames), .combine = rbind, .packages = "visDec") %dopar% {
    fogDec::FileNameParser(file, propertiesLocations$filePattern)
  }
  
  
  minutesBeforeSunrise <- 180
  minutesAfterSunset <- 180
  
  daylightImages <- FilterDayLightHours(imageSummary, properties, minutesBeforeSunrise, minutesAfterSunset)
  
  
  featureNames <- c("meanEdge", "changePoint", "smoothness",
                    "meanHue", "meanSaturation", "meanBrightness")
  
  
  
  daylightImages[, id := 1:.N]
  setkey(daylightImages, id)
  
  ##This is run in parallel and this is the most compute-intense part 
  imageSummary <- foreach(id = iter(daylightImages[, id]), .packages = c('data.table','visDec'), .combine = rbind) %dopar% {
    tmp <- daylightImages[id, ]
    tmp[, eval(featureNames) := fogDec:::ReturnFeatures(filePath), by = dateTime]
  }
  
  saveRDS(imageSummary, file = paste0("ResultFeatures", propertiesLocations$stationID, ".rds"))
  
}
