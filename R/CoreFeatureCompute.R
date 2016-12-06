# #' Computes the features of the images
# #' @param filePath String of the file location
# #' @import data.table
# #' @export
# ReturnFeatures <- function(filePath) {
#   if(grepl("Meetterrein",filePath) == TRUE){
#     im <- subim(load.image(filePath), y > 16) #cut the upper black band of DeBilt
#   } else {
#     im <- load.image(filePath)
#   }
#   #check if the image is a RGB or grayscale (converted to grayscale when dark by GIMP rectification)
#   if(dim(im)[4] == 1){
#     im<-add.colour(im, TRUE)
#   }
#   imT <- RGBtoHSV(im)
#   transmission <- GetHorizAvgTrans(im)
#   list(meanEdge = DetectMeanEdges(im, 3),
#        changePoint = TransmissionChangepoint(transmission),
#        smoothness = TransmissionSmoothness(transmission),
#        fractaldim = GetFractalDim(transmission),
#        fractalDim = GetFractalDim(im),
#        meanHue = mean(channel(imT, 1)),
#        meanSaturation = mean(channel(imT, 2)),
#        meanBrightness = mean(channel(imT, 3)) )
# }


#' Saves the features of the images
#' @param propertiesLocationsVect vector containing the configuration/properties
#' of the location of the images
#' @import data.table
#' @export
featureExtraction <- function(propertiesLocationsVect) {
  propertiesLocations<-data.table(
   fileLocation = propertiesLocationsVect[9],
   filePrefix = propertiesLocationsVect[7],
   imageFormat = propertiesLocationsVect[8],
   filePattern = propertiesLocationsVect[6],
   stationID = propertiesLocationsVect[2],
   lon = as.numeric(propertiesLocationsVect[4]),
   lat = as.numeric(propertiesLocationsVect[5]),
   locationID = propertiesLocationsVect[1]
  )
   #propertiesLocations<-data.table(propertiesLocations)
   #print(str(propertiesLocations))
  
  filenames <- list.files(propertiesLocations$fileLocation, recursive = T,
                          pattern=paste0(propertiesLocations$filePrefix, ".*.", 
                                         propertiesLocations$imageFormat, "$"),
                          full.names=TRUE)
  
  
  ##This is run in parallel
  imageSummary <- foreach(file = iter(filenames), .combine = rbind, .packages = "visDec") %dopar% {
    cat(paste0(file,"\n"), file="mylog.txt", append=TRUE)
    fogDec::FileNameParser(file, propertiesLocations$filePattern)
  }
  
  setkey(imageSummary, filePrefix, dateTime)
  
  imageSummary <- merge(imageSummary, propertiesLocations,
                        by.x = "filePrefix", by.y = "filePrefix")
  
  daylightImages <- imageSummary[IsDayLightImage(dateTime, lon, lat), ]
  
  
  #minutesBeforeSunrise <- 180
  #minutesAfterSunset <- 180
  
  #daylightImages <- FilterDayLightHours(imageSummary, properties, minutesBeforeSunrise, minutesAfterSunset)
  
  
  invisible(daylightImages[, id := 1:.N])
  setkey(daylightImages, id)
  
  
  ##This is run in parallel and this is the most compute-intense part 
  imageFeatures <- foreach(id = iter(daylightImages[, id]), .combine = rbind) %do% {
    cutPoint <- 0
    if ((daylightImages$locationID == "UK21" || daylightImages$locationID == "UK11") == TRUE) {
      cutPoint <- 39
    }
    ##WE HAVE TO FIND A SOLUTION FOR THE ARGUMENTS TO THIS NEW IMAGE FEATURE FUNCTION,
    ##IT DOEASN'T LIKE VARIABLES
    daylightImages[id, ImageFeatures(filePath, y > 39)]
  }
  
  
  setkey(daylightImages, filePath)
  setkey(imageFeatures, filePath)
  
  imageSummary <- merge(daylightImages, imageFeatures)
  
  
  saveRDS(imageSummary, file = paste0("ResultFeatures", propertiesLocations$stationID, ".rds"))
  
}
