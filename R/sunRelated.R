GetSunTimes <- function(configDF, locationID, date){
  lat <- configDF[configDF$locationID == locationID,]$lat
  lon <- configDF[configDF$locationID == locationID,]$lon
  position <- matrix(c(lon, lat),nrow = 1)
  sunriseTime <- sunriset(position, date, direction = "sunrise", POSIXct.out = TRUE)
  sunsetTime  <- sunriset(position, date, direction = "sunset", POSIXct.out = TRUE)
  return(c(sunriseTime$time, sunsetTime$time))
  
}


FilterNightTime<-function(darkOffsetHours, configDF, locationID){
  imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
    #for(file in filenames) {
    filesToExamine<-list()
    fileInformation <- FileNameParser(file, configDF[configDF$locationID == locationID,]$filePattern)
    dateDay<-as.POSIXct(date(fileInformation$dateTime))
    value <- GetSunTimes(configDF, locationID, dateDay)
    if(!(fileInformation$dateTime < value[1] + hours(darkOffsetHours) | fileInformation$dateTime > value[2] - hours(darkOffsetHours))){
      filesToExamine<-file
    }
    filesToExamine
  }
}
