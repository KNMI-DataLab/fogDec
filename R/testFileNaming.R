

getFiles<-function(){
filenames <- list.files("/net/bhw420/nobackup/users/wauben/CAMERA/EHTW/201505",
                        pattern=glob2rx("*_2015*.jpg"),
                        full.names=TRUE, recursive = TRUE)
}


getInfo<-function(){
imageSummary <- foreach(file = iter(filenames), .combine = rbind) %do% {
  fileInformation <- FileNameParser(file, "na*me_yyyymmddhhmm.jpg")
  return(fileInformation)
}
}



GetSunTimes <- function(configDF, locationID, date){
  
  
  lat <- configDF[configDF$locationID == locationID,]$lat
  lon <- configDF[configDF$locationID == locationID,]$lon
  position <- matrix(c(lon, lat),nrow = 1)
  sunriseTime <- sunriset(position, date, direction = "sunrise", POSIXct.out = TRUE)
  sunsetTime  <- sunriset(position, date, direction = "sunset", POSIXct.out = TRUE)
  return(c(sunriseTime$time, sunsetTime$time))
  
}


FilterNightTime<-function(darkOffsetHours,configDF){
imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
  #for(file in filenames) {
  filesToExamine<-list()
  fileInformation <- FileNameParser(file, configDF$filePattern)
  dateDay<-as.POSIXct(date(fileInformation$dateTime))
  value <- GetSunTimes(configDF, 1, dateDay)
  if(!(fileInformation$dateTime < value[1] + hours(darkOffsetHours) | fileInformation$dateTime > value[2] - hours(darkOffsetHours))){
   filesToExamine<-file
  }
  filesToExamine
}
}




