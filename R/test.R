# library(data.table)
# library(visDec)
# library(ggplot2)
# library(doParallel)
# registerDoParallel(cores=3)
# library(imager)
# library(changepoint) # functionality should be included in imager
# library(maptools)
# library(lubridate)
# 
# GetSunTimes <- function(configDF, locationID, date){
#   
#   
#   lat <- configDF[configDF$locationID == locationID,]$lat
#   lon <- configDF[configDF$locationID == locationID,]$lon
#   position <- matrix(c(lon, lat),nrow = 1)
#   sunriseTime <- sunriset(position, date, direction = "sunrise", POSIXct.out = TRUE)
#   sunsetTime  <- sunriset(position, date, direction = "sunset", POSIXct.out = TRUE)
#   return(c(sunriseTime$time, sunsetTime$time))
#   
# }
# 
# 
# FilterNightTime<-function(darkOffsetHours,configDF, locationID){
#   imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
#     #for(file in filenames) {
#     filesToExamine<-list()
#     fileInformation <- FileNameParser(file, configDF[configDF$locationID == locationID,]$filePattern)
#     dateDay<-as.POSIXct(date(fileInformation$dateTime))
#     value <- GetSunTimes(configDF, locationID, dateDay)
#     if(!(fileInformation$dateTime < value[1] + hours(darkOffsetHours) | fileInformation$dateTime > value[2] - hours(darkOffsetHours))){
#       filesToExamine<-file
#     }
#     filesToExamine
#   }
# }
# 
# 
# filenames <- list.files("/net/bhw420/nobackup/users/haijde/DATA/AXIS214/Meetterrein/201512",
#                         pattern=glob2rx("*_2015*.jpg"),
#                         full.names=TRUE, recursive = TRUE)
# detect.edges <- function(im,sigma=1) {
#   # adapted from http://dahtah.github.io/imager/foreground_background.html
#   isoblur(im,sigma) %>% imgradient("xy") %>% llply(function(v) v^2) %>%
#     add %>% imsplit("c") %>% add 
# }
# 
# 
# configDF <- read.csv("properties.csv",stringsAsFactors = FALSE)
# 
# location <- 1
# 
# filenames <- FilterNightTime(2,configDF,locationID = location)
# 
# filenames<-unlist(filenames)
# 
# fileStringPattern <- configDF[configDF$locationID == location,]$filePattern
# 
# 
# 
# imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
#   #for(file in filenames){
#   fileInformation <- FileNameParser(file, fileStringPattern)
#   im <- subim(load.image(file), y > 16) #[, -(1 :17), ,]
#   #imGradient <- get_gradient(im, "xy", scheme = 2L)
#   print(fileInformation$name)
#   data.table(name = fileInformation$name,
#              dateTime = fileInformation$dateTime,
#              meanEdge = detect.edges(im, 3) %>% sqrt %>% mean,
#              changePoint = cpts(cpt.mean(GetHorizAvgTrans(im), penalty = "None"))
#   )
#   #hazeFactor <- GetHorizAvgTrans(im)
#   #print(hazeFactor)
# }
# 
# 
