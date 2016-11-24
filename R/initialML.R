
classifyUnsup <- function(dataSet){
  registerDoParallel(cores=6)
  imageSummary <- foreach(file = dataSet$filename, .combine = rbind) %dopar% {
    im <- subim(load.image(file), y > 16) 
    data.table(name = dataSet$name,
               dateTime = dataSet$dateTime,
               meanEdge = detect.edges(im, 3) %>% sqrt %>% mean,
               changePoint = cpts(cpt.mean(GetHorizAvgTrans(im), penalty = "None"))
    )
  }
  
  
}


detect.edges <- function(im,sigma=1) {
  # adapted from http://dahtah.github.io/imager/foreground_background.html
  isoblur(im,sigma) %>% imgradient("xy") %>% llply(function(v) v^2) %>%
    add %>% imsplit("c") %>% add 
}



main <- function(directoryPath){
  
  
  configDF <- read.csv("properties.csv")
  
  location <- 1
  
  testTrainDF <- assignTrainAndTestSet("/net/bhw420/nobackup/users/haijde/DATA/AXIS214/Meetterrein/201511")
  
  complete <- AddDayNightInfo(testTrainDF)
  
  # testTrainDF$dateOnly <- date(testTrainDF$dateTime)
  # setkeyv(testTrainDF, c("filename","dateOnly"))
  # uniqueDateStation<-subset(unique(testTrainDF), select=c("filename", "dateOnly"))
  # mergedData<-merge(configDF,uniqueDateStation, by.x = "imagePrefix", by.y = "filename")
  # dataWithSunTimes <- GetSunTimes(mergedData)
  # 
  # 
  # combined <- merge(testTrainDF, dataWithSunTimes, by.x = "dateOnly", by.y = "dateOnly")
  # 
  # combined$isDay <- combined[,dateTime] > combined[,sunriseDateTime] & combined[,dateTime] < combined[,sunsetDateTime]
  
  complete <- complete[isDay==TRUE]
  
  trainSet <- complete[trainTest=="train",]
  
  
  
  imageSummary <- foreach(file = iter(trainSet[,path]), .combine = rbind) %dopar% {
    im <- subim(load.image(file), y > 16) #[, -(1 :17), ,]
    #imGradient <- get_gradient(im, "xy", scheme = 2L)
    data.table(name = trainSet$path,
               dateTime = trainSet$dateTime,
               meanEdge = detect.edges(im, 3) %>% sqrt %>% mean,
               changePoint = cpts(cpt.mean(GetHorizAvgTrans(im), penalty = "None"))
    )
  
  }
  
  
  
}










