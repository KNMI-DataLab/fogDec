#' Evaluate the prediction computing prediction and recall
#' @param t predicted data table
#' @export
evaluatePrediction <- function(t){
  cleanedTest <- t[complete.cases(t)] #there are sitations where the MOR is not verified and is given an NA for the measure, thus remove, actually we should remove from the train set and test set before giving to the classifier
  
  predBin <- ifelse(cleanedTest$pred > 0.01, 1, 0) #round(cleanedTest$pred,0) #decision to a positive is taken when prediction probability higher than 0.01 (can be changed)
  
  labels <- cleanedTest$foggy
  tp <- sum(predBin == 1 & labels == T)
  tp
  tn <- sum(predBin == 0 & labels == F)
  tn
  falsePos <- sum(predBin == 1 & labels ==F)
  falsePos
  falseNeg <- sum(predBin == 0 & labels ==T)
  falseNeg
  precision <- tp / (tp + falsePos)
  recall <- tp / (tp + falseNeg)
  print("precision:")
  print(precision)
  print("recall:")
  print(recall)
  
  print("F1 score:")
  print(2*precision*recall/(precision+recall))
  
  # predComp<-prediction(cleanedTest$pred, labels)
  # PR.perf <- performance(predComp, "acc")
  # plot(PR.perf)
}


#' Perform the decision tree analysis based on the image and meteo (wind) feature
#' @param imageFeaturesFile RDS file with image features
#' @param windFile csv file containing wind speed
#' @return test test set with probability of positive label
#' @export
analyzeData <- function(imageFeaturesFile, windFile){

  library(data.table)
  library(imager)
  library(visDec)
  library(ggplot2)
  library(rpart)
  library(rattle)
  library(ROCR)
  library(caret)
  library(fogDec)
  Results<-readRDS(imageFeaturesFile)
  Results<-data.table(Results)
  
  Results[, dayIsEven := mday(dateOnly) %% 2]
  offsetBeforeSunrise <- 0
  offsetAfterSunset <- 0
  daylightImages <- Results[, isDay := dateTime > sunriseDateTime - offsetBeforeSunrise * 60 & dateTime < sunsetDateTime + offsetAfterSunset * 60]
  daylightImages <- daylightImages[isDay == TRUE]
  daylightImages[,isDay := NULL]
  
  windData <- ReadWindData(windFile)
  #setkey(windData, dateTime)
  #setkey(daylightImages, dateTime)
  
  daylightImages<-SynchronizeSensorReadingsNoMORPicture(windData, daylightImages)
  #daylightImages<-daylightImages[windData, nomatch = 0]
  
  set.seed(128)
  trainIndex <- createDataPartition(daylightImages$stationID, p = .7, list = FALSE, times = 1)
  trainingSet <- daylightImages[trainIndex,]
  testSet <- daylightImages[-trainIndex,]
  
  
  
  train <- trainingSet[, .(dateTime, MOR, meanEdge, changePoint, meanBrightness, windSpeed)]
  train[, foggy := MOR < 250]
  fogTree <- rpart(foggy ~ meanEdge + changePoint + meanBrightness, train , control = rpart.control(cp = 0.019))
  
  fancyRpartPlot(fogTree, sub="")
  
  
  test <- testSet[, .(dateTime, MOR, meanEdge, changePoint, meanBrightness, windSpeed)]
  test[, foggy := MOR < 250]
  
  
  pred <- predict(fogTree, test, method="class")
  test[, pred := pred]
  
  
  
  fogWindTree <- rpart(foggy ~ meanEdge + changePoint + meanBrightness+ windSpeed, train, control = rpart.control(cp = 0.019))
  
  
  fancyRpartPlot(fogWindTree, sub="")
  
  
  
  predWind <- predict(fogWindTree, test, method="class")
  test[, predWind := predWind]
  
  evaluatePrediction(test)
  
  return(test)
}
  








analysisTwente<-function(){
  library(data.table)
  library(imager)
  library(visDec)
  library(ggplot2)
  library(rpart)
  library(rattle)
  library(ROCR)
  library(caret)
  
  dataTwente <- readRDS("results/ResultsTwente2015-2016_3hSun.rds")
  offsetBeforeSunrise <- 0 #time in minutes
  offsetAfterSunset <- 0
  #trainDeBilt <- imageSummary[dateTime > sunriseDateTime - offsetBeforeSunrise * 60 & dateTime < sunsetDateTime + offsetAfterSunset * 60, ]
  dataTwente <- dataTwente[dateTime > sunriseDateTime & dateTime < sunsetDateTime, ]
  dataTwente[, foggy := MOR < 250]
  setkey(dataTwente, dateTime)

  source("scripts/ReadMeteoData.R")
  windTwente <- ReadWindData("inst/extdata/Sensor/TwenteWind1-1-2015-31-08-2016.csv")
  #windTwente <- ReadWindData()
  setkey(windTwente, dateTime)

  humidityTwente <- ReadHumidityData("inst/extdata/Sensor/TwenteHumidity1-1-2015-31-08-2016.csv")
  setkey(humidityTwente, dateTime)

  tempDewPointTwente <- ReadTempDewPointData("inst/extdata/Sensor/TwenteTemp_DewPoint1-1-2015-31-08-2016.csv")
  setkey(tempDewPointTwente, dateTime)
  
  precipitationTwente <- ReadPrecipitationData("inst/extdata/Sensor/TwenteRainAll-1-2015-31-08-2016.csv")
  precipitationTwente[, rain := FALSE]
  precipitationTwente[ precipitationDurationPWS > 300 | precipitationDurationElec > 300, rain:=TRUE]
  setkey(precipitationTwente, dateTime)
  
  
  
  dataTwente <- SynchronizeSensorReadingsNoMORPicture(windTwente, dataTwente)
  dataTwente <- SynchronizeSensorReadingsNoMORPicture(humidityTwente, dataTwente)
  dataTwente <- SynchronizeSensorReadingsNoMORPicture(tempDewPointTwente, dataTwente)
  dataTwente <- SynchronizeSensorReadingsNoMORPicture(precipitationTwente, dataTwente)
  
  
  
  
  dataTwente <- na.omit(dataTwente)
  
  trainTwente <- dataTwente[year==2015, .(dateTime, meanEdge, changePoint, smoothness, meanHue,
                                          meanSaturation, meanBrightness, MOR, foggy, day, month, hour, windSpeed, relHumidity, airTemperature, dewPoint, precipitationIntElec, precipitationIntPWS, precipitationDurationElec, precipitationDurationPWS, rain) ]
  
  testTwente <- dataTwente[year==2016, .(dateTime, meanEdge, changePoint, smoothness, meanHue,
                                         meanSaturation, meanBrightness, MOR, foggy, day, month, hour, windSpeed, relHumidity, airTemperature, dewPoint, precipitationIntElec, precipitationIntPWS, precipitationDurationElec, precipitationDurationPWS, rain) ]
  
  
  
  
  fogTree <- rpart(foggy ~ meanEdge + changePoint + meanBrightness, trainTwente , control = rpart.control(cp = 0.02))

  fancyRpartPlot(fogTree, sub="")

  
  hindVals <- predict(fogTree, trainTwente, method = "class")
  trainTwente[, pred := hindVals]
  
  predVals <- predict(fogTree, testTwente, method="class")
  testTwente[, pred := predVals]
  
  #usually it is not a good practice to show the results of the classification on the training set itself.
  confmatImageFeaturesTrain <- confusionMatrix(ifelse(trainTwente$pred > 0.3, TRUE, FALSE), trainTwente$foggy, positive = "TRUE")
  confmatImageFeaturesTest <- confusionMatrix(ifelse(testTwente$pred  > 0.3, TRUE, FALSE), testTwente$foggy, positive = "TRUE")
  
  print(confmatImageFeaturesTest)

  
  
  mlm <- lm(log(MOR) ~ meanEdge + changePoint + meanBrightness, trainTwente) 
  round(mlm$coefficients, 3)
  predVals <- predict(mlm, trainTwente)
  trainTwente[, predMOR := predVals]

  
  ggplot(trainTwente, aes(x = log(MOR), y = predMOR)) + geom_point() +
    geom_vline(xintercept = c(log(250), log(1000), log(3000), log(5000)), lty = 3) + 
    ylab("modelled log(MOR)")
  
  
  fogTreeMeteoTwente <- rpart(foggy ~ meanEdge + changePoint + meanBrightness +
                                 relHumidity + windSpeed + airTemperature + dewPoint + rain, 
                                 trainTwente , control = rpart.control(cp = 0.01))
  
  

  
  fancyRpartPlot(fogTreeMeteoTwente, sub="")
  
  print(fogTreeMeteoTwente)

  
  
  hindVals <- predict(fogTreeMeteoTwente, trainTwente, method = "class")
  trainTwente[, pred := hindVals]
  predVals <- predict(fogTreeMeteoTwente, testTwente, method="class")
  testTwente[, pred := predVals]
  
  ###RAIN CORRECTION###
  testTwente[rain == TRUE, pred := 0.1]
  
  confusionMatrix(ifelse(trainTwente$pred > 0.3, TRUE, FALSE), trainTwente$foggy, positive = "TRUE")
  confmatAllMeteoFeaturesTest<-confusionMatrix(ifelse(testTwente$pred > 0.3, TRUE, FALSE), testTwente$foggy, positive = "TRUE")
  print(confmatAllMeteoFeaturesTest)
  
  return(trainTwente)
  
}
