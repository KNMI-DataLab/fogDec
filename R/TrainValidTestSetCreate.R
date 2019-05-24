#' Create training, validation and test datasets using time consistent data
#' @param dataDir String of directory containing the cameras
#' @param dateMin String of initial date do use for fetching data
#' @param dateMax String of latest date do use for fetching data
#' @param dbConfigDir String of path with directory containing the DB param access config file
#' @param maxDist Numerical maximum distance between camera and KNMI station
#' @import data.table
#' @export
createTrainValidTestSetsBinaryTimeSeries<-function(dataDir,dateMin="\'2015-01-01 00:00:00\'", dateMax= "\'2018-02-28 00:00:00\'",dbConfigDir,maxDist=2500){
  
  Sys.setenv(TZ = "UTC")

  total<-coupleImagesAndMeteoToDate(dateMin,dateMax,dbConfigDir,maxDist)
  

  set.seed(11)
  
  nFog<-dim(total[foggy==TRUE])[[1]]
  nNonFog<-dim(total[foggy==FALSE])[[1]]
  
  fogNonFogRatio<-nFog/nNonFog
  
  
  #FOGGY CASES
  #####TRAINING
  
  foggyData<-total[foggy==TRUE]
  inTraining<-sample(nrow(foggyData),0.6*nrow(foggyData))
  trainingSmall<-foggyData[inTraining]
  #inTrainingMore<-sample(nrow(trainingSmall),200000, replace = T)
  #training<-trainingSmall[inTrainingMore]
  training<-trainingSmall
  
  
  #####CROSS VALIDATION
  
  remaining<-foggyData[-inTraining]
  inCrossVal<-sample(nrow(remaining),0.2*nrow(foggyData))
  
  crossValidating<-remaining[inCrossVal]
  
  #####TEST SET
  testing<-remaining[-inCrossVal]
  
  
  #check that are disjoint datasets
  sum(duplicated(rbind(crossValidating,testing)))
  sum(duplicated(rbind(unique(training),testing)))
  sum(duplicated(rbind(unique(training),crossValidating)))
  
  #####NON-FOGGY CASES
  #####TRAINING
  nonFoggyData<-total[foggy==FALSE]
  #inTrainNoFog<-sample(nrow(nonFoggyData),nrow(training))
  inTrainNoFog<-sample(nrow(nonFoggyData),0.6*nrow(nonFoggyData))
  
  nonFoggyTraining<-nonFoggyData[inTrainNoFog]
  
  #####CROSS VALIDATION
  remaining<-nonFoggyData[-inTrainNoFog]
  inCrossVal<-sample(nrow(remaining),0.2*nrow(nonFoggyData))
  
  nonFoggyCrossValidating<-remaining[inCrossVal]
  
  ######TEST SET
  #foggyInTest<-dim(testing[foggy==TRUE])[[1]]
  #nonFoggyForRealisticRatio<-foggyInTest*1/fogNonFogRatio
  #inTestNoFog<-sample(nrow(remaining[-inCrossVal]),nonFoggyForRealisticRatio)
  inTestNoFog<-remaining[-inCrossVal]
  
  #nonFoggyTesting<-remaining[-inCrossVal][inTestNoFog]
  nonFoggyTesting<-inTestNoFog
  
  
  sum(duplicated(rbind(nonFoggyCrossValidating,nonFoggyTesting)))
  sum(duplicated(rbind(nonFoggyTraining,nonFoggyTesting)))
  sum(duplicated(rbind(nonFoggyTraining,nonFoggyCrossValidating)))
  
  
  
  ####Binding the fog and non-fog sets with the corresponding
  training<-rbind(training,nonFoggyTraining)
  crossValidating<-rbind(crossValidating,nonFoggyCrossValidating)
  testing<-rbind(testing,nonFoggyTesting)
  
  dataSets<-list(training,crossValidating,testing)
}














#' Create training, validation and test datasets using random data
#' @param dataDir String of directory containing the cameras
#' @param dateMin String of initial date do use for fetching data
#' @param dateMax String of latest date do use for fetching data
#' @param dbConfigDir String of path with directory containing the DB param access config file
#' @param maxDist Numerical maximum distance between camera and KNMI station
#' @param visibilityThreshold Numerical binary threshold for fog condition
#' @param dayPhaseFlag Numerical definition of the dayphase (0:"night" 1:"day" 10:"civil dawn" 11:"civil dusk" 20:"nautical dawn" 21:"nautical dusk" 30:"astronomical dawn" 31:"astronomical dusk")
#' @import data.table
#' @export
createTrainValidTestSetsBinaryRandom<-function(dataDir,dateMin="\'2015-01-01 00:00:00\'",dateMax= "\'2018-02-28 00:00:00\'",dbConfigDir,maxDist=2500, visibilityThreshold=250, dayPhaseFlag=1){

Sys.setenv(TZ = "UTC")
#resolutionImg<-28

total<-coupleImagesAndMeteoToDate(dateMin,dateMax,dbConfigDir,maxDist,visibilityThreshold,dayPhaseFlag)

set.seed(11)

nFog<-dim(total[foggy==TRUE])[[1]]
nNonFog<-dim(total[foggy==FALSE])[[1]]

fogNonFogRatio<-nFog/nNonFog


#FOGGY CASES
#####TRAINING

foggyData<-total[foggy==TRUE]
inTraining<-sample(nrow(foggyData),0.6*nrow(foggyData))
trainingSmall<-foggyData[inTraining]
#inTrainingMore<-sample(nrow(trainingSmall),200000, replace = T)
#training<-trainingSmall[inTrainingMore]
training<-trainingSmall


#####CROSS VALIDATION

remaining<-foggyData[-inTraining]
inCrossVal<-sample(nrow(remaining),0.2*nrow(foggyData))

crossValidating<-remaining[inCrossVal]

#####TEST SET
testing<-remaining[-inCrossVal]


#check that are disjoint datasets
sum(duplicated(rbind(crossValidating,testing)))
sum(duplicated(rbind(unique(training),testing)))
sum(duplicated(rbind(unique(training),crossValidating)))

#####NON-FOGGY CASES
#####TRAINING
nonFoggyData<-total[foggy==FALSE]
#inTrainNoFog<-sample(nrow(nonFoggyData),nrow(training))
inTrainNoFog<-sample(nrow(nonFoggyData),0.6*nrow(nonFoggyData))

nonFoggyTraining<-nonFoggyData[inTrainNoFog]

#####CROSS VALIDATION
remaining<-nonFoggyData[-inTrainNoFog]
inCrossVal<-sample(nrow(remaining),0.2*nrow(nonFoggyData))

nonFoggyCrossValidating<-remaining[inCrossVal]

######TEST SET
#foggyInTest<-dim(testing[foggy==TRUE])[[1]]
#nonFoggyForRealisticRatio<-foggyInTest*1/fogNonFogRatio
#inTestNoFog<-sample(nrow(remaining[-inCrossVal]),nonFoggyForRealisticRatio)
inTestNoFog<-remaining[-inCrossVal]

#nonFoggyTesting<-remaining[-inCrossVal][inTestNoFog]
nonFoggyTesting<-inTestNoFog


sum(duplicated(rbind(nonFoggyCrossValidating,nonFoggyTesting)))
sum(duplicated(rbind(nonFoggyTraining,nonFoggyTesting)))
sum(duplicated(rbind(nonFoggyTraining,nonFoggyCrossValidating)))



####Binding the fog and non-fog sets with the corresponding
training<-rbind(training,nonFoggyTraining)
training<-training[sample(nrow(training)),]
crossValidating<-rbind(crossValidating,nonFoggyCrossValidating)
crossValidating<-crossValidating[sample(nrow(crossValidating)),]
testing<-rbind(testing,nonFoggyTesting)
testing<-testing[sample(nrow(testing)),]

dataSets<-list(training,crossValidating,testing)
}




#' Create training, validation and test datasets using random data
#' @param dataDir String of directory containing the cameras
#' @param dateMin String of initial date do use for fetching data
#' @param dateMax String of latest date do use for fetching data
#' @param dbConfigDir String of path with directory containing the DB param access config file
#' @param maxDist Numerical maximum distance between camera and KNMI station
#' @param visibilityThreshold Numerical binary threshold for fog condition
#' @param dayPhaseFlag Numerical definition of the dayphase (0:"night" 1:"day" 10:"civil dawn" 11:"civil dusk" 20:"nautical dawn" 21:"nautical dusk" 30:"astronomical dawn" 31:"astronomical dusk")
#' @param splitPositive Numerical split for the fraction of positive to negative cases
#' @import data.table
#' @export
createTrainValidTestSetsSplitBinaryRandom<-function(dataDir,dateMin="\'2015-01-01 00:00:00\'",dateMax= "\'2018-02-28 00:00:00\'",dbConfigDir,maxDist=2500, visibilityThreshold=250, dayPhaseFlag=1, splitPositive=0.3){
  
  Sys.setenv(TZ = "UTC")
  #resolutionImg<-28
  
  total<-coupleImagesAndMeteoToDate(dateMin,dateMax,dbConfigDir,maxDist,visibilityThreshold,dayPhaseFlag)
  
  set.seed(11)
  
  #remove the NA on fog variable
  total<-total[is.na(foggy)==FALSE]
  
  nFog<-dim(total[foggy==TRUE])[[1]]
  nNonFog<-dim(total[foggy==FALSE])[[1]]
  
  fogNonFogRatio<-nFog/nNonFog
  
  
  #FOGGY CASES
  #####TRAINING
  
  foggyData<-total[foggy==TRUE]
  inTraining<-sample(nrow(foggyData),0.6*nrow(foggyData))
  trainingSmall<-foggyData[inTraining]
  #inTrainingMore<-sample(nrow(trainingSmall),200000, replace = T)
  #training<-trainingSmall[inTrainingMore]
  training<-trainingSmall
  
  
  #####CROSS VALIDATION
  
  remaining<-foggyData[-inTraining]
  inCrossVal<-sample(nrow(remaining),0.2*nrow(foggyData))
  
  crossValidating<-remaining[inCrossVal]
  
  #####TEST SET
  testing<-remaining[-inCrossVal]
  
  
  #check that are disjoint datasets
  sum(duplicated(rbind(crossValidating,testing)))
  sum(duplicated(rbind(unique(training),testing)))
  sum(duplicated(rbind(unique(training),crossValidating)))
  
  
  totalCasesFogTrain = nrow(training)
  totalCasesTrain = round(totalCasesFogTrain/splitPositive)
  totalCasesNonFogTrain = totalCasesTrain-totalCasesFogTrain
  
  print(totalCasesFogTrain)
  print(totalCasesTrain)
  print(totalCasesNonFogTrain)
  
  #totalCasesFogCrossValid = nrow(nrow(testing))
  #totalCasesCrossValid = round(totalCasesFogCrossValid/splitPositive)
  #totalCasesNonFogCrossValid = totalCasesCrossValid-totalCasesFogCrossValid
  
  
  #totalCasesFogTrain = nrow(nrow(training))
 # totalCasesTrain = round(totalCasesFogTrain/splitPositive)
 # totalCasesNonFogTrain = totalCasesTrain-totalCasesFogTrain
  
  
  
  #####NON-FOGGY CASES
  #####TRAINING
  nonFoggyData<-total[foggy==FALSE]
  #inTrainNoFog<-sample(nrow(nonFoggyData),nrow(training))
  inTrainNoFog<-sample(nrow(nonFoggyData),size=totalCasesNonFogTrain)
  
  nonFoggyTraining<-nonFoggyData[inTrainNoFog]
  
  #####CROSS VALIDATION
  remaining<-nonFoggyData[-inTrainNoFog]
  inCrossVal<-sample(nrow(remaining),0.2*nrow(nonFoggyData))
  
  nonFoggyCrossValidating<-remaining[inCrossVal]
  
  ######TEST SET
  #foggyInTest<-dim(testing[foggy==TRUE])[[1]]
  #nonFoggyForRealisticRatio<-foggyInTest*1/fogNonFogRatio
  #inTestNoFog<-sample(nrow(remaining[-inCrossVal]),nonFoggyForRealisticRatio)
  inTestNoFog<-remaining[-inCrossVal]
  
  #nonFoggyTesting<-remaining[-inCrossVal][inTestNoFog]
  nonFoggyTesting<-inTestNoFog
  
  
  sum(duplicated(rbind(nonFoggyCrossValidating,nonFoggyTesting)))
  sum(duplicated(rbind(nonFoggyTraining,nonFoggyTesting)))
  sum(duplicated(rbind(nonFoggyTraining,nonFoggyCrossValidating)))
  
  
  
  ####Binding the fog and non-fog sets with the corresponding
  training<-rbind(training,nonFoggyTraining)
  training<-training[sample(nrow(training)),]
  crossValidating<-rbind(crossValidating,nonFoggyCrossValidating)
  crossValidating<-crossValidating[sample(nrow(crossValidating)),]
  testing<-rbind(testing,nonFoggyTesting)
  testing<-testing[sample(nrow(testing)),]
  
  dataSets<-list(training,crossValidating,testing)
}





#' Create training, validation and test datasets using random data
#' @param dataDir String of directory containing the cameras
#' @param dateMin String of initial date do use for fetching data
#' @param dateMax String of latest date do use for fetching data
#' @param dbConfigDir String of path with directory containing the DB param access config file
#' @param maxDist Numerical maximum distance between camera and KNMI station
#' @param visibilityThreshold Numerical binary threshold for fog condition
#' @param dayPhaseFlag Numerical definition of the dayphase (0:"night" 1:"day" 10:"civil dawn" 11:"civil dusk" 20:"nautical dawn" 21:"nautical dusk" 30:"astronomical dawn" 31:"astronomical dusk")
#' @param splitPositive Numerical split for the fraction of positive to negative cases
#' @import data.table
#' @export
createTrainValidTestSetsSplitMulticlassRandom<-function(dataDir,dateMin="\'2015-01-01 00:00:00\'",dateMax= "\'2018-02-28 00:00:00\'",dbConfigDir,maxDist=2500, visibilityThreshold=c(200,1000,5000), dayPhaseFlag=1){
  
  Sys.setenv(TZ = "UTC")
  #resolutionImg<-28
  
  total<-coupleImagesAndMeteoToDate(dateMin,dateMax,dbConfigDir,maxDist,visibilityThreshold,dayPhaseFlag)
  
  set.seed(11)
  
  #remove the NA on fog variable
  total<-total[is.na(visClass)==FALSE]
  
  nClass1<-dim(total[visClass==1])[[1]]
  nClass2<-dim(total[visClass==2])[[1]]
  nClass3<-dim(total[visClass==3])[[1]]
  nClass4<-dim(total[visClass==4])[[1]]
  
  classes<-c(nClass1,nClass2,nClass3,nClass4)
  
  minSample=classes[[which.min(c(nClass1,nClass2,nClass3,nClass4))]]
  
  
  #Assume 4 classes and evenly split
  totalCases = minSample*4
  
  
  
  
  #class1 CASES
  #####TRAINING
  
  class1Data<-total[visClass==1]
  inTraining<-sample(minSample,0.6*minSample)
  trainingClass1<-class1Data[inTraining]
 
  #training<-trainingSmall
  
  
  #####CROSS VALIDATION
  
  remaining<-class1Data[-inTraining]
  inCrossVal<-sample(nrow(remaining),0.2*minSample)
  
  crossValidatingClass1<-remaining[inCrossVal]
  
  #####TEST SET
  inTestNoFog<-remaining[-inCrossVal]
  testingClass1<-inTestNoFog
  inTestingClass1<-sample(nrow(inTestNoFog),0.2*minSample)
  testingClass1<-inTestNoFog[inTestingClass1]
  
  
  
  #check that are disjoint datasets
  #sum(duplicated(rbind(crossValidating,testing)))
  #sum(duplicated(rbind(unique(training),testing)))
  #sum(duplicated(rbind(unique(training),crossValidating)))
  
  
  # totalCasesFogTrain = nrow(training)
  # totalCasesTrain = round(totalCasesFogTrain/splitPositive)
  # totalCasesNonFogTrain = totalCasesTrain-totalCasesFogTrain
  # 
  # print(totalCasesFogTrain)
  # print(totalCasesTrain)
  # print(totalCasesNonFogTrain)
  
 
 #Class2 data 
  
  
  #####NON-FOGGY CASES
  #####TRAINING
  class2Data<-total[visClass==2]
  #inTrainClass2<-sample(nrow(class2Data),nrow(training))
  inTrainClass2<-sample(nrow(class2Data),size=minSample)
  
  trainingClass2<-class2Data[inTrainClass2]
  
  #####CROSS VALIDATION
  remaining<-class2Data[-inTrainClass2]
  inCrossVal<-sample(nrow(remaining),0.2*minSample)
  
  crossValidatingClass2<-remaining[inCrossVal]
  
  ######TEST SET
  #foggyInTest<-dim(testing[foggy==TRUE])[[1]]
  #nonFoggyForRealisticRatio<-foggyInTest*1/fogNonFogRatio
  #inTestNoFog<-sample(nrow(remaining[-inCrossVal]),nonFoggyForRealisticRatio)
  inTestNoFog<-remaining[-inCrossVal]
  
  #nonFoggyTesting<-remaining[-inCrossVal][inTestNoFog]
  testingClass2<-inTestNoFog
  inTestingClass2<-sample(nrow(inTestNoFog),0.2*minSample)
  testingClass2<-inTestNoFog[inTestingClass2]
  
  
  
  
  #Class3 data 
  
  
  #####NON-FOGGY CASES
  #####TRAINING
  class3Data<-total[visClass==3]
  #inTrainClass2<-sample(nrow(class2Data),nrow(training))
  inTrainClass3<-sample(nrow(class3Data),size=minSample)
  
  trainingClass3<-class3Data[inTrainClass3]
  
  #####CROSS VALIDATION
  remaining<-class3Data[-inTrainClass3]
  inCrossVal<-sample(nrow(remaining),0.2*nrow(class3Data))
  
  crossValidatingClass3<-remaining[inCrossVal]
  
  ######TEST SET
  #foggyInTest<-dim(testing[foggy==TRUE])[[1]]
  #nonFoggyForRealisticRatio<-foggyInTest*1/fogNonFogRatio
  #inTestNoFog<-sample(nrow(remaining[-inCrossVal]),nonFoggyForRealisticRatio)
  inTestNoFog<-remaining[-inCrossVal]
  
  #nonFoggyTesting<-remaining[-inCrossVal][inTestNoFog]
  testingClass3<-inTestNoFog
  inTestingClass3<-sample(nrow(inTestNoFog),0.2*minSample)
  testingClass3<-inTestNoFog[inTestingClass3]
  
  
  
  
  #Class4 data 
  
  
  #####NON-FOGGY CASES
  #####TRAINING
  class4Data<-total[visClass==4]
  #inTrainClass2<-sample(nrow(class2Data),nrow(training))
  inTrainClass4<-sample(nrow(class4Data),size=minSample)
  
  trainingClass4<-class4Data[inTrainClass4]
  
  #####CROSS VALIDATION
  remaining<-class4Data[-inTrainClass4]
  inCrossVal<-sample(nrow(remaining),0.2*nrow(class4Data))
  
  crossValidatingClass4<-remaining[inCrossVal]
  
  ######TEST SET
  #foggyInTest<-dim(testing[foggy==TRUE])[[1]]
  #nonFoggyForRealisticRatio<-foggyInTest*1/fogNonFogRatio
  #inTestNoFog<-sample(nrow(remaining[-inCrossVal]),nonFoggyForRealisticRatio)
  inTestNoFog<-remaining[-inCrossVal]
  
  #nonFoggyTesting<-remaining[-inCrossVal][inTestNoFog]
  testingClass4<-inTestNoFog
  inTestingClass4<-sample(nrow(inTestNoFog),0.2*minSample)
  testingClass4<-inTestNoFog[inTestingClass4]
  
  
  
  
  
  
  
  
  
  
  
  #sum(duplicated(rbind(nonFoggyCrossValidating,nonFoggyTesting)))
  #sum(duplicated(rbind(nonFoggyTraining,nonFoggyTesting)))
  #sum(duplicated(rbind(nonFoggyTraining,nonFoggyCrossValidating)))
  
  
  
  ####Binding the fog and non-fog sets with the corresponding
  training<-rbind(trainingClass1,trainingClass2,trainingClass3,trainingClass4)
  training<-training[sample(nrow(training)),]
  crossValidating<-rbind(crossValidatingClass1,crossValidatingClass2,crossValidatingClass3,crossValidatingClass4)
  crossValidating<-crossValidating[sample(nrow(crossValidating)),]
  testing<-rbind(testingClass1,testingClass2,testingClass3,testingClass4)
  testing<-testing[sample(nrow(testing)),]
  
  dataSets<-list(training,crossValidating,testing)
}





#' Assign images a binary fog property (foggy=TRUE/FALSE)
#' @param dtImages Data table of images from the DB
#' @param dtMeteo Data table of meteo (visibility) from the DB
#' @param visibilityThreshold Numerical with threshold for visbility in meters to be considered fog 
#' @import data.table
#' @export
imagesAndMeteoFogBinary<-function(dtImages, dtMeteo,visibilityThreshold){
  dtImages[,timeSyncToMeteo:=strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(timestamp)/600)*600]
  setkey(dtImages, location_id_closest_KNMI_meteo,timeSyncToMeteo)
  setkey(dtMeteo, location_id,timestamp)
  imagesAndMOR<-dtMeteo[dtImages]
  imagesAndMOR[,foggy:=mor_visibility<=visibilityThreshold]
  imagesAndMOR
}



#' Assign images a multiclass (now considering 4 classes) label fog property (visClass=[1,2,3,4])
#' @param dtImages Data table of images from the DB
#' @param dtMeteo Data table of meteo (visibility) from the DB
#' @param visibilityThreshold Numerical with threshold for visbility in meters to be considered fog 
#' @import data.table
#' @export
imagesAndMeteoFogMulticlass<-function(dtImages, dtMeteo,visibilityThreshold){
  dtImages[,timeSyncToMeteo:=strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(timestamp)/600)*600]
  setkey(dtImages, location_id_closest_KNMI_meteo,timeSyncToMeteo)
  setkey(dtMeteo, location_id,timestamp)
  imagesAndMOR<-dtMeteo[dtImages]
  imagesAndMOR$visClass<-cut(imagesAndMOR$mor_visibility, c(0,visibilityThreshold,Inf), right=FALSE, labels=FALSE)
  
  print(imagesAndMOR)
  
  # imagesAndMOR$visClass[mor_visibility<=visibilityThreshold[[1]]]=1
  # imagesAndMOR$visClass[mor_visibility>visibilityThreshold[[1]] & mor_visibility<=visibilityThreshold[[2]]]=2
  # imagesAndMOR$visClass[mor_visibility>visibilityThreshold[[2]] & mor_visibility<=visibilityThreshold[[3]]]=3
  # imagesAndMOR$visClass[mor_visibility>visibilityThreshold[[3]]]=4
  imagesAndMOR
}





#' Couple cameras and KNMI nearby stations
#' @param dateStrInitial String of initial date do use for fetching data
#' @param dateStrFinal String of final date do use for fetching data
#' @param dbConfigDir String of path with directory containing the DB param access config file
#' @param maxDist Numerical containing max distance from KNMI station
#' @param visibilityThreshold Numerical array with threshold for visbility in meters to be considered fog 
#' @param dayPhaseFlag flag for pahse of day 
#' @import data.table jsonlite DBI
#' @export
coupleImagesAndMeteoToDate<-function(dateStrInitial,dateStrFinal,dbConfigDir,maxDist,visibilityThreshold,dayPhaseFlag=1){
  #the coupling contains also the locations of the station itself
  coupling<-coupleCamerasAndKNMInearStations(maxDistance = maxDist, dbConfigDir)
  dbConfig <- fromJSON(paste0(dbConfigDir,"config.json"))
  con <- dbConnect(RPostgreSQL::PostgreSQL(),
                   dbname = "FOGDB",
                   host = dbConfig[["host"]], port = 9418,
                   user = dbConfig[["user"]], password = dbConfig[["pw"]])
  cameras<-dbReadTable(con, "cameras")
  meteoStations<-dbReadTable(con,"meteo_stations")
  camerasRWSCoupledMeteo <- dbGetQuery(con, paste0("SELECT * FROM cameras
                                                   WHERE location_id IN (", paste(coupling$locationIDsHW, collapse=", "), ");"))
  imagesRWSDayLight <- dbGetQuery(con, paste0("SELECT images.image_id, images.filepath, images.timestamp, images.day_phase, images.camera_id
                                              FROM images
                                              WHERE camera_id IN (", paste(camerasRWSCoupledMeteo$camera_id, collapse=", "), ")AND day_phase=",dayPhaseFlag," AND timestamp>=",dateStrInitial ," AND timestamp<", dateStrFinal,";"))
  
  camerasRWSCoupledMeteo<-data.table(camerasRWSCoupledMeteo)
  imagesRWSDayLight<-data.table(imagesRWSDayLight)
  setkey(camerasRWSCoupledMeteo,camera_id)
  setkey(imagesRWSDayLight,camera_id)
  full<-imagesRWSDayLight[camerasRWSCoupledMeteo]
  coupling$locationIDsHW<-as.numeric(coupling$locationIDsHW)
  setkey(coupling, locationIDsHW)
  setkey(full, location_id)
  full<-full[coupling, nomatch=0]
  meteoStations <- dbGetQuery(con, paste0("SELECT * FROM meteo_stations
                                          WHERE knmi_kis_id IN (", paste(paste0("'",coupling$KISstations,"'"), collapse = ", "), ");"))
  meteoStations<-data.table(meteoStations)
  setkey(meteoStations,knmi_kis_id)
  setkey(full,KISstations)
  full<-full[meteoStations,nomatch=0]
  setnames(full, old=c("i.location_id"), new=c("location_id_closest_KNMI_meteo"))
  
  #meteoConditions <- dbGetQuery(con, paste0("SELECT * FROM meteo_stations
  #             WHERE knmi_kis_id IN (", paste(paste0("'",coupling$KISstations,"'"), collapse = ", "), ");"))
  #######TEST TABLE
  meteoConditions <- dbGetQuery(con, paste0("SELECT location_id, timestamp, mor_visibility 
                                            FROM meteo_features_stations  
                                            WHERE location_id IN (", paste(meteoStations$location_id, collapse=", "), ") AND timestamp>=",dateStrInitial ," AND timestamp<", dateStrFinal,";"))
  meteoConditions<-data.table(meteoConditions)
  dbDisconnect(con)
  if(length(visibilityThreshold)>1){
    mergedRWSandKNMIstations<-imagesAndMeteoFogMulticlass(full, meteoConditions, visibilityThreshold)
    
  } else{
  mergedRWSandKNMIstations<-imagesAndMeteoFogBinary(full, meteoConditions, visibilityThreshold)
  }
  total<-mergedRWSandKNMIstations
  total
}

