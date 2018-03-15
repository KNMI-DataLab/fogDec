#' Create training, validation and test datasets
#' @param dataDir String of directory containing the cameras
#' @param dateMax String of latest date do use for fetching data
#' @param dbConfigDir String of path with directory containing the DB param access config file
#' @import data.table
#' @export
createTrainValidTestSetsBinary<-function(dataDir,dateMax= "\'2018-02-28 00:00:00\'",dbConfigDir){

Sys.setenv(TZ = "UTC")
#resolutionImg<-28

total<-coupleImagesAndMeteoToDate(dateMax,dbConfigDir)

set.seed(11)

nFog<-dim(total[foggy==TRUE])[[1]]
nNonFog<-dim(total[foggy==FALSE])[[1]]

fogNonFogRatio<-nFog/nNonFog


#FOGGY CASES
#####TRAINING

foggyData<-total[foggy==TRUE]
inTraining<-sample(nrow(foggyData),0.6*nrow(foggyData))
trainingSmall<-foggyData[inTraining]
inTrainingMore<-sample(nrow(trainingSmall),200000, replace = T)
training<-trainingSmall[inTrainingMore]


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
inTrainNoFog<-sample(nrow(nonFoggyData),nrow(training))
nonFoggyTraining<-nonFoggyData[inTrainNoFog]

#####CROSS VALIDATION
remaining<-nonFoggyData[-inTrainNoFog]
inCrossVal<-sample(nrow(remaining),0.2*nrow(foggyData))

nonFoggyCrossValidating<-remaining[inCrossVal]

######TEST SET
foggyInTest<-dim(testing[foggy==TRUE])[[1]]
nonFoggyForRealisticRatio<-foggyInTest*1/fogNonFogRatio
inTestNoFog<-sample(nrow(remaining[-inCrossVal]),nonFoggyForRealisticRatio)

nonFoggyTesting<-remaining[-inCrossVal][inTestNoFog]


sum(duplicated(rbind(nonFoggyCrossValidating,nonFoggyTesting)))
sum(duplicated(rbind(nonFoggyTraining,nonFoggyTesting)))
sum(duplicated(rbind(nonFoggyTraining,nonFoggyCrossValidating)))



####Binding the fog and non-fog sets with the corresponding
training<-rbind(training,nonFoggyTraining)
crossValidating<-rbind(crossValidating,nonFoggyCrossValidating)
testing<-rbind(testing,nonFoggyTesting)

dataSets<-list(training,crossValidating,testing)
}


#' Assign images a binary fog property (foggy=TRUE/FALSE)
#' @param dtImages Data table of images from the DB
#' @param dtMeteo Data table of meteo (visibility) from the DB
#' @import data.table
#' @export
imagesAndMeteoFogBinary<-function(dtImages, dtMeteo){
  dtImages[,timeSyncToMeteo:=strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(timestamp)/600)*600]
  setkey(dtImages, location_id_closest_KNMI_meteo,timeSyncToMeteo)
  setkey(dtMeteo, location_id,timestamp)
  imagesAndMOR<-dtMeteo[dtImages]
  imagesAndMOR[,foggy:=mor_visibility<=250]
  imagesAndMOR
}


#' Couple cameras and KNMI nearby stations
#' @param dateStr String of latest date do use for fetching data
#' @param dbConfigDir String of path with directory containing the DB param access config file
#' @import data.table jsonlite DBI
#' @export
coupleImagesAndMeteoToDate<-function(dateStr,dbConfigDir){
  #the coupling contains also the locations of the station itself
  coupling<-coupleCamerasAndKNMInearStations(maxDistance = 7500,dbConfigDir)
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
                                              WHERE camera_id IN (", paste(camerasRWSCoupledMeteo$camera_id, collapse=", "), ")AND day_phase=1 AND timestamp<", dateStr,";"))
  
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
                                            FROM meteo_features_copy  
                                            WHERE location_id IN (", paste(meteoStations$location_id, collapse=", "), ") AND timestamp<", dateStr,";"))
  meteoConditions<-data.table(meteoConditions)
  dbDisconnect(con)
  mergedRWSandKNMIstations<-imagesAndMeteoFogBinary(full, meteoConditions)
  total<-mergedRWSandKNMIstations
  total
}

