library(DBI)
library(fogDec)
library(jsonlite)
library(data.table)
library(knmiR)
library(postGIStools)
library(parallel)

getValueFromKIS<-function(x, KISStationID, variable, knmiStations){
  if(variable == "mor_visibility"){
    varForKIS <- "MOR_10"
    sensorType <- "A"
  }
  if(variable == "wind_speed"){
    varForKIS <- "FF_10M_10"
    sensorType <- "W"
  }
  if(variable == "air_temp"){
    varForKIS <- "T_DRYB_10"
    sensorType <- "T"
  }
  if(variable == "dew_point"){
    varForKIS <- "T_DEWP_10"
    sensorType <- "T"
  }
  if(variable == "rel_humidity"){
    varForKIS <- "U_10"
    sensorType <- "T"
  }
  
  
  # if(location==1){
  # value<-KIS(var = varForKIS, paste0("260_",sensorType,"_a"), as.character(x)) #DeBilt
  # }
  # if(location==3){
  # value<-KIS(var = varForKIS, paste0("348_",sensorType,"_a"), as.character(x))#Cabauw
  # }
  # if(location==6){
  #   value<-KIS(var = varForKIS, paste0("380_",sensorType,"_22t"), as.character(x))#Beek
  # }
  # if(location==7){
  #   value<-KIS(var = varForKIS, paste0("280_",sensorType,"_23t"), as.character(x))#Eelde
  # }
  # if(location==8){
  #  value<-KIS(var = varForKIS, paste0("344_",sensorType,"_24t"), as.character(x))#Rotterdam Airport
  # }
  #if(location==9){
  geoIdentifier = gsub("SENSORID",sensorType,KISStationID)
  print(geoIdentifier)
    value<-KIS(var = varForKIS, geoIdentifier, period =as.character(x), knmiStationsTable=knmiStations)#Schiphol Airport
  #}
  value
}


`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))



prepareMeteoTable<-function(location, variable,newval){

Sys.setenv(TZ = "UTC")
  
dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])


imagesTable <- as.data.table(dbReadTable(con, "images"))
camerasTable <- as.data.table(dbReadTable(con, "cameras"))
locationsTable <- as.data.table(dbReadTable(con, "locations"))
meteoFeaturesTable <- as.data.table(dbReadTable(con, "meteo_features_stations"))
dbDisconnect(con)


setkey(camerasTable,location_id)
setkey(locationsTable,location_id)
setkey(imagesTable,camera_id)
tempTB<-camerasTable[locationsTable]
setkey(tempTB,camera_id)
tempTB<-imagesTable[tempTB]
tempTB[,timestamp:= strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(tempTB$timestamp)/600)*600]

setkey(tempTB,timestamp,location_id)
setkey(meteoFeaturesTable, timestamp,location_id)
temp2<-tempTB[meteoFeaturesTable]



if(newval==TRUE){
dataNoMeteoAll<-tempTB[(which(tempTB$image_id %not in% temp2$image_id))]#images have no meteo fetures at all
dataNoMeteo<-dataNoMeteoAll[location_id == location, location_id,timestamp]
dataNoMeteo<-dataNoMeteo[,unique(dataNoMeteo)]
datesRequired<-unique(as.Date(dataNoMeteo[,timestamp]))
}else{
  test<-meteoFeaturesTable[location_id==location & is.na(get(variable)),c("location_id","timestamp")]
  datesRequired<-unique(as.Date(test[,timestamp]))

  #values<-lapply(datesRequired, getValueFromKIS, 1, variable)
  #tmp <- within(values, rm(DS_CODE, "TOW.Q_FF_10M_10"))
}

today<-as.Date(Sys.time())
datesToFetch<-datesRequired[datesRequired != today]

#test purposes
#datesToFetch<-seq(as.Date("2016/09/04"), by = "day", length.out = 100)


cl<-makeCluster(4)
clusterEvalQ(cl, library(knmiR))

values<-parLapply(cl,datesToFetch, getValueFromKIS, location, variable)

print(values)
values<-rbindlist(values)
stopCluster(cl)
#values<-rbindlist(values)
#print(values)

values<-data.table(values)

#values<-rbindlist(values)




values[, IT_DATETIME := as.POSIXct(values[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
setnames(values, "IT_DATETIME", "timestamp")

if(variable=="mor_visibility"){
values[TOA.MOR_10  == -1, TOA.MOR_10  := NA]
tmp <- within(values, rm(DS_CODE, "TOA.Q_MOR_10"))
setnames(tmp,"TOA.MOR_10" ,"mor_visibility")
}

if(variable=="wind_speed"){
  ##values[TOW.FF_10M_10  == -1, TOW.FF_10M_10  := NA]
  tmp <- within(values, rm(DS_CODE, "TOW.Q_FF_10M_10"))
  setnames(tmp,"TOW.FF_10M_10" ,"wind_speed")
}

if(variable=="rel_humidity"){
  ##values[TOT.U_10  == -1, TOT.U_10  := NA] ##TO BE ADDED WHEN INFO ARE PROVIDED
  tmp <- within(values, rm(DS_CODE, "TOT.Q_U_10"))
  setnames(tmp,"TOT.U_10" ,"rel_humidity")
}

if(variable=="air_temp"){
  ##values[TOT.T_DRYB_10  == -1, TOT.T_DRYB_10  := NA] ##TO BE ADDED WHEN INFO ARE PROVIDED
  tmp <- within(values, rm(DS_CODE, "TOT.Q_T_DRYB_10"))
  setnames(tmp,"TOT.T_DRYB_10" ,"air_temp")
}

if(variable=="dew_point"){
  ##values[TOT.T_DEWP_10  == -1, TOT.T_DEWP_10  := NA] ##TO BE ADDED WHEN INFO ARE PROVIDED
  tmp <- within(values, rm(DS_CODE, "TOT.Q_T_DEWP_10"))
  setnames(tmp,"TOT.T_DEWP_10" ,"dew_point")
}

tmp[,location_id:=location]


timeSyncToMeteo<-strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(dataNoMeteo$timestamp)/600)*600



if(newval==TRUE){
  #check to put just the missing time stamps otherwise getting exception in the DB if trying to fill meteo for timestamps already present
  toBeFilled<-tmp[which(tmp$timestamp %in% timeSyncToMeteo)]
}

else{
setkey(meteoFeaturesTable, location_id, timestamp)
setkey(tmp, location_id, timestamp)
toBeFilled<-tmp[meteoFeaturesTable, nomatch=0]
toRem<-paste0("i.",variable)
toBeFilled<-toBeFilled[, eval(toRem):=NULL]
}

toBeFilled
}




prepareMeteoTableStationMapping<-function(variable,newval, stationMapping){
  
  Sys.setenv(TZ = "UTC")
  
  dbConfig <- fromJSON("config.json")
  
  con <- dbConnect(RPostgreSQL::PostgreSQL(),
                   dbname = "FOGDB",
                   host = dbConfig[["host"]], port = 9418,
                   user = dbConfig[["user"]], password = dbConfig[["pw"]])
  
  
  imagesTable <- as.data.table(dbReadTable(con, "images"))
  camerasTable <- as.data.table(dbReadTable(con, "cameras"))
  locationsTable <- as.data.table(dbReadTable(con, "locations"))
  meteoFeaturesTable <- as.data.table(dbReadTable(con, "meteo_features_copy"))#######TEST
  KNMIstationsTable <- as.data.table(dbReadTable(con, "meteo_stations"))
  
  
  dbDisconnect(con)
  
  setkey(KNMIstationsTable,knmi_kis_id)
  setkey(stationMapping, KISstations)
  test<-stationMapping[KNMIstationsTable]
  test2<-test[locationIDsHW!=location_id]
  
  locationsOfCamerasMatched<-as.numeric(test2[,locationIDsHW])
  
  stationsCodes<-unique(test2[,KISstations])
  
  
  
  setkey(camerasTable,location_id)
  setkey(locationsTable,location_id)
  setkey(imagesTable,camera_id)
  tempTB<-camerasTable[locationsTable, nomatch=0]
  setkey(tempTB,camera_id)
  tempTB<-imagesTable[tempTB]
  tempTB<-tempTB[location_id %in% locationsOfCamerasMatched] ##basically filter to the cameras where there is a location match with KNMI stations
  tempTB[,timestamp:= strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(tempTB$timestamp)/600)*600]
  
  
  setkey(tempTB,location_id)
  test2$locationIDsHW<-as.numeric(test2$locationIDsHW)
  setkey(test2,locationIDsHW)
  tempTB<-tempTB[test2]
  setnames(tempTB,"i.location_id","meteoStationLocationID")
  
  setkey(tempTB,timestamp,meteoStationLocationID)
  
  setkey(meteoFeaturesTable,timestamp,location_id)
  temp2<-tempTB[meteoFeaturesTable]
  
  if(newval==TRUE){
    dataNoMeteoAll<-tempTB[(which(tempTB$image_id %not in% temp2$image_id))]#images have no meteo fetures at all
    noMeteoSelection<-dataNoMeteoAll[,KISstations,timestamp]
    noMeteoSelection2<-noMeteoSelection[,.(dateOnly=as.Date(timestamp)), by=KISstations]
  }else{
    #TO BE LOOKED INTO
    test<-meteoFeaturesTable[location_id==location & is.na(get(variable)),c("location_id","timestamp")]
    datesRequired<-unique(as.Date(test[,timestamp]))
    
    #values<-lapply(datesRequired, getValueFromKIS, 1, variable)
    #tmp <- within(values, rm(DS_CODE, "TOW.Q_FF_10M_10"))
  }
  
  today<-as.Date(Sys.time())
  noMeteoSelection2<-noMeteoSelection2[dateOnly!=today]
  
  #test purposes
  #datesToFetch<-seq(as.Date("2016/09/04"), by = "day", length.out = 100)
  
  stationsCodesToFetch<-unique(noMeteoSelection2$KISstations)
  
  cl<-makeCluster(8)
  clusterEvalQ(cl, library(knmiR))
  valuesTotal<-NULL
  
  for(knmilocation in stationsCodesToFetch){
  print(knmilocation)
  datesToFetch<-unique(noMeteoSelection2[KISstations==knmilocation,dateOnly])
  values<-parLapply(cl,datesToFetch, getValueFromKIS, knmilocation, variable, KNMIstationsTable)
  
  #values<-lapply(datesToFetch, getValueFromKIS, knmilocation, variable, KNMIstationsTable)
  
  print(values)
  values<-rbindlist(values)
  values<-cbind(rep(KNMIstationsTable[KNMIstationsTable$knmi_kis_id==knmilocation]$location_id,dim(values)[1]),values)
  valuesTotal<-rbind(valuesTotal,values)
  }
  stopCluster(cl)
  #values<-rbindlist(values)
  #print(values)
  
  valuesTotal<-data.table(valuesTotal)
  
  #values<-rbindlist(values)
  
  
  
  
  valuesTotal[, IT_DATETIME := as.POSIXct(valuesTotal[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
  setnames(valuesTotal, "IT_DATETIME", "timestamp")
  
  if(variable=="mor_visibility"){
    valuesTotal[TOA.MOR_10  == -1, TOA.MOR_10  := NA]
    tmp <- within(valuesTotal, rm(DS_CODE, "TOA.Q_MOR_10"))
    setnames(tmp,"TOA.MOR_10" ,"mor_visibility")
  }
  
  if(variable=="wind_speed"){
    ##valuesTotal[TOW.FF_10M_10  == -1, TOW.FF_10M_10  := NA]
    tmp <- within(valuesTotal, rm(DS_CODE, "TOW.Q_FF_10M_10"))
    setnames(tmp,"TOW.FF_10M_10" ,"wind_speed")
  }
  
  if(variable=="rel_humidity"){
    ##valuesTotal[TOT.U_10  == -1, TOT.U_10  := NA] ##TO BE ADDED WHEN INFO ARE PROVIDED
    tmp <- within(valuesTotal, rm(DS_CODE, "TOT.Q_U_10"))
    setnames(tmp,"TOT.U_10" ,"rel_humidity")
  }
  
  if(variable=="air_temp"){
    ##valuesTotal[TOT.T_DRYB_10  == -1, TOT.T_DRYB_10  := NA] ##TO BE ADDED WHEN INFO ARE PROVIDED
    tmp <- within(valuesTotal, rm(DS_CODE, "TOT.Q_T_DRYB_10"))
    setnames(tmp,"TOT.T_DRYB_10" ,"air_temp")
  }
  
  if(variable=="dew_point"){
    ##valuesTotal[TOT.T_DEWP_10  == -1, TOT.T_DEWP_10  := NA] ##TO BE ADDED WHEN INFO ARE PROVIDED
    tmp <- within(valuesTotal, rm(DS_CODE, "TOT.Q_T_DEWP_10"))
    setnames(tmp,"TOT.T_DEWP_10" ,"dew_point")
  }
  
  setnames(tmp,"V1","location_id")

  
  timeSyncToMeteo<-strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(noMeteoSelection$timestamp)/600)*600
  
  
  
  if(newval==TRUE){
    #check to put just the missing time stamps otherwise getting exception in the DB if trying to fill meteo for timestamps already present
    toBeFilled<-tmp[which(tmp$timestamp %in% timeSyncToMeteo)]
  }
  
  else{
    setkey(meteoFeaturesTable, location_id, timestamp)
    setkey(tmp, location_id, timestamp)
    toBeFilled<-tmp[meteoFeaturesTable, nomatch=0]
    toRem<-paste0("i.",variable)
    toBeFilled<-toBeFilled[, eval(toRem):=NULL]
  }
  
  toBeFilled
}






#Updates meteo variables in the DB for which at least one meteo variable has been already introduced for the 
#corresponding date and time
updateExistingMeteo<-function(){
variables<-c("air_temp","dew_point","mor_visibility","wind_speed","rel_humidity")
locations<-c(1,3,6,7,8) #schiphol at the moment is waiting,9)

for(var in variables){
  for(loc in locations){
    tmp<-prepareMeteoTable(loc, var, FALSE)
    message(paste("FINISHED location ",loc, " and variable ", var))
    
    dbConfig <- fromJSON("config.json")

    con <- dbConnect(RPostgreSQL::PostgreSQL(),
                     dbname = "FOGDB",
                     host = dbConfig[["host"]], port = 9418,
                     user = dbConfig[["user"]], password = dbConfig[["pw"]])
    tryCatch(
     {

    postgis_update(con,tmp,"meteo_features_stations",id_cols = "meteo_feature_id",update_cols = var)
      },
     error=function(cond) {
       message("data not available to update table")
       message(cond)
     })
   dbDisconnect(con)
  }
}

}




#####################################################################################################

#locationsMeteo<-c(1,3,6,7,8,9)


stationMapping<-coupleCamerasAndKNMInearStations(maxDistance = 7500)
table<-prepareMeteoTableStationMapping(variable = "mor_visibility", newval = TRUE, stationMapping)
dbConfig <- fromJSON("config.json")
con <- dbConnect(RPostgreSQL::PostgreSQL(),
                   dbname = "FOGDB",
                  host = dbConfig[["host"]], port = 9418,
                  user = dbConfig[["user"]], password = dbConfig[["pw"]])
dbWriteTable(con, "meteo_features_copy", table, append = TRUE, row.names = FALSE, match.cols = TRUE)
dbDisconnect(con)


# dbConfig <- fromJSON("config.json")
# 
# con <- dbConnect(RPostgreSQL::PostgreSQL(),
#                  dbname = "FOGDB",
#                  host = dbConfig[["host"]], port = 9418,
#                  user = dbConfig[["user"]], password = dbConfig[["pw"]])
# KNMIstationsTable <- as.data.table(dbReadTable(con, "meteo_stations"))
# dbDisconnect(con)
# 
# 
# 
# setkey(KNMIstationsTable,knmi_kis_id)
# setkey(stationCouple, KISstations)
# test<-stationCouple[KNMIstationsTable]
# test2<-test[locationIDsHW!=location_id]
# stationsCodes<-unique(test2[,KISstations])
# 
# 
# 
# for(i in stationsCodes){
# dbConfig <- fromJSON("config.json")
# # #
#  con <- dbConnect(RPostgreSQL::PostgreSQL(),
#                      dbname = "FOGDB",
#                     host = dbConfig[["host"]], port = 9418,
#                     user = dbConfig[["user"]], password = dbConfig[["pw"]])
#  tmp<-prepareMeteoTable(location = i, variable = "mor_visibility", newval = TRUE )
#  message(paste("location",i,"ready to be written on DB"))
#  dbWriteTable(con, "meteo_features_stations", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
# dbDisconnect(con)
# }
# #####################################################################################################
