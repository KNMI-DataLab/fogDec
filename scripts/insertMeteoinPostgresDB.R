library(DBI)
library(jsonlite)
library(data.table)
library(knmiR)
library(postGIStools)
library(parallel)

getValueFromKIS<-function(x, location, variable){
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
  
  
  if(location==1){
  value<-KIS(var = varForKIS, paste0("260_",sensorType,"_a"), as.character(x)) #DeBilt
  }
  if(location==3){
  value<-KIS(var = varForKIS, paste0("348_",sensorType,"_a"), as.character(x))#Cabauw
  }
  if(location==6){
    value<-KIS(var = varForKIS, paste0("380_",sensorType,"_22t"), as.character(x))#Beek
  }
  if(location==7){
    value<-KIS(var = varForKIS, paste0("280_",sensorType,"_23t"), as.character(x))#Eelde
  }
  if(location==8){
   value<-KIS(var = varForKIS, paste0("344_",sensorType,"_24t"), as.character(x))#Rotterdam Airport
  }
  if(location==9){
    value<-KIS(var = varForKIS, paste0("240_",sensorType,"_18Ct"), as.character(x))#Schiphol Airport
  }
  value
}



prepareMeteoTable<-function(location, variable,newval){


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
setkey(tempTB,timestamp,location_id)
setkey(meteoFeaturesTable, timestamp,location_id)
temp2<-tempTB[meteoFeaturesTable]



if(newval==TRUE){

dataNoMeteoAll<-tempTB[!(which(tempTB$image_id %in% temp2$image_id))]#images have no meteo fetures at all
dataNoMeteo<-dataNoMeteoAll[location_id == location, location_id,timestamp]
dataNoMeteo<-dataNoMeteo[,unique(dataNoMeteo)]
datesRequired<-unique(as.Date(dataNoMeteo[,timestamp]))
}
else{
  test<-meteoFeaturesTable[location_id==location & is.na(get(variable)),c("location_id","timestamp")]
  datesRequired<-unique(as.Date(test[,timestamp]))

  #values<-lapply(datesRequired, getValueFromKIS, 1, variable)
  #tmp <- within(values, rm(DS_CODE, "TOW.Q_FF_10M_10"))
}

today<-as.Date(Sys.time())
datesToFetch<-datesRequired[datesRequired != today]

#print(datesToFetch[1:20])


#testing parallel processes fetching the data via wget, but issues as {"failure":"Het,recept,kan,niet,opgevraagd,worden
#cl<-makeCluster(4)
#clusterEvalQ(cl, library(knmiR))

values<-lapply(datesToFetch, getValueFromKIS, location, variable)

#print(values)
values<-rbindlist(values)
#stopCluster(cl)
#values<-rbindlist(values)
#print(values)

#values<-data.table(values)

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


if(newval==TRUE){
  #check to put just the missing time stamps otherwise getting exception in the DB if trying to fill meteo for timestamps already present
  toBeFilled<-tmp[which(tmp$timestamp %in% dataNoMeteo$timestamp)]
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
#tmp<-prepareMeteoTable(6, "dew_point",FALSE)

#postgis_update(con,tmp,"meteo_features_stations",id_cols = "meteo_feature_id",update_cols = "rel_humidity")
}


















#####################################################################################################
# dbConfig <- fromJSON("config.json")
#  
# con <- dbConnect(RPostgreSQL::PostgreSQL(),
#                   dbname = "FOGDB",
#                   host = dbConfig[["host"]], port = 9418,
#                   user = dbConfig[["user"]], password = dbConfig[["pw"]])
# tmp<-prepareMeteoTable(location = 1, variable = "airTemp" )
#dbWriteTable(con, "meteo_features_stations", tmp, append = TRUE, row.names = FALSE, match.cols = TRUE)
#dbDisconnect(con)
#####################################################################################################



























