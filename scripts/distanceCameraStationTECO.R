library(jsonlite)
library(DBI)
library(postGIStools)
library(geosphere)
library(stats)
library(data.table)
Sys.setenv(TZ = "UTC")
dbConfig <- fromJSON("/home/pagani/development/fogDec/config.json")
con <- dbConnect(RPostgreSQL::PostgreSQL(),
dbname = "FOGDB",
host = dbConfig[["host"]], port = 9418,
user = dbConfig[["user"]], password = dbConfig[["pw"]])
locationsTable <- as.data.table(dbReadTable(con, "locations"))
dbDisconnect(con)
matPositions<-cbind(locationsTable$longitude,locationsTable$latitude)
matPositions<-matrix(matPositions,ncol=2)
#distm(matPositions)
distance<-distm(matPositions)
locationsTable$num<-1:dim(locationsTable)[[1]]
locationsTable

trainValTestSetList2500<-createTrainValidTestSetsBinary("~/share/", dateMax= "\'2018-05-14 00:00:00\'", dbConfigDir = "~/development/fogDec/",maxDist=2500)

all2500<-rbind(trainValTestSetList2500[[1]],trainValTestSetList2500[[2]],trainValTestSetList2500[[3]])

#locationsTable[all2500$location_id==num,]$num
setkey(locationsTable,location_id)
setkey(all2500,location_id)


merged<-locationsTable[all2500,nomatch=0]


setkey(merged,i.location_id)

merged<-locationsTable[merged,nomatch=0]

indexes<-cbind(merged$num,merged$i.num)

allDist<-distance[indexes]



merged[,distKNMIstationCam:=allDist]



distance[indexes$num,indexes$i.num]
allDist<-distance[indexes]
merged[,distKNMIstationCam:=allDist]



files<-sapply(merged$filepath, function(x) gsub(".*/AXIS214/", "oldArchiveDEBILT/",x))
files<-sapply(files, function(x) gsub(".*/CAMERA/", "",x))
files<-sapply(files, function(x) gsub(".*/cabauw/", "oldArchiveCABAUW/cabauw/",x))



setwd("~/share/")

#files<-files[1:20]


resolutionImg<-28

cl <- makeCluster(45)
registerDoParallel(cl)

clusterEvalQ(cl, library("imager"))

matRWS<-foreach(i=1:length(files)) %dopar%{
  message(files[[i]])
  image<-tryCatch(
    load.image(files[[i]]),

    error=function(error_message) {
      #message("Yet another error message.")
      #message("Here is the actual R error message:")
      #next
      return(NA)
    }
  )
  if(is.na(image[[1]])){
    v<-NA*1:(resolutionImg*resolutionImg)
    message("Image not available error in acquisition")
    v
  }else{
    image<-resize(image,resolutionImg,resolutionImg)
    image<-blur_anisotropic(image, amplitude = 10000)
    df<-as.data.frame(image)
    v<-df$value
    #mat<-rbind(mat,v)
    v
  }
}

stopCluster(cl)


matRWS<-do.call(rbind,matRWS)


dtMat<-data.table(matRWS)
dtMat<-cbind(dtMat,merged)
#[,foggy:=merged$foggy]
#dtMat[,filepath:=testSetDiff$filepath]
complete<-dtMat[complete.cases(dtMat)]


write.csv(complete,"~/nndataH2O/TECO/allData2500.csv", row.names = F)




resultsOnThreshold<-function(predictions,filename,threshold){
  
  predictionDT<-as.data.table(predictions)
  
  
  results<-fread(filename)
  results<-results[,2354:2378]
  setnames(results, old = "foggy", new = "groundTruth")
  #results$groundTruth<-as.data.table(h2oFrame$foggy)
  results$prediction<-as.factor(predictionDT$TRUE.>=threshold)
  
  results
}




library(h2o)

h2o.init(nthreads=-1, max_mem_size="120G")
h2o.removeAll() ## clean slate - just in cas

h2oTrainingFrame<-h2o.importFile("/home/pagani/nndataH2O/h2oFrames/trainingh2o.csv")
h2oAllFrame<-h2o.importFile("/home/pagani/nndataH2O/TECO/allData2500.csv")

filename<-"/home/pagani/nndataH2O/TECO/allData2500.csv"

loadedModel<-h2o.loadModel("/home/pagani/nndataH2O/TECO/2500model/dl_grid_model_2")

perfTraining<-h2o.performance(loadedModel,h2oTrainingFrame)

threshold<-perfTraining@metrics$max_criteria_and_metric_scores$threshold[[1]]

predOnAll<-h2o.predict(loadedModel, h2oAllFrame)
resultsTest<-resultsOnThreshold(predOnAll,filename,threshold)






