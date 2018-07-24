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

indexes<-merged[,num,i.num]

distance[indexes$num,indexes$i.num]
distance[1,2]


