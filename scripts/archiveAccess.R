library(mongolite)
library(data.table)
library(lubridate)


con<-mongo(collection = "collection", db = "fogDetectionArchive", url = "mongodb://145.23.219.231:27017")
data<-con$find('{}')
test<-data$features
test22<-lapply(test,function(x){data.table(x$properties)})
df<-rbindlist(test22)
df<-data.table(df)
df[, timeStamp:=posi]
df[,timeStamp:=ymd_hms(df$timeStamp,tz = "UTC")]
sample16Dic<-df[date(timeStamp)==ymd("2018-12-16")]
saveRDS(df,"archivePredictions.RDS")
