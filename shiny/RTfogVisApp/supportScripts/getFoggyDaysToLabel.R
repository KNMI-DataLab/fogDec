library(DBI)
library(jsonlite)
library(data.table)

dbConfig <-   DB_conf_file<-"/external/config/configDB.json"

dbConfig <- fromJSON(DB_conf_file)

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])



imagesToLabel <- as.data.table(dbGetQuery(con, "SELECT * FROM images WHERE 
(timestamp >= '2019-12-30' AND timestamp <  '2020-01-02') OR
timestamp = '2019-12-03' OR
(timestamp >= '2020-01-21' AND timestamp <  '2020-01-24')
  LIMIT 10;"))

saveRDS(imagesToLabel,file = "/external/data/promisingFoggyDays.RDS")