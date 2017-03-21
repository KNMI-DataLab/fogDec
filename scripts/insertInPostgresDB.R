library(DBI)
library(jsonlite)

dbConfig <- fromJSON("config.json")

connectionSetup <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = "FOGDB",
                host = dbConfig[["host"]], port = 5432,
                user = dbConfig[["user"]], password = dbConfig[["pw"]])



insertionDayPhase <- dbGetQuery(connectionSetup, "INSERT INTO day_phases ( day_phase_id, day_phase_description)
                                                  VALUES (0, 'night'), (1, 'day'), (10, 'civil dawn'),
                                                  (11, 'civil dusk'), (20, 'nautical dawn'), (21, 'nautical dusk'),
                                                  (30, 'astronomical dawn'), (31, 'astronomical dusk');")


dfScotlandLocations <- read.csv("inst/extScripts/scotlandLatLon.csv", stringsAsFactors = F)
dfScotlandLocationsToWrite <- dfScotlandLocations[,3:5]
colnames(dfScotlandLocationsToWrite) <- c("location_description","longitude","latitude")
dfScotlandLocationsToWrite$location_description <- paste("UK", dfScotlandLocationsToWrite$location_description)
dbWriteTable(connectionSetup, "locations", dfScotlandLocationsToWrite, append = T)

                          

dbDisconnect(connectionSetup)




