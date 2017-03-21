library(DBI)
library(jsonlite)

dbConfig <- fromJSON("config.json")

connectionSetup <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = "FOGDB",
                host = dbConfig[["host"]], port = 5432,
                user = dbConfig[["user"]], password = dbConfig[["pw"]])



insertionDayPhase <- dbGetQuery(connectionSetup, "INSERT INTO day_phases ( day_phase_id, day_phase_description)
                                                  VALUES (0, 'night'), (1, 'day'), (10, 'civil dusk'),
                                                  (11, 'civil twilight'), (20, 'nautical dusk'), (21, 'nautical twilight'),
                                                  (30, 'astronomical dusk'), (31, 'astronomical twilight');")




                          

dbDisconnect(connectionSetup)




