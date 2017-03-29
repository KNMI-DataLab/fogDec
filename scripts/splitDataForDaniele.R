library(data.table)

library(DBI)
library(jsonlite)

dbConfig <- fromJSON("config.json")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 22022,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])

dbGetQuery(con, "SELECT table_name FROM INFORMATION_SCHEMA.tables WHERE table_schema = ANY (current_schemas(false));")

tmp <- as.data.table(dbGetQuery(con, "SELECT * FROM meteo_images;"))

dbDisconnect(con)

tmp[, vis_class := cut(mor_visibility, breaks = c(0, 250, 1000, 3000, 50000), 
                       labels = c("A", "B", "C", "D"),
                       right = FALSE, ordered_result = TRUE)]

tmp <- na.omit(tmp)

set.seed(123)

tmp <- tmp[sample(1:.N), ]

fogData <- tmp[vis_class != "D", ]
fogData <- rbind(fogData, tmp[vis_class == "D"][1 : 2000, ])

trainIndex <- caret::createDataPartition(fogData$vis_class, p = .75, list = FALSE, times = 1)

fogTrain <- fogData[trainIndex, ]
fogTest  <- fogData[-trainIndex, ]

setkey(fogTrain, vis_class, location_id)
setkey(fogTest, vis_class, location_id)


fogTrain[, .N, by = .(vis_class, location_id)]
fogTest[, .N, by = .(vis_class, location_id)]

fogTest[, .N, by = .(vis_class, location_id)][, N] / fogTrain[, .N, by = .(vis_class, location_id)][, N]

