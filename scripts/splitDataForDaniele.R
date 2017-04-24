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

fogTrain[, basename := basename(filepath)]
fogTest[, basename := basename(filepath)]

folder <- "~/Desktop/Daniel/Training/"

# fogTrain[1, filepath]

# file.copy(fogTrain[1, filepath], paste0(folder, "test.jpg"))

fogTrain[, file.copy(filepath, paste0(folder, basename))]
fogTrain[, filepath := NULL]
saveRDS(fogTrain, file = "~/Desktop/Daniel/Training/ImageDescription.rds")
write.csv(fogTrain, file = "~/Desktop/Daniel/Training/ImageDescription.csv", row.names = FALSE, col.names = TRUE)

folder <- "~/Desktop/Daniel/Testing/"

# fogTrain[1, filepath]

# file.copy(fogTrain[1, filepath], paste0(folder, "test.jpg"))

fogTest[, file.copy(filepath, paste0(folder, basename))]
fogTest[, filepath := NULL]
saveRDS(fogTest, file = "~/Desktop/Daniel/Testing/ImageDescription.rds")
write.csv(fogTest, file = "~/Desktop/Daniel/Testing/ImageDescription.csv", row.names = FALSE, col.names = TRUE)


## update fogTrain and fogTest
tmp2 <- tmp[, .(image_id, mor_visibility)]

fogTrain2 <- merge(fogTrain, tmp2, by = "image_id")
fogTrain2[, mor_visibility.x := mor_visibility.y]
fogTrain2[, mor_visibility.y := NULL]
setnames(fogTrain2, "mor_visibility.x", "mor_visibility")
fogTrain2[, vis_class := cut(mor_visibility, breaks = c(0, 250, 1000, 3000, 50000), 
                       labels = c("A", "B", "C", "D"),
                       right = FALSE, ordered_result = TRUE)]

saveRDS(fogTrain2, file = "~/Desktop/Daniel/Training/ImageDescription2.rds")
write.csv(fogTrain2, file = "~/Desktop/Daniel/Training/ImageDescription2.csv", row.names = FALSE, col.names = TRUE)

fogTest2 <- merge(fogTest, tmp2, by = "image_id")
fogTest2[, mor_visibility.x := mor_visibility.y]
fogTest2[, mor_visibility.y := NULL]
setnames(fogTest2, "mor_visibility.x", "mor_visibility")
fogTest2[, vis_class := cut(mor_visibility, breaks = c(0, 250, 1000, 3000, 50000), 
                       labels = c("A", "B", "C", "D"),
                       right = FALSE, ordered_result = TRUE)]

saveRDS(fogTest2, file = "~/Desktop/Daniel/Testing/ImageDescription2.rds")
write.csv(fogTest2, file = "~/Desktop/Daniel/Testing/ImageDescription2.csv", row.names = FALSE, col.names = TRUE)

