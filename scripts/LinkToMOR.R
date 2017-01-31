## Link to MOR (new results only evaluate pictures)

path <- system.file("extdata/Sensor", package="visDec")
sensorFiles <- list.files(path,
                          pattern=glob2rx("Twente*.csv"),
                          full.names=TRUE)
sensorData <- ReadMORSensorData(sensorFiles)
setkey(sensorData, dateTime)
setkey(imageSummary, dateTime)
imageSummary <- SynchronizeSensorPicture(sensorData, imageSummary)
#imageSummary <- merge(imageSummary, sensorData)
imageSummary[, MOR := TOA.MOR_10, by = dateTime]
