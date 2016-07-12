library(fogDec)
library(data.table)

context("Filename parsing")

midnightFileType1 <- "../../inst/extdata/Meetterrein_20151009_0000.jpg"
standardFileType1 <- "../../inst/extdata/Meetterrein_20151009_0610.jpg"
sensorTestFileType1 <- "../../inst/extdata/MOR_DeBilt_201510.txt"

test_that("FileNameParser", {
  expect_match(as.character(FileNameParser(standardFileType1, "na*me_yyyymmdd_hhmm.jpg")$name), "Meetterrein")
  expect_match(paste(FileNameParser(standardFileType1, "na*me_yyyymmdd_hhmm.jpg")$dateTime), "2015-10-09 06:10:00")
  expect_equal_to_reference(ReadMORSensorData(sensorTestFileType1), "sensorOutputDeBilt.rds")
})


midnightFileType2 <- "../../inst/extdata/EHTW_201512010000.jpg"
standardFileType2 <- "../../inst/extdata/EHTW_201512010610.jpg"
sensorTestFileType2 <- "../../inst/extdata/MOR_Twente_201512_10min_full.csv"


test_that("FileNameParser", {
  expect_match(as.character(FileNameParser(standardFileType2, "na*me_yyyymmddhhmm.jpg")$name), "EHTW")
  expect_match(paste(FileNameParser(standardFileType2, "na*me_yyyymmddhhmm.jpg")$dateTime), "2015-12-01 06:10:00")
  expect_equal_to_reference(ReadMORSensorDataTWE(sensorTestFileType2), "sensorOutputTWE.rds")
})