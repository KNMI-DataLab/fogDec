#' Extracts name and date time from filename
#' @param fullFilename String
#' @param pattern String encoding the filename pattern
#' @export
FileNameParser <- function(fullFilename, pattern) {
  if (!file.exists(fullFilename)) stop("File does not exist.")
  if (pattern != "na*me_yyyymmdd_hhmm.jpg" & pattern != "na*me_yyyymmddhhmm.jpg" & pattern != "na*me_yyyy-mm-dd_hhmm.jpg") stop("pattern not implemented")
  tmp <- strsplit(fullFilename, "/")[[1]]
  tmp <- tmp[length(tmp)]
  name <- tmp
  tmp <- strsplit(tmp, "_")[[1]]
  if (length(tmp) == 3){
    filePrefix <- tmp[1]
    date <- tmp[2]
    time <- substr(tmp[3], 1, 4)
    year <- substr(date,1,4)
    month <- substr(date, 5, 6)
    day   <- substr(date, 7, 8)
    hour  <- substr(time, 1, 2)
    min   <- substr(time, 3, 4)
  }
  else if (length(tmp) == 2) {
    filePrefix <- tmp[1]
    date <- substr(tmp[2], 1, 8)
    time <- substr(tmp[2], 9, 12)
    year <- substr(date,1,4)
    month <- substr(date, 5, 6)
    day   <- substr(date, 7, 8)
    hour  <- substr(time, 1, 2)
    min   <- substr(time, 3, 4)
  }
  #Scotland pattern: it has to be re-considered for the future there are 
  #cameras with multiple ____ that might create some problems
  else if (length(tmp) == 4) {
    filePrefix <- paste0(tmp[1], "_", tmp[2])
    date <- substr(tmp[3], 1, 10)
    time <- substr(tmp[4], 1, 6)
    year <- substr(date,1,4)
    month <- substr(date, 6, 7)
    day   <- substr(date, 9, 10)
    hour  <- substr(time, 1, 2)
    min   <- substr(time, 3, 4)
  }
  else stop("filename does not match specified pattern")
  dateTime <- as.POSIXct(paste(paste(year,month,day,sep="-"),
                               paste(hour,min,sep=":")),
                         tz = "UTC")
  return(data.table(filePrefix = filePrefix, filePath = fullFilename,
                    dateTime = dateTime))
}
