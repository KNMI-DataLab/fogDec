#' Divide the training and test set based on day number
#' @param fullFilename String
#' @param pattern String encoding the filename pattern
#' @export

assignTrainAndTestSet<-function(direcoryPath){
  
  library(doParallel)
  registerDoParallel(cores=2)
  library(lubridate)
  
  filenames <- list.files(direcoryPath,
                          pattern=glob2rx("Meetterrein_201510*.jpg"),
                          full.names=TRUE, recursive = TRUE)
  
  
  destinationDir <- "~/MYTEST/"
  
  
  #imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
  for(file in filenames){
    fileInformation <- FileNameParser(file, "na*me_yyyymmdd_hhmm.jpg")
    if (yday(fileInformation$dateTime) %% 2 == 0){
      fileInformation$trainTest = "train"
      dir.create(paste0(destinationDir, "train/", year(fileInformation$dateTime), month(fileInformation$dateTime)))
      file.copy(file, paste0(destinationDir, "train/", year(fileInformation$dateTime), month(fileInformation$dateTime)))
    }
    else{
      fileInformation$trainTest = "test"
    }
    
    fileInformation
  }
}









