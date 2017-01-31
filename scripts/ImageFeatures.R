## script: compute image features

## Load Libraries
library(data.table)
library(fogDec)
library(visDec)
library(ggplot2)
library(doParallel)
library(imager)
library(changepoint) # functionality should be included in imager
library(maptools)



#cl<-makePSOCKcluster(2)
#primary<-'127.0.0.1'
#user<-'andrea'
#machineAddresses <- list(
 #    list(host=primary,user=user,
  #      ncore=2)
 # )

## make cluster
registerDoParallel(cores=3)

i<-0
machines<-list()
## the users and addresses are based on the AWS configuration
user    <- 'ubuntu'
primary <- '172.31.45.30'

#IPs contains a list of slaves that will run the computations
#IPs<-paste0("172.31.46.", seq(from = 157, to = 174))
#IPs<-c(IPs, "172.31.38.73") ##slave gold master machine
IPs<-c("172.31.38.73")
for (ip in IPs){
  i<-i+1
  machines[[i]]<-list(host=ip, user = user, ncore=1)
}

machineAddresses <- list(
  list(host=primary,user=user,
       ncore=1)
)
machineAddresses<-c(machineAddresses,machines)

#characteristics of the cluster are assigned (e.g., IPs, hosts, users, IPs)
spec <- lapply(machineAddresses,
               function(machine) {
                  rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
                })
spec <- unlist(spec,recursive=FALSE)

#cluster is created (the communication between master and slaves takes place on the port 11000 and is a SSH-like session)
parallelCluster <- parallel::makeCluster(type='PSOCK',
                                         master=primary,
                                         spec=spec,
                                         port=11000, outfile="")
print(parallelCluster)


#source("./R/CoreFeatureCompute.R")# we might export those functions


##some libraries and functions are explicitly exported
#clusterEvalQ(parallelCluster, library(imager), FileNameParser())
parallelCluster <- cl
clusterEvalQ(parallelCluster, c(library(imager),library(data.table)))
clusterEvalQ(parallelCluster, c(library(fogDec)))
clusterExport(parallelCluster,"FileNameParser")
registerDoParallel(parallelCluster)


## get configuration

propertiesLocations <- fread("properties.csv") #, stringsAsFactors = FALSE)

#print(propertiesLocations)

## extract features save the data.table (do not print anything / besides progress)


apply(propertiesLocations, 1, fogDec:::featureExtraction)


##stop the cluster
stopImplicitCluster()










