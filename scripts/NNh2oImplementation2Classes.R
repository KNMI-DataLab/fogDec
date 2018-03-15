library(h2o)

h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll() ## clean slate - just in case the cluster was already running


photoDir<-"/usr/people/pagani/share"
setwd(photoDir) ##For RStudio



trainValTestSetList<-createTrainValidTestSetsBinary("~/share/", dbConfigDir = "/usr/people/pagani/development/fogVisibility/fogDec/")

trainSet<-trainValTestSetList[[1]]
validSet<-trainValTestSetList[[2]]
testSet<-trainValTestSetList[[3]]








# spiral <- h2o.importFile(path = normalizePath("../data/spiral.csv"))
# grid   <- h2o.importFile(path = normalizePath("../data/grid.csv"))
# # Define helper to plot contours
# plotC <- function(name, model, data=spiral, g=grid) {
#   data <- as.data.frame(data) #get data from into R
#   pred <- as.data.frame(h2o.predict(model, g))
#   n=0.5*(sqrt(nrow(g))-1); d <- 1.5; h <- d*(-n:n)/n
#   plot(data[,-3],pch=19,col=data[,3],cex=0.5,
#        xlim=c(-d,d),ylim=c(-d,d),main=name)
#   contour(h,h,z=array(ifelse(pred[,1]=="Red",0,1),
#                       dim=c(2*n+1,2*n+1)),col="blue",lwd=2,add=T)
# }
# 
# 
# par(mfrow=c(2,2)) #set up the canvas for 2x2 plots
# plotC( "DL", h2o.deeplearning(1:2,3,spiral,epochs=1e3))
# 
# #dev.new(noRStudioGD=FALSE) #direct plotting output to a new window
# par(mfrow=c(2,2)) #set up the canvas for 2x2 plots
# ep <- c(1,250,500,750)
# plotC(paste0("DL ",ep[1]," epochs"),
#       h2o.deeplearning(1:2,3,spiral,epochs=ep[1],
#                        model_id="dl_1"))
# plotC(paste0("DL ",ep[2]," epochs"),
#       h2o.deeplearning(1:2,3,spiral,epochs=ep[2],
#                        checkpoint="dl_1",model_id="dl_2"))
# plotC(paste0("DL ",ep[3]," epochs"),
#       h2o.deeplearning(1:2,3,spiral,epochs=ep[3],
#                        checkpoint="dl_2",model_id="dl_3"))
# plotC(paste0("DL ",ep[4]," epochs"),
#       h2o.deeplearning(1:2,3,spiral,epochs=ep[4],
#                        checkpoint="dl_3",model_id="dl_4"))
# 
# #dev.new(noRStudioGD=FALSE) #direct plotting output to a new window
# par(mfrow=c(2,2)) #set up the canvas for 2x2 plots
# for (hidden in list(c(11,13,17,19),c(42,42,42),c(200,200),c(1000))) {
#   plotC(paste0("DL hidden=",paste0(hidden, collapse="x")),
#         h2o.deeplearning(1:2,3,spiral,hidden=hidden,epochs=500))
}


