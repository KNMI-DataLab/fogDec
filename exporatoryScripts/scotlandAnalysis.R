scotland709<-readRDS("results/ResultFeatures7-709.rds")





filtered709[fractalDim<=2.2]
#image seems out of focus






filtered709[meanHue<=60]

#images of the previous night are kept in the system: fault of the scottish system

# 7-709 2016-11-18 08:00:00
# 2:      7-709 2016-11-18 08:10:00
# 3:      7-709 2016-11-18 08:20:00
# 4:      7-709 2016-11-18 08:30:00
# 5:      7-709 2016-11-18 08:40:00
# 6:      7-709 2016-11-18 08:50:00
# 7:      7-709 2016-11-18 09:00:00
# 8:      7-709 2016-11-18 09:10:00
# 9:      7-709 2016-11-18 09:20:00
#others are pictures in gray given the still early morning. Camera take shoots in B/W when there is not enough light?



filtered709[meanHue>=170 & fractalDim<2.3]
#scenary is changed



clusters<-hclust(dist(filtered709[,meanEdge,fractalDim]))
clusterCut<-cutree(clusters,5)
ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
clusters<-hclust(dist(filtered709[,meanEdge,fractalDim,changePoint]))
clusters<-hclust(dist(filtered709[,c(meanEdge,fractalDim,changePoint)]))
clusterCut<-cutree(clusters,5)
clusters<-hclust(dist(filtered709[,c(meanEdge,fractalDim,changePoint)]))
clusterCut<-cutree(clusters,5)
ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
filtered709[,c(meanEdge,meanHue)]
filtered709[,c(meanEdge,changePoint)]
filtered709[,c("meanEdge","changePoint")]
clusters<-hclust(dist(filtered709[,c("meanEdge","fractalDim","changePoint")]))
clusterCut<-cutree(clusters,5)
ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
clusters<-hclust(dist(filtered709[,c("meanEdge","fractalDim","changePoint", "smoothness")]))
clusterCut<-cutree(clusters,4)
ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
clusterCut
filtered709[clusterCut==2,]
clusterCut<-cutree(clusters,5)
ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
hist(filtered709$changePoint)
filtered709[,changePoint>175]
filtered709[changePoint>175]
filtered709[changePoint>180]
test<-filtered709[changePoint>180]
test
head(test,30)
test<-filtered709[changePoint>195]
test<-filtered709[changePoint>190]
test<-filtered709[changePoint>192]
test<-filtered709[changePoint>193]
test<-filtered709[changePoint>193]
test<-filtered709[changePoint>194]
test<-filtered709[changePoint>193]
View(test)
View(test)
test<-filtered709[changePoint>175 & changePoint<190]
View(test)
hist(filtered709$fractalDim)
test<-filtered709[fractalDim<2.2]
View(test)
test<-filtered709[fractalDim<2.3 & fractalDim>2.2]
View(test)





