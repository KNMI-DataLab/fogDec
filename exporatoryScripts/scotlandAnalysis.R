scotland709<-readRDS("results/ResultFeatures7-709.rds")


filtered709<-scotland709[complete.cases(scotland709),]



filtered709[fractalDim<=2.2]
#image seems out of focus

filtered709.2<-filtered709[fractalDim>2.2]


#use of fractal dimention to recognize the change of scene 


filtered709.3<-filtered709.2[meanEdge>=0.010]
#removed images that are fault system scottish service



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





#there is fog at 8.10-20; 9.10-20-30; 10.40; 11.40
foggy<-filtered709.3[as.Date(dateTime)=="2016-11-21" & hour(dateTime)<12 & lubridate::minute(dateTime)<30,]


#looking at the bivariate plots it is difficult to say which features matter probably meanEdge but 



ggplot(filtered709.3, aes(x = meanEdge, meanBrightness))
+geom_point(color="black")
+geom_point(data=filtered709.3[as.Date(dateTime)=="2016-11-21" & ((hour(dateTime)==9 & lubridate::minute(dateTime)==10) | (hour(dateTime)==9 & lubridate::minute(dateTime)==20) | (hour(dateTime)==9 & lubridate::minute(dateTime)==30)| (hour(dateTime)==8 & lubridate::minute(dateTime)==10)| (hour(dateTime)==8 & lubridate::minute(dateTime)==20) | (hour(dateTime)==10 & lubridate::minute(dateTime)==40)),], aes(x = meanEdge, meanBrightness), colour="red",size =3)


foggyDay = filtered709.3[as.Date(dateTime)=="2016-11-21" & ((hour(dateTime)==9 & lubridate::minute(dateTime)==10) | (hour(dateTime)==9 & lubridate::minute(dateTime)==20) | (hour(dateTime)==9 & lubridate::minute(dateTime)==30)| (hour(dateTime)==8 & lubridate::minute(dateTime)==10)| (hour(dateTime)==8 & lubridate::minute(dateTime)==20) | (hour(dateTime)==10 & lubridate::minute(dateTime)==40)),]



foggyDay2 = filtered709.3[as.Date(dateTime)=="2016-11-21" & ((hour(dateTime)>=8 & (hour(dateTime)<=11))),] 


ggplot(filtered709.3, aes(x = meanEdge, fractalDim))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanEdge, fractalDim, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)



#given the visual inspection they are similar in terms of features but only 21-11 is foggy
filtered709.3[meanEdge>0.014 & meanEdge<0.0152 & fractalDim>2.37 & fractalDim<2.38,]


ggplot(filtered709.3, aes(x = meanEdge, smoothness))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanEdge, smoothness, label=filePath), colour="red",size =3)#+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)

ggplot(filtered709.3, aes(x = meanEdge, meanHue))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanEdge, meanHue, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)

ggplot(filtered709.3, aes(x = meanEdge, changePoint))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanEdge, changePoint, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)


#given the visual inspection they are similar in features but sunny day but different zoom in scenery on 21-11 is foggy
filtered709.3[meanEdge<=0.015 & changePoint<80 &changePoint>70,]

ggplot(filtered709.3, aes(x = meanEdge, meanSaturation))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanEdge, meanSaturation, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)


#given the visual inspection on 2016-12-07/08 seems misty
filtered709.3[meanEdge<=0.013 & meanSaturation<0.15 & meanSaturation>0.1,]



ggplot(filtered709.3, aes(x = meanEdge, meanBrightness))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanEdge, meanBrightness, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)


ggplot(filtered709.3, aes(x = changePoint, fractalDim))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = changePoint, fractalDim, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)


ggplot(filtered709.3, aes(x = changePoint, meanSaturation))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = changePoint, meanSaturation, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)

ggplot(filtered709.3, aes(x = changePoint, meanHue))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = changePoint, meanHue, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)


#none of the imagaes with the similar features to the one of 21-11 has fog
filtered709.3[meanHue<=125 & meanHue>100 & changePoint<100 & changePoint>70,]


ggplot(filtered709.3, aes(x = changePoint, meanBrightness))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = changePoint, meanBrightness, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)

ggplot(filtered709.3, aes(x = meanSaturation, meanBrightness))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanSaturation, meanBrightness, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)


ggplot(filtered709.3[smoothness<2.5,], aes(x = meanSaturation, smoothness))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanSaturation, smoothness, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)


#some haziness conditions are identified, better than other indicators
filtered709.3[meanSaturation<=0.15 & meanSaturation<1 & smoothness<1,]


ggplot(filtered709.3, aes(x = meanSaturation, meanHue))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = meanSaturation, meanHue, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)

filtered709.3[meanSaturation<=0.1 & meanSaturation<1 & meanHue<150,]

ggplot(filtered709.3[smoothness<2.5], aes(x = fractalDim, smoothness))+geom_point(color="black")+geom_point(data=foggyDay2, aes(x = fractalDim, smoothness, label=filePath), colour="red",size =3)+geom_text(data = foggyDay2, aes(label= dateTime, colour = "red"),check_overlap = F, size =3)


#not really useful
filtered709.3[fractalDim<=2.4 & smoothness<.8 & smoothness>.7,]




#looking if any dimentions can be removed, PCA analysis
valuesNumbers<-filtered709.3[,12:18]


library(caret)
PCAtransf<-preProcess(valuesNumbers,method=c("BoxCox", "center", "scale", "pca"))

PCAtransf

# 
# 
# clusters<-hclust(dist(filtered709[,meanEdge,fractalDim]))
# clusterCut<-cutree(clusters,5)
# ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
# clusters<-hclust(dist(filtered709[,meanEdge,fractalDim,changePoint]))
# clusters<-hclust(dist(filtered709[,c(meanEdge,fractalDim,changePoint)]))
# clusterCut<-cutree(clusters,5)
# clusters<-hclust(dist(filtered709[,c(meanEdge,fractalDim,changePoint)]))
# clusterCut<-cutree(clusters,5)
# ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
# filtered709[,c(meanEdge,meanHue)]
# filtered709[,c(meanEdge,changePoint)]
# filtered709[,c("meanEdge","changePoint")]
# clusters<-hclust(dist(filtered709[,c("meanEdge","fractalDim","changePoint")]))
# clusterCut<-cutree(clusters,5)
# ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
# clusters<-hclust(dist(filtered709[,c("meanEdge","fractalDim","changePoint", "smoothness")]))
# clusterCut<-cutree(clusters,4)
# ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
# clusterCut
# filtered709[clusterCut==2,]
# clusterCut<-cutree(clusters,5)
# ggplot(filtered709,aes(meanEdge,fractalDim))+geom_point(col = clusterCut)
# hist(filtered709$changePoint)
# filtered709[,changePoint>175]
# filtered709[changePoint>175]
# filtered709[changePoint>180]
# test<-filtered709[changePoint>180]
# test
# head(test,30)
# test<-filtered709[changePoint>195]
# test<-filtered709[changePoint>190]
# test<-filtered709[changePoint>192]
# test<-filtered709[changePoint>193]
# test<-filtered709[changePoint>193]
# test<-filtered709[changePoint>194]
# test<-filtered709[changePoint>193]
# View(test)
# View(test)
# test<-filtered709[changePoint>175 & changePoint<190]
# View(test)
# hist(filtered709$fractalDim)
# test<-filtered709[fractalDim<2.2]
# View(test)
# test<-filtered709[fractalDim<2.3 & fractalDim>2.2]
# View(test)


#######################################################################

scotland715<-readRDS("results/ResultFeatures7-715.rds")

filtered715<-scotland715[meanEdge>0.008,] # remove the frozen dark images of the night error of the system


filtered715[meanEdge<0.011,] # the images have light in the camera

# /nobackup/users/pagani/scotland/2016_10/7-715/7-715_20161027_1610.jpg      7-715 2016-10-27 16:10:00
# 2: /nobackup/users/pagani/scotland/2016_12/7-715/7-715_20161217_1420.jpg      7-715 2016-12-17 14:20:00
# 3: /nobackup/users/pagani/scotland/2016_12/7-715/7-715_20161217_1500.jpg      7-715 2016-12-17 15:00:00
# 4: /nobackup/users/pagani/scotland/2016_12/7-715/7-715_20161217_1510.jpg      7-715 2016-12-17 15:10:00

filtered715[meanHue<75] # these pictures are in Black and White :-(

filtered715.1<-filtered715[meanHue>=75]

