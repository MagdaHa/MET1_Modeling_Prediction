#####################################
#### prediction: EAGLE occurence ####
#####################################

library(rgdal)
library(raster)

#occurance data
occ <- readOGR("C:\\02_Studium\\02_Master\\02_Semester_2\\MET1_Spatial_modelling_and_prediction\\lecture_01\\occurence.gpkg")

class(occ)
summary(occ)
plot(occ)

#building data
bui <- readOGR("C:\\02_Studium\\02_Master\\02_Semester_2\\MET1_Spatial_modelling_and_prediction\\lecture_01\\campus_buildings.gpkg")
class(bui)
summary(bui)
plot(bui)
plot(occ[occ$students==1,], col="red", pch=16, add=T)

#--------------------------
#rasterized campus buildings
r <- raster(bui, ncols=100, nrows=100)

rr.0 <- rasterize(bui, r, progress="text")
plot(rr.0)

#distance campus buildings
rr.0.d <- distance(rr.0)
plot(rr.0.d)

#-----------------------
#install.packages("sdm")
library(sdm)

preds <- rr.0.d
d <- sdmData(formula=students~layer, train=occ, predictors = preds)
d

#random forest and support vector machine prediction
m1 <- sdm(students~., data = d, methods = c("glm", "svm"))
p1 <- predict(m1,newdata=preds)
plot(p1)

#-----------------------
#prediction of different areas
rr <- rasterize(bui, r, progress="text", field="id")
plot(rr)

rr.1 <- rr==1
rr.1[rr.1==0] <- NA
plot(rr.1)
rr.2 <- rr==2
rr.2[rr.2==0] <- NA
plot(rr.2)
rr.3 <- rr==3
rr.3[rr.3==0] <- NA

rr.1.d <- distance(rr.1)
plot(rr.1.d)

rr.2.d <- distance(rr.2)
plot(rr.2.d)

rr.3.d <- distance(rr.3)
plot(rr.3.d)

preds <- stack(rr.1.d, rr.2.d, rr.3.d)
plot(preds)

#------------------------
#ocurence data and time stamp combined
occ.10h <- occ[occ$time==10,]
occ.13h <- occ[occ$time==13,]
occ.22h <- occ[occ$time==22,]

#creating sdm data objects
d.10h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.10h, predictors=preds)
d.13h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.13h, predictors=preds)
d.22h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.22h, predictors=preds)

#evaluate sdm data
m.10h <- sdm(formula=students~., data=d.10h, methods=c("glm", "svm"))
m.13h <- sdm(formula=students~., data=d.13h, methods=c("glm", "svm"))
m.22h <- sdm(formula=students~., data=d.22h, methods=c("glm", "svm"))

#predict sdm model
p.10h <- predict(m.10h, newdata=preds)
p.13h <- predict(m.13h, newdata=preds)
p.22h <- predict(m.22h, newdata=preds)

#plot models
p.time <- stack(p.10h, p.13h, p.22h)
plot(p.time)

plot(p.time, 1) #plot only first layer

#plot sdm for every time in one plot with building outlines
plotRGB(p.time, 1,3,5, stretch="lin")
plot(bui, add=T)