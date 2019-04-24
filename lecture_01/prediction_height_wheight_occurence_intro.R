###############################################
#### prediction: height, wheight, occurence ####
###############################################

hw <- read.csv("C:\\02_Studium\\02_Master\\02_Semester_2\\MET1_Spatial_modelling_and_prediction\\lecture_01\\weight-height.csv", sep = ",", header=T)
head(hw)
summary(hw)

#---------------------------
#change measurement
#install.packages("measurements")
library(measurements)

hw2 <- data.frame(Gender=hw$Gender, Weight=conv_unit(hw$Weight,"lbs", "kg"), Height=conv_unit(hw$Height, "inch", "cm"))
head(hw2)
summary(hw2)

plot(hw2$Height, hw2$Weight)

#---------------------------
#data exploration

library(dplyr)
#install.packages("glue")
library(glue)

#provide 10 random samples
dplyr::sample_n(hw2, 10)

#how are the values for male and female?
summary(filter(hw2, Gender=="Female"))
summary(filter(hw2, Gender=="Male"))

#boxplot
boxplot(filter(hw2, Gender=="Female")$Weight, filter(hw2, Gender=="Male")$Weight, notch=T)
boxplot(filter(hw2, Gender=="Female")$Heigth, filter(hw2, Gender=="Male")$Height, notch=T)

#-----------------------------
#statistics
shapiro.test(dplyr::sample_n(hw2, 5000)$Weight)
shapiro.test(dplyr::sample_n(hw2, 5000)$Height)

#plot
plot(density(hw2$Weight))
plot(density(hw2$Height))

#density by gender
plot(density(filter(hw2, Gender=="Female")$Weight), col="pink")
lines(density(filter(hw2, Gender=="Male")$Weight), col="blue")

plot(density(filter(hw2, Gender=="Female")$Height), col="pink")
lines(density(filter(hw2, Gender=="Male")$Height), col="blue")

shapiro.test(dplyr::sample_n(hw2, Gender=="Female"))#####fehlt

#only male data
hw2.male <- filter(hw2, Gender=="Male")
summary(hw2.male)

#regression of male data
hw.lm <- lm(formula = Weight~Height, data=hw2.male)
summary(hw.lm)


#--------------------------
#prediction male
hw.new <- data.frame(name=c("Patrick", "Mike", "Sandro", "Basil", "Fred"),
                     Height=c(xxx, 177, 181, 165, 180))



#############################################################
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
install.packages("sdm")
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

#-------------------------
d <- sdmData()  #fehlt! xxxxxxx

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

plot(p.time, 1) #plot first layer

#plot sdm for every time in one plot with building outlines
plotRGB(p.time, 1,3,5, stretch="lin")
plot(bui, add=T)


