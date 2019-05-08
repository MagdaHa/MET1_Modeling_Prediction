#####################################
#### prediction: height, wheight ####
#####################################

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


