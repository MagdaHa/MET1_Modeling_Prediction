###############################################
#### Caffea Arabica: Modelin and Prediction ###
###############################################

#' Species distribution models (SDM) using caffea arabica data from GBIF
#' ============================================
#' May 2019
#' Magdalena Halbgewachs
#' R version 3.5.1
#' Occurrence data source: [gbif](http://www.gbif.org)  
#' Environmental data source: [worldclim](http://www.worldclim.org)  
#' Algorithms: GAM, RF, MaxEnt
#' 
#' Overview
#' ===============================
#' - Set working directory and load packages
#' - Import data
#' - Data preprocessing
#'  * Presence records for which environmental data is available
#'  * Collinearity
#' - Modelling
#' - Model evaluation
#'  * Model performance on test data
#'  * Variable importance
#'  * Response functions
#' - Model predictions
#' 

#' Set working directory and load packages
#' ==========================================
#' Set the working directory. You have to use your own path.
setwd("C:\\02_Studium\\02_Master\\02_Semester_2\\MET1_Spatial_modelling_and_prediction\\MET1_Modeling_Prediction\\caffea_arabica")


#' Package rms provides function val.prob for evaluation of model performance; 
#' load before raster to avoid name conflicts with mask (alternatively: use raster::mask, 
#' i.e. explicitly specify that you want to use function mask from namespace raster)
#' 
#install.packages("rms")
library(rms) 
library(raster)
library(mgcv) # gam
#' We assume that the file "varImpBiomod.R" is in the working directory
source("varImpBiomod.R") 
library(randomForest)
library(dismo)
library(rgdal)
library(ellipse)
library(randomForest)
#install.packages("rJava")
library(rJava)
library(XML)

#' Import data: Environment and species occurrences
#' ====================================================
#' Read vector layer with study area
#' ----------------------------------
#' Notes:
#' - './' refers to the current working directory, i.e. we 
#' are specifying as the first argument of readOGR, the dsn, the subdirectory "GIS" within the current working directory
#' - make sure there is no trailing '/' in the value of the dsn argument, i.e. do **not** use "./GIS/"
#' - when importing shapefiles, drop the suffix from the layer argument, i.e. do **not** use "africa_dissolved.shp"
#' 

study_area <- readOGR("./GIS", "africa_dissolved")
plot(study_area)

#' Download and read bioclim variables
#' -----------------------------------
#' The function getData will load the data from the working directory, if available. If they are not available,
#' the function will attempt to download the data from the internet.
#' For a definition of the variables, see [http://www.worldclim.org/bioclim](http://www.worldclim.org/bioclim)
#' 
#' Variable | Description
#' -------- | -----------
#' BIO1 | Annual Mean Temperature
#' BIO2 | Mean Diurnal Range (Mean of monthly (max temp - min temp))
#' BIO3 | Isothermality (BIO2/BIO7) (* 100)
#' BIO4 | Temperature Seasonality (standard deviation *100)
#' BIO5 | Max Temperature of Warmest Month
#' BIO6 | Min Temperature of Coldest Month
#' BIO7 | Temperature Annual Range (BIO5-BIO6)
#' BIO8 | Mean Temperature of Wettest Quarter
#' BIO9 | Mean Temperature of Driest Quarter
#' BIO10 | Mean Temperature of Warmest Quarter
#' BIO11 | Mean Temperature of Coldest Quarter
#' BIO12 | Annual Precipitation
#' BIO13 | Precipitation of Wettest Month
#' BIO14 | Precipitation of Driest Month
#' BIO15 | Precipitation Seasonality (Coefficient of Variation)
#' BIO16 | Precipitation of Wettest Quarter
#' BIO17 | Precipitation of Driest Quarter
#' BIO18 | Precipitation of Warmest Quarter
#' BIO19 | Precipitation of Coldest Quarter
#' 

bio <- raster::getData("worldclim", var = "bio", res = 10)

#' Plot the first raster layer, i.e. annual mean temperature
plot(raster(bio, 1))
# Add the outline of the study area
plot(study_area, add=TRUE)

#' Crop to study area extent (with a 5 degree buffer in each direction)
biocrop <- crop(bio, extent(study_area) + 10)

#' Plot the first raster layer of the cropped climate data
plot(raster(biocrop, 1))
plot(study_area, add=TRUE)

#' Read occurrence points. If they exist locally, use the local file.
#' If they do not exist, download from [gbif](http://www.gbif.org) 
if (file.exists("./GIS/Synceruscaffer/Synceruscaffer.mif")) {
  species <- readOGR("./GIS/Synceruscaffer/Synceruscaffer.mif", layer = "Synceruscaffer")
} else {
  # Download species location data from gbif
  # species <- gbif("Syncerus", "caffer", ext = extent(bio), sp = TRUE, removeZeros = TRUE)
  species0 <- gbif('Syncerus', 'caffer')
  species <- subset(species0,select=c("lat","lon"))
  species <- na.omit(species)
  coordinates(species) <- c("lon", "lat")  # set spatial coordinates
  
  
  # Add projection information
  proj4string(species) <- CRS("+proj=longlat +datum=WGS84")
  # Save species records in mif-format (preserves full column names)
  #writeOGR(species, "./GIS/Synceruscaffer", "Synceruscaffer", driver="MapInfo File", dataset_options="FORMAT=MIF")
}

plot(raster(biocrop, 1))
plot(species, add = TRUE)

#' Data preprocessing
#' ==============================

#' Select species records for which environmental information is available
#' -------------------------------
species <- species[complete.cases(extract(biocrop, species)), ]

#' Collinearity
#' -----------------------------
#' ### Visual inspection of collinearity ###
cm <- cor(getValues(bio), use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

#' ### Select an uncorrelated subset of environmental variables ###
env <- subset(biocrop, c("bio1", "bio7", "bio10", "bio14", "bio15"))

#' Sampling of (pseudo-)absence points
#' ====================================================
#' The function randomPoints in package dismo allows to 
#' randomly select a certain number of random points,
#' and to adjust the probability of selecting a cell
#' according to its size, which is relevant in lat-lon-grids,
#' where cells are of differing size

#' Selecting 2000 random background points, excluding cells where
#' the species is present
set.seed(2)
background <- randomPoints(env, 2000, species)
#' Select only one presence record in each cell of the environmental layer
presence <- gridSample(species, env, n = 1)

#' 
#' Now we combine the presence and background points, adding a 
#' column "species" that contains the information about presence (1)
#' and background (0)
fulldata <- SpatialPointsDataFrame(rbind(presence, background),
                                   data = data.frame("species" = rep(c(1,0), 
                                                                     c(nrow(presence), nrow(background)))),
                                   match.ID = FALSE,
                                   proj4string = CRS(projection(env)))
#' Add information of environmental conditions at point locations
fulldata@data <- cbind(fulldata@data, extract(env, fulldata))

#' 
# Split data set into a training and test data set
set.seed(2)
fold <- kfold(fulldata, k = 5)
traindata <- fulldata[fold != 1, ]
testdata <- fulldata[fold == 1, ]

#' We can now use a range of statistical methods to estimate the
#' probability of species occurrence.
#' Unfortunately, there are often subtle differences in how the models
#' are specified and in which data formats are useable

varnames <- c("bio1", "bio7", "bio10", "bio14", "bio15")

## Generalized Linear Model

## Generalized additive models
gammodel <- gam(species ~ s(bio1) + s(bio7) + s(bio10) + s(bio14) + s(bio15),
                family="binomial", data=traindata)
summary(gammodel)

plot(gammodel)

# Now we should do model selection: bio14 does not contribute to the fit

# Evaluate model on test data
# a) Predict to test data
gamtest <- predict(gammodel, newdata = testdata, type = "response")
# b) Calculate performance indices
val.prob(gamtest, testdata[["species"]])

# Variable importance
gamimp <- varImpBiomod(gammodel, varnames,
                       traindata)
barplot(100 * gamimp/sum(gamimp), ylab = "Variable importance (%)")

# Response functions
plot(gammodel, pages = 1)

# png("gammodel_resp.png", 800, 800)
# plot(gammodel, pages = 1)
# dev.off()

# Prediction map
gammap <- predict(env, gammodel, type = "response")

plot(gammap)


## Random forest

# randomForest requires the dependent variable to be a factor
# if we want to do classification
rftraindata <- as(traindata, "data.frame")
rftraindata$species <- factor(rftraindata$species)

# TODO: check proper settings of random forest algorithm
rfmodel <- randomForest(species ~ bio1 + bio7 + bio10 + bio14 + bio15, data = rftraindata)

# Evaluate model on test data
# a) Predict to test data
rftest <- predict(rfmodel, newdata = testdata, type = "prob")[,2]
# b) Calculate performance indices
val.prob(rftest, testdata[["species"]])

# Variable importance
rfImp <- importance(rfmodel)
varImpPlot(rfmodel)

# Response functions
par(mfrow=c(3,2))
for (i in seq_along(varnames)) {
  partialPlot(rfmodel, rftraindata, varnames[i], xlab = varnames[i], main="")  
}

# Prediction map
rfmap <- predict(env, rfmodel, type = "prob", index = 2)
par(mfrow=c(1, 1))
plot(rfmap)

#-----------------------------------------------------------------------------------------
## Maxent
# The following code assumes that the column with the species information
# is in the first position
maxentmodel <- maxent(traindata@data[, -1], traindata[["species"]], 
                      args = c("nothreshold", 
                               "nohinge"))

# Model evaluation on test data
maxenttest <- predict(maxentmodel, testdata)
val.prob(maxenttest, testdata[["species"]])

# Alternatively, we can use the evaluate function
maxente <- evaluate(p = maxenttest[testdata[["species"]] == 1],
                    a = maxenttest[testdata[["species"]] == 0])

# Show variable importance
plot(maxentmodel)

# Plot response functions
response(maxentmodel)

# Prediction map
maxentmap <- predict(maxentmodel, env)
plot(maxentmap)

# Plot predictions of several methods, using the same
# colour scheme
par(mfrow = c(3, 1), mar = c(3, 3, 1, 1))
brks <- seq(0, 1, by = 0.1)
arg <- list(at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2))
col <- rev(terrain.colors(length(brks) - 1))
plot(gammap, breaks = brks, col = col, axis.args = arg)
plot(rfmap, breaks = brks, col = col, axis.args = arg)
plot(maxentmap, breaks = brks, col = col, axis.args = arg)

