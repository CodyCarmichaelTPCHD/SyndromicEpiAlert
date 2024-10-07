##Spatial Analysis Data Read In
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(sf)
library(leaflet)




#load in poly files for mapping with Leaflet
shapeData <- readOGR("Pierce_Map/Zip_Codes.shp")
shapeData <- spTransform(shapeData, CRS("+proj=longlat +datum=WGS84 +no_defs"))


save(shapeData, file = "FormattedMapBase.RData")