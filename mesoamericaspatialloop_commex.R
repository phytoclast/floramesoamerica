remotes::install_github("jannes-m/RQGIS3")
library(raster)
library(sf)
library(fasterize)
library(sp)
library(ranger)
library(terra)
library(rgbif)
library(randomForest)
library(gdalUtilities)
library("RQGIS3")
library(stringr)
#process spatial data points to raster?

path = 'C:/workspace2/treeatlas/nam5k/'
Tw <- raster(paste0(path, 'Tw.tif'))
# make simplefeatures spatial dataframe ----------------------------------------------------------
prebiogeopts <- readRDS('data/prebiogeopts.RDS')
fips_sf <- st_read('C:/a/geo/base/americas4.shp')
fips_sfna <- subset(fips_sf, !GROUP %in% c('South America', 'Bermuda' ))
states<-st_read("C:/workspace2/modelmap/data/states.shp")
fips_sfna$ID <- as.numeric(fips_sfna$ID)
BONAPFips <- readRDS('data/BONAPFips2015.RDS')
BONAPFips$FIPS2 <- as.character(BONAPFips$FIPS)
BONAPFips[nchar(BONAPFips$FIPS2)==4,]$FIPS2 <- paste0('0', BONAPFips[nchar(BONAPFips$FIPS2)==4,]$FIPS2 )
BONAPFips$FIPS <- BONAPFips$FIPS2
BONAPFips <- BONAPFips[,-7]
BONAPState <- readRDS('data/BONAPState2015.RDS')