#run through lat lon to count records
library(rgbif)
lltab<- as.data.frame(cbind(lat0=c(0),lon0=c(0),lat1=c(0),lon1=c(0),ct=c(0)))
latmin <- 5
latmax <- 85
lonmin <- -180
lonmax <- -20
latincmax <- 80
lonincmax <- 160
for (latinc in 1:latincmax){
  for (loninc in 1:lonincmax){
    lat0 <- (latmax-latmin)/latincmax*(latinc-1) + latmin
    lon0 <- (lonmax-lonmin)/lonincmax*(loninc-1) + lonmin
    lat1 <- lat0 + (latmax-latmin)/latincmax
    lon1 <- lon0 + (lonmax-lonmin)/lonincmax
    lat <- paste0(as.character(lat0), ",",as.character(lat1))
    lon <- paste0(as.character(lon0), ",",as.character(lon1))
    ct <- occ_search(limit=0,
                     phylumKey = 7707728,
                     decimalLatitude=lat, decimalLongitude =lon)$meta$count
    
    lltab1 <- as.data.frame(cbind(lat0,lon0,lat1, lon1, ct))
    lltab <- rbind(lltab, lltab1)
  }}
lltab <-lltab[-1,]
lltab$lat <- (lltab$lat0 + lltab$lat1)/2 
lltab$lon <- (lltab$lon0 + lltab$lon1)/2 
lltab$area <- cos((lltab$lat0+lltab$lat1)/360*pi) * (lltab$lon1 - lltab$lon0)*(lltab$lat1 - lltab$lat0)*(10000/90)^2
lltab$density <- round(lltab$ct/lltab$area*10000, 1)
write.csv(lltab, 'output/lltab.csv')
#lltab <- read.csv('output/lltab.csv')
#----
#rasterize the gbif dots
library(raster)
library(sf)
library(fasterize)
library(sp)
dotsf <- st_as_sf(x = lltab, 
                              coords = c('lon', 'lat'),
                              crs = "+proj=longlat +datum=WGS84")
dotraster <- raster(dotsf, nrow = length(unique(lltab$lat)), ncol = length(unique(lltab$lon)))
dotraster <- rasterize(dotsf, dotraster, field='density')
plot(dotraster)
#compensate for percent land area
landvect <- st_read('C:/a/geo/base/americas4.shp')
landvect <- subset(landvect, !COLOR %in% 'Blue or Delete')
landgrid <- disaggregate(dotraster, fact=10, )
landgrid <- fasterize(landvect, landgrid, field = 'ID', fun = "last")
plot(landgrid)
nainv <- !is.na(landgrid)
nainv <- aggregate(nainv, fact=10, FUN=mean)
plot(nainv)
res(landgrid)
res(nainv)
res(dotraster)
#reproject grids
path = 'C:/workspace2/treeatlas/nam5k/'
Tw <- raster(paste0(path, 'Tw.tif'))
landpercent <- projectRaster(nainv, Tw, method="bilinear")
dotrasterproj <- projectRaster(dotraster, Tw, method="bilinear")
compensateddots <- projectRaster((dotraster/(nainv+0.01)), Tw, method="bilinear")
writeRaster(compensateddots, paste0(path, 'effort.tif'), overwrite=T)
states<-st_read("C:/workspace2/modelmap/data/states.shp")
plot(log(compensateddots+100))
plot(st_geometry(states),  lwd=0.1, fill=F, border = 'black', add=T)

#----

row0<- c(17, 25, -165, -150)
row1<- c(49, 85, -185, -25)
row2 <- c(37, 49, -130, -110)
row3 <- c(37, 49, -110, -80)
row4 <- c(37, 49, -80, -50)
row5 <- c(25, 37, -105, -70)
row6 <- c(25, 37, -110, -105)
row7 <- c(13, 25, -115, -95)
row8 <- c(13, 25, -95, -55)
row9 <- c(5, 13, -90, -60)
coordtable <- rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9)
