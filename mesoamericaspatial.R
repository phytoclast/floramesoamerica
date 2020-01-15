library(raster)
library(fasterize)
library(sf)
#process spatial data points to raster?

path = 'C:/workspace2/treeatlas/nam5k/'
Tw <- raster(paste0(path, 'Tw.tif'))
res(Tw)
crs(Tw)
# make simplefeatures spatial dataframe ----------------------------------------------------------

fips_sf <- st_read('C:/a/geo/base/americas4.shp')
fips_sfna <- subset(fips_sf, !GROUP %in% c('South America', 'Bermuda','Hawaii' ))
fips_sfna$ID <- as.numeric(fips_sfna$ID)

fip_sfna1 <- st_transform(fips_sfna, crs(Tw))
narast <- fasterize(fip_sfna1, Tw, field = 'ID', fun = "last")
plot(narast)
points1 <- as.data.frame(rasterToPoints(Tw))
points2 <- as.data.frame(rasterToPoints(narast))
points2 <- merge(points2, st_drop_geometry(fips_sfna[,c('ID','GROUP','NATION','STATE','STATECODE','FIPS')]), by.x='layer',by.y='ID', all.x = TRUE)
points3 <- merge(points1, points2, by=c('x','y'))
#preproject
fips_sfnarepro <- st_transform(fips_sfna, st_crs(Tw))
plot(fips_sfnarepro[1])
namap <- st_rasterize(fips_sfnarepro, Tw, field="GROUP", fun='last', background=NA)
ptsfit<-st_rasterize(ptsALL,Tw,field="best", fun='last', background=NA)

#----
library(rgbif)
lat0 <- 5
lon0 <- -180
lat1 <- 85
lon1 <- -25
lat <- paste0(as.character(lat0), ",",as.character(lat1))
lon <- paste0(as.character(lon0), ",",as.character(lon1))
biogeopts <- occ_search(limit=1000,
                 phylumKey = 7707728, scientificName = "Picea mariana", 
                 decimalLatitude=lat, decimalLongitude =lon)$
  data[,c('decimalLatitude', 'decimalLongitude')]

biogeopts2 <- occ_search(limit=1000,
                        phylumKey = 7707728, scientificName = "Pinus strobus", 
                        decimalLatitude=lat, decimalLongitude =lon)$
  data[,c('decimalLatitude', 'decimalLongitude')]


biogeopts3 <- rbind(biogeopts2, biogeopts)
plot(biogeopts3$decimalLongitude, biogeopts3$decimalLatitude)


p1 <- rbind(c(-180,-20), c(-140,56), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p1 <- list(p1, hole)
p2 <- list(rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0)))
p3 <- list(rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0)))
pols <- st_sf(value = c(1,2,3),
              geometry = st_sfc(lapply(list(p1, p2, p3), st_polygon)))
r <- raster(pols, res = 1)
r <- fasterize(pols, r, field = "value", fun="sum")
plot(r)
