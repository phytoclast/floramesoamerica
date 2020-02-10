library(raster)
library(sf)
library(fasterize)
library(sp)
library(ranger)
library(terra)
#process spatial data points to raster?

path = 'C:/workspace2/treeatlas/nam5k/'
Tw <- raster(paste0(path, 'Tw.tif'))
# make simplefeatures spatial dataframe ----------------------------------------------------------

fips_sf <- st_read('C:/a/geo/base/americas4.shp')
fips_sfna <- subset(fips_sf, !GROUP %in% c('South America', 'Bermuda','Hawaii' ))
states<-st_read("C:/workspace2/modelmap/data/states.shp")
fips_sfna$ID <- as.numeric(fips_sfna$ID)

fip_sfna1 <- st_transform(fips_sfna, crs(Tw))
narast <- fasterize(fip_sfna1, Tw, field = 'ID', fun = "last")
if(F){ #create shoreline raster
landvect <- st_read('C:/a/geo/base/americas4.shp')
landvect <- st_transform(landvect, crs(Tw))
landgrid <- fasterize(landvect, Tw, field = 'ID', fun = "last")
nainv <- is.na(landgrid) 
nainv[nainv == 0]<- NA
plot(nainv)
distgrid <- distance(nainv)
distgrid2 <- ((distgrid >= 15000)*15000 + (distgrid < 15000)*distgrid)/15000*-1+1
png(filename=paste0('output/', 'shore','.png'),width = 5000, height = 5000, units = 'px', pointsize = 10)
plot(distgrid2)
dev.off()
writeRaster(distgrid2, paste0(path, 'shore.tif'), overwrite=T)
}

#----
#make raster brick
#make st points from lat lon distrib then reproject and convert to raster the same as brick
#make distance from distrib points raster add to brick
#convert brick raster to points
filename <- c("effort.tif","water100k.tif", "shore.tif", "A.tif", "bedrock.tif", "clay.tif", "Deficit.tif",
              "gdem.tif", "hydric.tif", "M.tif", "MAP.tif", "pAET.tif", 
              "salids.tif", "sand.tif", "sealevel.tif", "slope.tif", "SoilpH.tif", "Surplus.tif", 
              "Tc.tif", "Tcl.tif", "Tclx.tif", "Tgs.tif", "tgsmin.tif", "Tw.tif", "Twh.tif")
effort <- raster(paste0(path, 'effort.tif'))
shore <- raster(paste0(path, 'shore.tif'))
water100k <- raster(paste0(path, 'water100k.tif'))
A <- raster(paste0(path, 'A.tif'))
bedrock <- raster(paste0(path, 'bedrock.tif'))
clay <- raster(paste0(path, 'clay.tif'))
Deficit <- raster(paste0(path, 'Deficit.tif'))
gdem <- raster(paste0(path, 'gdem.tif'))
hydric <- raster(paste0(path, 'hydric.tif'))
M <- raster(paste0(path, 'M.tif'))
MAP <- raster(paste0(path, 'MAP.tif'))
pAET <- raster(paste0(path, 'pAET.tif'))
salids <- raster(paste0(path, 'salids.tif'))
sand <- raster(paste0(path, 'sand.tif'))
sealevel <- raster(paste0(path, 'sealevel.tif'))
slope <- raster(paste0(path, 'slope.tif'))
SoilpH <- raster(paste0(path, 'SoilpH.tif'))
Surplus <- raster(paste0(path, 'Surplus.tif'))
Tc <- raster(paste0(path, 'Tc.tif'))
Tcl <- raster(paste0(path, 'Tcl.tif'))
Tclx <- raster(paste0(path, 'Tclx.tif'))
Tgs <- raster(paste0(path, 'Tgs.tif'))
tgsmin <- raster(paste0(path, 'tgsmin.tif'))
Tw <- raster(paste0(path, 'Tw.tif'))
Twh <- raster(paste0(path, 'Twh.tif'))


rastbrick <- brick(effort, water100k, shore, A, bedrock, clay, Deficit,
              gdem, hydric, M, MAP, pAET, 
              salids, sand, sealevel, slope, SoilpH, Surplus, 
              Tc, Tcl, Tclx, Tgs, tgsmin, Tw, Twh)

taxon <- "Picea mariana"

library(rgbif)
maxdist <- 500000 
mindist <- 200000
middist <- (maxdist+mindist)/2
lat0 <- 5
lon0 <- -180
lat1 <- 85
lon1 <- -25
lat <- paste0(as.character(lat0), ",",as.character(lat1))
lon <- paste0(as.character(lon0), ",",as.character(lon1))
biogeopts <- occ_search(limit=25000,
                         phylumKey = 7707728, scientificName = taxon, 
                         decimalLatitude=lat, decimalLongitude =lon)$
  data[,c('scientificName', 'decimalLatitude', 'decimalLongitude')]


sfpoints <- st_as_sf(x = biogeopts, 
                     coords = c('decimalLongitude', 'decimalLatitude'),
                     crs = "+proj=longlat +datum=WGS84")

sfpoints2 <- st_transform(sfpoints, crs(Tw))

sfraster <- rasterize(sfpoints2, Tw, field = 1, fun='count')
dist <- distance(sfraster)
plot(dist < maxdist & dist > mindist)
plot(st_geometry(states),  lwd=0.1, fill=F, add=T)

rastbrick <- brick(effort, water100k, shore, A, bedrock, clay, Deficit,
                   gdem, hydric, M, MAP, pAET, 
                   salids, sand, sealevel, slope, SoilpH, Surplus, 
                   Tc, Tcl, Tclx, Tgs, tgsmin, Tw, Twh, dist)




rbrkfrm <- as.data.frame(rasterToPoints(rastbrick))
rbrkfrm <- subset(rbrkfrm, !is.na(Tc) & !is.na(slope)  & !is.na(SoilpH) & !is.na(M) & !is.na(sand) & !is.na(clay))
summary(rbrkfrm$effort)
plot(dist<500000 & dist>=50000 & dist*effort >= 200000000)
plot(st_geometry(states),  lwd=0.1, fill=F, add=T)

present <- subset(rbrkfrm, layer == 0)
present$present <- 1
absent <- subset(rbrkfrm, layer <= 1000000 & layer >= 50000 & layer*effort >= 200000000)
absent$present <- 0
#weighting sceam doesn't work for randforest data where false absence locations all have unique combinations not found in the few occurence dots.
#wtfactor <- sum(absent$effort, na.rm = T)/sum(1/present$effort, na.rm = T)
#absent$wt <- absent$effort
#present$wt <- wtfactor/present$effort
present <- rbind(present, absent)

library(randomForest)

rf <- randomForest(present ~ shore+ gdem+ sealevel+ water100k+ #x+ y+
                     A+ bedrock+ clay+ Deficit+
                   hydric+ M+ MAP+ pAET+
                   salids+ sand+ slope+ SoilpH+ Surplus+
                   Tc+ Tcl+ Tclx+ Tgs+ tgsmin+ Tw+ Twh,
                   data=present, importance=TRUE, ntree=10, maxnodes = 500, na.action=na.omit)
# Make plot  other params to try: maxnodes=64,mtry=10,
rf#statistical summary
varImpPlot(rf)
if(F){
rf <- glm(present ~ water100k+
                     x+ y+ A+ bedrock+ clay+ Deficit+
                     gdem+ hydric+ M+ MAP+ pAET+
                     salids+ sand+ sealevel+ slope+ SoilpH+ Surplus+
                     Tc+ Tcl+ Tclx+ Tgs+ tgsmin+ Tw+ Twh,
                   data=present, na.action=na.omit)
}
rbrkfrm$output <- predict(rf, rbrkfrm, progress="window")
rbrkfrm$binary <- as.numeric(ifelse(rbrkfrm$output >= 0.5 & rbrkfrm$layer < 500000, 1, 0))
sfpredict <- st_as_sf(x = rbrkfrm, 
                      coords = c('x', 'y'),
                      crs = crs(Tw))


#sppredict <- as_Spatial(sfpredict)
#Twlowres <- aggregate(Tw, fact=2, fun=mean)


#plot(Twlowres)
#res(Twlowres)
sfpositive <- subset(sfpredict, binary==1)
predictraster <- rasterize(sfpositive, Tw, field = 'binary', fun='last')

plot(predictraster, col='red', legend=F)
plot(st_geometry(states),  lwd=0.1, fill=F, border = 'black', add=T)
plot(st_geometry(sfpoints2), pch=20, cex=0.5, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.7), add=T)

png(filename=paste0('output/', taxon,'.png'),width = 1500, height = 1500, units = 'px', pointsize = 10)
plot(predictraster, col='red', legend=F)
plot(st_geometry(states),  lwd=0.1, fill=F, border = 'black', add=T)
plot(st_geometry(sfpoints2), pch=20, cex=0.5, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.4), add=T)
dev.off()



#how to make a sf polygon
p1 <- rbind(c(-180,-20), c(-140,56), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p1 <- list(p1, hole)
p2 <- list(rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0)))
p3 <- list(rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0)))
pols <- st_sf(value = c(1,2,3),
              geometry = st_sfc(lapply(list(p1, p2, p3), st_polygon)))
r <- raster(pols, res = 1)
r <- fasterize(pols, r, field = "value", fun="sum")

