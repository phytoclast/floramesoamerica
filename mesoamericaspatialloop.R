library(raster)
library(sf)
library(fasterize)
library(sp)
library(ranger)
library(terra)
library(rgbif)
library(randomForest)
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

#search parameters for rgbif
maxdist <- 500000 
mindist <- 200000
middist <- (maxdist+mindist)/2
lat0 <- 5
lon0 <- -180
lat1 <- 85
lon1 <- -25
lat <- paste0(as.character(lat0), ",",as.character(lat1))
lon <- paste0(as.character(lon0), ",",as.character(lon1))

taxonlist <- c("Arctostaphylos parryana",
               "Larrea tridentata",
               "Tillandsia usneoides",
               "Magnolia virginiana",
               "Populus tremuloides",
               "Abies balsamea",
               "Picea rubens",
               "Prunus serotina",
               "Liriodendron tulipifera",
               "Liquidambar styraciflua",
               "Pseudotsuga menziesii",
               "Pinus ponderosa",
               "Taxodium",
               "Sequoia sempervirens",
               "Tillandsia usneoides",
               "Ceiba pentandra",
               "Acer saccharum",
               "Picea sitchensis",
               "Pinus palustris",
               "Cyrilla",
               "Cakile edentula",
               "Rhizophora mangle",
               "Prosopis glandulosa",
               "Populus tremuloides",
               "Fagus",
               "Annona glabra",
               "Populus tremuloides",
               "Yucca guatemalensis",
               "Enterolobium cyclocarpum",
               "Vachellia farnesiana",
               "Manilkara bidentata",
               "Quercus costaricensis",
               "Pinus ayacahuite",
               "Podocarpus oleifolius",
               "Salix fuscescens",
               "Pinus caribaea",
               "Cypripedium acaule",
               "Cypripedium reginae",
               "Andropogon gerardii",
               "Artemisia tridentata",
               "Nymphaea odorata",
               "Andropogon glomeratus",
               "Cirsium wheeleri",
               "Prosartes",
               "Prosartes trachycarpa",
               "Prosartes lanuginosa",
               "Magnolia tripetala",
               "Magnolia",
               "Platanus",
               "Podocarpus",
               "Taxus",
               "Loiseleuria procumbens",
               "Rhexia virginica",
               "Rhexia mariana",
               "Arctostaphylos alpina",
               "Dryas integrifolia",
               "Dryas punctata",
               "Picea glauca",
               "Picea mariana",
               "Loiseleuria procumbens")
for (i in 1:3){
taxon <- taxonlist[i]

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

rastbrick <- brick(effort, water100k, shore, A, bedrock, clay, Deficit,
                   gdem, hydric, M, MAP, pAET, 
                   salids, sand, sealevel, slope, SoilpH, Surplus, 
                   Tc, Tcl, Tclx, Tgs, tgsmin, Tw, Twh, dist)

rbrkfrm <- as.data.frame(rasterToPoints(rastbrick))
rbrkfrm <- subset(rbrkfrm, !is.na(sealevel)&  !is.na(salids) & !is.na(tgsmin) & !is.na(Tc) & !is.na(slope)  & !is.na(SoilpH) & !is.na(M) & !is.na(sand) & !is.na(clay))

grid1 <- (50000/((effort/200)^0.5*dist+5000))^2
grid2 <- dist>100000/(effort/250)^0.5+50000

#----
#thermal contraints
QTclx0 <- quantile(rbrkfrm[rbrkfrm$layer == 0,]$Tclx, 0.01)
QTclx1 <- quantile(rbrkfrm[rbrkfrm$layer == 0,]$Tclx, 0.99)
QTc0 <- quantile(rbrkfrm[rbrkfrm$layer == 0,]$Tc, 0.01)
QTc1 <- quantile(rbrkfrm[rbrkfrm$layer == 0,]$Tc, 0.99)
QTg0 <- quantile(rbrkfrm[rbrkfrm$layer == 0,]$Tgs, 0.01)
QTg1 <- quantile(rbrkfrm[rbrkfrm$layer == 0,]$Tgs, 0.99)



#----
present <- subset(rbrkfrm, layer <= 500000)
present[is.na(present$effort),]$effort <- 100
present$observed <- 1
present$wt <- (5000/((present$effort/200)^0.5*present$layer+5000))^2

absent <- present
present <- subset(present, wt >= 0.04 | (Tclx >= QTclx0 &
                                           Tclx <= QTclx1 &
                                           Tc >= QTc0 &
                                           Tc <= QTc1 &
                                           Tgs >= QTg0 &
                                           Tgs <= QTg1))
absent$observed <- 0
absent$wt <- 1
absent <- subset(absent, layer>100000/(effort/250)^0.5+50000 | (Tclx < QTclx0 &
                                                                  Tclx > QTclx1 &
                                                                  Tc < QTc0 &
                                                                  Tc > QTc1 &
                                                                  Tgs < QTg0 &
                                                                  Tgs > QTg1))
present <- rbind(present, absent)
present <- subset(present, wt >= 0.0001)

rf <- ranger(observed ~ shore+ gdem+ sealevel+ water100k+ #x+ y+
                     A+ bedrock+ clay+ Deficit+
                     hydric+ M+ MAP+ pAET+
                     salids+ sand+ slope+ SoilpH+ Surplus+
                     Tc+ Tcl+ Tclx+ Tgs+ tgsmin+ Tw+ Twh,
                   data=present, num.trees=25, max.depth = 200, case.weights = present$wt, importance = 'impurity', write.forest = TRUE)

rbrkfrm$output <- predictions(predict(rf, data=rbrkfrm))

maxmodel <- max(rbrkfrm[rbrkfrm$layer < 500000,]$output)
maxmodel <- ifelse(maxmodel>1,1,maxmodel)
rbrkfrm$binary <- as.numeric(ifelse(rbrkfrm$output/maxmodel >= 0.5 & rbrkfrm$layer < 500000, 1, 0))
sfpredict <- st_as_sf(x = rbrkfrm, 
                      coords = c('x', 'y'),
                      crs = crs(Tw))


sfpositive <- subset(sfpredict, binary==1)
predictraster <- rasterize(sfpositive, Tw, field = 'binary', fun='last')

png(filename=paste0('output2/', taxon,'.png'),width = 1500, height = 1500, units = 'px', pointsize = 10)
plot(predictraster, col = rgb(red = 0, green = 0.85, blue = 0, alpha = 1), legend=F)
plot(st_geometry(states),  lwd=0.1, fill=F, border = 'black', add=T)
plot(st_geometry(sfpoints2), pch=20, cex=0.5, col = rgb(red = 0.85, green = 0, blue = 0, alpha = 1), add=T)
dev.off()
}
