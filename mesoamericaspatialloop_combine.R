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
#liststates <- unique(st_drop_geometry(subset(fips_sf, NATION %in% c('Denmark', 'USA', 'Canada', 'France'),select=c(STATE, NATION, STATECODE))))
#liststates$simplecode <- substr(liststates$STATECODE, nchar(as.character(liststates$STATECODE))-1, nchar(as.character(liststates$STATECODE)))
#saveRDS(liststates, 'data/liststates.RDS')

library(stringr)
BONAPState$Binomial <- paste(str_split_fixed(BONAPState$Scientific.Name, ' ', n=3)[,1],str_split_fixed(BONAPState$Scientific.Name, ' ', n=3)[,2])

BONAPState[substr(BONAPState$Scientific.Name, 1,2) %in% 'X ',]$Binomial <- 
  paste(str_split_fixed(BONAPState[substr(BONAPState$Scientific.Name, 1,2) %in% 'X ',]$Scientific.Name, ' ', n=4)[,1],
        str_split_fixed(BONAPState[substr(BONAPState$Scientific.Name, 1,2) %in% 'X ',]$Scientific.Name, ' ', n=4)[,2],
        str_split_fixed(BONAPState[substr(BONAPState$Scientific.Name, 1,2) %in% 'X ',]$Scientific.Name, ' ', n=4)[,3])
BONAPState[grepl(' X ', BONAPState$Scientific.Name),]$Binomial <- 
  paste(str_split_fixed(BONAPState[grepl(' X ', BONAPState$Scientific.Name),]$Scientific.Name, ' ', n=4)[,1],
        str_split_fixed(BONAPState[grepl(' X ', BONAPState$Scientific.Name),]$Scientific.Name, ' ', n=4)[,2],
        str_split_fixed(BONAPState[grepl(' X ', BONAPState$Scientific.Name),]$Scientific.Name, ' ', n=4)[,3])

BONAPState <- subset(BONAPState, !Nativity %in% '-')
DELETEBINOMIAL <- unique(subset(BONAPState, grepl('\\.', Scientific.Name), select='Binomial'))[,1]
STATENATIVITY <- unique(subset(BONAPState, !Scientific.Name %in% DELETEBINOMIAL, select=c('Binomial', 'State_Code','Nativity')))
Native <- unique(subset(STATENATIVITY, Nativity %in% c('N', 'NW'), select=c('Binomial', 'State_Code')))
Native$Nativity <- 'N'
STATENATIVITY <- merge(STATENATIVITY[,c('Binomial', 'State_Code')], Native, by=c('Binomial', 'State_Code'), all.x=T)
STATENATIVITY[is.na(STATENATIVITY$Nativity),]$Nativity <- 'I'
colnames(STATENATIVITY) <- c('Scientific.Name', 'State_Code','Nativity')
BONAPState <- unique(subset(BONAPState, grepl('\\.', Scientific.Name), 
                           select=c('Scientific.Name', 'State_Code','Nativity')))
BONAPState <- rbind(BONAPState, STATENATIVITY)
liststates <- readRDS('data/liststates.RDS')
liststates[liststates$simplecode %in% 'UM',]$simplecode <- 'NI'
BONAPState <- merge(BONAPState, liststates[,3:4], by.x='State_Code', by.y='simplecode', all.x=T)
#convert polygons to raster, then count the pixels
fip_sfna1 <- st_transform(fips_sfna, crs(Tw))
narast <- fasterize(fip_sfna1, Tw, field = 'ID', fun = "last")
napoints <- freq(narast)
fip_sfna1 <- merge(fip_sfna1, napoints, by.x= 'ID', by.y= 'value')
countbystate <- aggregate(fip_sfna1$count, by=list(fip_sfna1$STATECODE), FUN='sum')
colnames(countbystate) <- c('STATECODE', 'statecount')
countbyfips <- aggregate(fip_sfna1$count, by=list(fip_sfna1$FIPS), FUN='sum')
colnames(countbyfips) <- c('FIPS', 'fipscount')

fip_sfna1 <- merge(fip_sfna1, countbystate, by='STATECODE')
fip_sfna1 <- merge(fip_sfna1, countbyfips, by='FIPS')
fip_sfna1$pwt <- ifelse(fip_sfna1$COLOR %in% 'County Color - County Data', (1/fip_sfna1$fipscount)^0.5, (1/fip_sfna1$statecount)^0.5)

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
#----
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
taxonlist <- c('Tsuga canadensis',
               'Asimina triloba',
               'Asimina  parviflora',
               'Asimina reticulata',
               'Liatris aspera',
               'Liatris elegans',
               'Carphephorus bellidifolius',
               'Carphephorus odoratissimus',
               'Carphephorus pseudoliatris',
               'Garberia heterophylla',
               'Trillium undulatum',
               'Pseudotrillium rivale',
               
               'Eutrochium maculatum',
               'Eutrochium fistulosum',
               'Rhododendron maximum',
               'Rhododendron catawbiense',
               'Rhododendron macrophyllum',
               'Pinus echinata',
               'Pinus rigida')
taxonlist <- c('Cercis canadensis','Sassafras albidum', 'Lindera benzoin', 'Carya ovata')               

#----
#for (i in 1:1){
#taxon <- taxonlist[i]
taxon <- 'Picea mariana'

taxonstatelist <- BONAPState[BONAPState$Scientific.Name %in% taxon & BONAPState$Nativity %in% c('N','NW'), 'STATECODE']
taxonfipslist <- BONAPFips[BONAPFips$Scientific.Name %in% taxon, 'FIPS']

taxonstates <- subset(fip_sfna1, (STATECODE %in% taxonstatelist & !COLOR %in% 'County Color - County Data') |
                       ( STATECODE %in% taxonstatelist & FIPS %in% taxonfipslist))

pwt <- fasterize(taxonstates, Tw, field = 'pwt')
writeRaster(pwt, file='tmp/pwt.tif', overwrite=TRUE)
pwt <- raster('tmp/pwt.tif')

#####QGIS----
pwt2 <- pwt
#writeRaster(pwt2, file='tmp/pwt2.tif', overwrite=TRUE)

pwt2[!is.na(pwt2)] <- 1


params2 = get_args_man(alg = "gdal:proximity")
params2$INPUT = pwt2
params2$BAND
params2$VALUES = 1
params2$UNITS = 1
params2$MAX_DISTANCE
params2$REPLACE
params2$OPTIONS 
params2$EXTRA
params2$DATA_TYPE = 5
params2$OUTPUT = 'tmp/dist2.tif'
dist2 <- run_qgis(alg = 'gdal:proximity', params = params2, load_output = TRUE,
                 show_output_paths = TRUE, qgis_env = set_env())
get_options('gdal:proximity')
#----









#dist2 <- terra::distance(pwt)

rastbrick <- brick(pwt, water100k, shore, A, bedrock, clay, Deficit,
                   gdem, hydric, M, MAP, pAET, 
                   salids, sand, sealevel, slope, SoilpH, Surplus, 
                   Tc, Tcl, Tclx, Tgs, tgsmin, Tw, Twh, dist2)
rbrkfrm <- as.data.frame(rasterToPoints(rastbrick))
rbrkfrm <- subset(rbrkfrm, !is.na(sealevel)&  !is.na(salids) & !is.na(tgsmin) & !is.na(Tc) & !is.na(slope)  & !is.na(SoilpH) & !is.na(M) & !is.na(sand) & !is.na(clay))

present2 <- subset(rbrkfrm, pwt > 0)
present2$observed <- 1
present2$wt <- present2$pwt
absent2 <- subset(rbrkfrm, dist2 >= 50000 & dist2 < 500000 )
absent2$observed <- 0
absent2$wt <- 1

present2 <- rbind(present2, absent2)






################################## code for point data---------

if(nrow(prebiogeopts[grepl(taxon,prebiogeopts$taxon),])>0){
biogeopts <- prebiogeopts[grepl(taxon,prebiogeopts$taxon),]


sfpoints <- st_as_sf(x = biogeopts, 
                     coords = c('decimalLongitude', 'decimalLatitude'),
                     crs = "+proj=longlat +datum=WGS84")

sfpoints2 <- st_transform(sfpoints, crs(Tw))

joined <- st_join(sfpoints2, fip_sfna1)


#filter out wrong FIPS
go <- F
joined$fipsstatus <- 'no'
if(grepl(' ', taxon)){
  if(nrow(subset(BONAPFips, Scientific.Name %in% taxon ))>0){
    FIPSlist <- subset(BONAPFips, Scientific.Name %in% taxon, select=FIPS)[,1]
    joined[joined$FIPS %in% FIPSlist,]$fipsstatus <- 'yes'
    go <- T}
}else{
  if(nrow(subset(BONAPFips, grepl(taxon, Scientific.Name) ))>0){
    FIPSlist <- subset(BONAPFips, grepl(taxon, Scientific.Name), select=FIPS)[,1]
    joined[joined$FIPS %in% FIPSlist,]$fipsstatus <- 'yes'
    go <- T}}

joined$statestatus <- 'no'
#filter out wrong States
if(grepl(' ', taxon)){
  if(nrow(subset(BONAPState, Scientific.Name %in% taxon & Nativity %in% c('N','NW')))>0){
    Statelist <- subset(BONAPState, Scientific.Name %in% taxon & Nativity %in% c('N','NW'), select=STATECODE)[,1]
    joined[joined$STATECODE %in% Statelist,]$statestatus <- 'yes'
    go <- T}
}else{
  if(nrow(subset(BONAPState, grepl(taxon, Scientific.Name)  & Nativity %in% c('N','NW')))>0){
    Statelist <- subset(BONAPState, grepl(taxon, Scientific.Name) & Nativity %in% c('N','NW'), select=STATECODE)[,1]
    joined[joined$STATECODE %in% Statelist,]$statestatus <- 'yes'
    go <- T}}




#eliminate off map and water records
sfpoints2 <- subset(joined, !(COLOR %in% c('Blue or Delete') | is.na(COLOR)))
#eliminate non matching states
sfpoints2 <- subset(sfpoints2, !(COLOR %in% c('County Color - County Data', 'State Color - State Data Only') & statestatus %in% 'no'))
#eliminate non matching fips, except GA
sfpoints2 <- subset(sfpoints2, !(COLOR %in% 'County Color - County Data' & fipsstatus %in% 'no' & !STATECODE %in% 'US.GA'))


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
if(nrow(present[is.na(present$effort),])>0){
present[is.na(present$effort),]$effort <- 100}
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

png(filename=paste0('output3/', taxon,'.png'),width = 1500, height = 1500, units = 'px', pointsize = 10)
plot(predictraster, col = rgb(red = 0, green = 0.85, blue = 0, alpha = 1), legend=F)
plot(st_geometry(states),  lwd=0.1, fill=F, border = 'black', add=T)
plot(st_geometry(sfpoints2), pch=20, cex=0.5, col = rgb(red = 0.85, green = 0, blue = 0, alpha = 1), add=T)
dev.off()
}
}
