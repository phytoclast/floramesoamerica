library(stringr)
library(sf)
library(raster)
library(rgdal)
library(fasterize)
####----
Species <- read.delim("acuatica.txt", encoding = 'UTF-8')
Species$habit <- 'H.A'
SpeciesHabits <- Species
Species<- read.delim("arbolito.txt", encoding = 'UTF-8')
Species$habit <- 'T1'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("arbols.txt", encoding = 'UTF-8')
Species$habit <- 'T2'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("arbusto.txt", encoding = 'UTF-8')
Species$habit <- 'S2'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("bejuco.txt", encoding = 'UTF-8')
Species$habit <- 'L'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("epifito.txt", encoding = 'UTF-8')
Species$habit <- 'E.F'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("hemiepifito.txt", encoding = 'UTF-8')
Species$habit <- 'E.hemi'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("hierba.txt", encoding = 'UTF-8')
Species$habit <- 'H'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("parasito.txt", encoding = 'UTF-8')
Species$habit <- '.i'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("subarbusto.txt", encoding = 'UTF-8')  
Species$habit <- 'S1'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("suculento.txt", encoding = 'UTF-8')
Species$habit <- '.U'
SpeciesHabits <- rbind(SpeciesHabits, Species)
#Species<- read.delim("terrestre.txt", encoding = 'UTF-8')
#Species$habit <- '.terrestre'
#SpeciesHabits <- rbind(SpeciesHabits, Species)
#Species<- read.delim("trepadora.txt", encoding = 'UTF-8')
#Species$habit <- 'L.trailing'
#SpeciesHabits <- rbind(SpeciesHabits, Species)
####----
NA_Flora <- read.delim("NA_Flora.txt", encoding = 'UTF-8')
Habit_Symbols <- read.delim("Habit_Symbols.txt", encoding = 'UTF-8')
SpeciesHabits$split <- paste(str_split_fixed(SpeciesHabits$Name, " ",3)[,1],str_split_fixed(SpeciesHabits$Name , " ",3)[,2])
SpeciesHabits$auth <- str_split_fixed(SpeciesHabits$Name, " ",3)[,3]
SpeciesHabits$second <- str_split_fixed(SpeciesHabits$Name , " ",3)[,2]

SpeciesHabits[toupper(substr(SpeciesHabits$second,1,1)) == substr(SpeciesHabits$second,1,1),]$split <-
  str_split_fixed(SpeciesHabits[toupper(substr(SpeciesHabits$second,1,1))
                                == substr(SpeciesHabits$second,1,1),]$Name, " ",3)[,1]
SpeciesHabits[toupper(substr(SpeciesHabits$second,1,1)) == substr(SpeciesHabits$second,1,1),]$auth <- 
  str_split_fixed(SpeciesHabits[toupper(substr(SpeciesHabits$second,1,1)) == substr(SpeciesHabits$second,1,1),]$
                    Name, " ",2)[,2]

SpeciesHabits[SpeciesHabits$second == '×',]$split <-
  paste(str_split_fixed(SpeciesHabits[SpeciesHabits$second == '×',]$Name, " ",4)[,1],
        str_split_fixed(SpeciesHabits[SpeciesHabits$second == '×',]$Name , " ",4)[,2],
        str_split_fixed(SpeciesHabits[SpeciesHabits$second == '×',]$Name, " ",4)[,3])
SpeciesHabits[SpeciesHabits$second == '×',]$auth <-
  str_split_fixed(SpeciesHabits[SpeciesHabits$second == '×',]$Name, " ",4)[,4]



SpeciesHabits[grepl(' subsp\\. ',SpeciesHabits$Name),]$split <- 
  paste(str_split_fixed(SpeciesHabits[grepl(' subsp\\. ',SpeciesHabits$Name),]$Name, " ",5)[,1],
        str_split_fixed(SpeciesHabits[grepl(' subsp\\. ',SpeciesHabits$Name),]$Name , " ",5)[,2],
        str_split_fixed(SpeciesHabits[grepl(' subsp\\. ',SpeciesHabits$Name),]$Name , " ",5)[,3],
        str_split_fixed(SpeciesHabits[grepl(' subsp\\. ',SpeciesHabits$Name),]$Name , " ",5)[,4])
SpeciesHabits[grepl('sp\\.',SpeciesHabits$Name),]$auth <-
  str_split_fixed(SpeciesHabits[grepl('sp\\.',SpeciesHabits$Name),]$Name, " ",5)[,5]

SpeciesHabits[grepl(' var\\. ',SpeciesHabits$Name),]$split <- 
  paste(str_split_fixed(SpeciesHabits[grepl(' var\\. ',SpeciesHabits$Name),]$Name, " ",5)[,1],
        str_split_fixed(SpeciesHabits[grepl(' var\\. ',SpeciesHabits$Name),]$Name , " ",5)[,2],
        str_split_fixed(SpeciesHabits[grepl(' var\\. ',SpeciesHabits$Name),]$Name , " ",5)[,3],
        str_split_fixed(SpeciesHabits[grepl(' var\\. ',SpeciesHabits$Name),]$Name , " ",5)[,4])
SpeciesHabits[grepl(' var\\.',SpeciesHabits$Name),]$auth <-
  str_split_fixed(SpeciesHabits[grepl(' var\\. ',SpeciesHabits$Name),]$Name, " ",5)[,5]
####----Crosstab
SpeciesHabits$a <- 1
SpeciesHabits2 <- unique(SpeciesHabits[,c('split','auth')])
forma <- unique(SpeciesHabits$habit)
for (i in 1:length(forma)){
SpeciesHabits2 <- merge(SpeciesHabits2, SpeciesHabits[SpeciesHabits$habit %in% forma[i],c('split','auth','a')], by=c('split','auth'), all.x = TRUE)
SpeciesHabits2$new <- 0
SpeciesHabits2[!is.na(SpeciesHabits2$a),]$new <- 1
colnames(SpeciesHabits2)[colnames(SpeciesHabits2)=="new"] <- forma[i]
SpeciesHabits2 <- subset(SpeciesHabits2, select = -a)
}



NA_Floramerge <- unique(
merge(NA_Flora[,c('Scientific.Name','HabitSymbol')], 
      SpeciesHabits[,c('split','habit')], by.x='Scientific.Name', by.y = 'split', all.x = TRUE)
)

NA_Floramerge$HabitSymbol <- as.character(NA_Floramerge$HabitSymbol)
filtermerged <- subset(NA_Floramerge, !is.na(NA_Floramerge$habit))

#----

#Model Species Distribution
#Positive point data weighted 100%.
#Negative data weighted inverse of collective point data density.
#Positive state/county data inversely wieghted based on surface area.
#Distance from positive data can be used as training data, if model can weigh it.
#Model would probably use it as the primary variable, because by definition all inputs will be at zero distance from the points and therefore only will predict exactly the known locations.It may be more appropriate to multiply a proximity factor to the final prediction before selecting species rate of presence threshold.
#Distance from positive data can be used to weigh negative data. Greater distance lower weight. Lower density of total point data, lower weight.
filepath <- 'C:/a/bio/biodiversity/2019/0007389-191105090559680/0007389-191105090559680.csv'
txa <- read.delim('C:/a/bio/biodiversity/2019/0007389-191105090559680/0007389-191105090559680.csv',
                  sep = "\t", header = TRUE, encoding = 'UTF-8')
txa <- read.table('C:/a/bio/biodiversity/2019/0007389-191105090559680/0007389-191105090559680.csv',
                  sep = "\t", header = TRUE, encoding = 'UTF-8', nrows = 5)
#----
#this reads a certain amount of the input file
filepath <- 'C:/a/bio/biodiversity/2019/0007369-191105090559680/0007369-191105090559680.csv'
filepath <- 'C:/a/bio/biodiversity/2019/0007993-191105090559680/0007993-191105090559680.csv'
filepath <- 'C:/a/bio/biodiversity/2019/GBIFcaribean.csv.txt'
df <- read.delim(filepath, sep = "\t", quote = "", skip = 22972, nrows = 5, 
                 header = FALSE, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE, colClasses = 'character')
#This determines the column classes to apply to next table
df.colclass <- sapply(df,class)
df2 <- read.delim(filepath, sep = "\t", skip = 50000, nrows = 5, header = FALSE, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
#This renames the headers
colnames(df) <- colnames(read.delim(filepath, sep = "\t", nrows = 1, header = TRUE, fileEncoding="UTF-8-BOM"))

#this determines the number of lines in the file.
library(readr)
filelinecount <- length(read_lines(filepath))
df3 <- read_lines(filepath, locale = locale(encoding = "UTF-8"))
df3[22972]
library(rgbif)
library(ggplot2)
occ_count(basisOfRecord='OBSERVATION')
gbifrecord <- (name_lookup(query='Tabebuia glomerata', rank="species", return="data"))
gbifrecord <- occ_search(scientificName = "Tabebuia glomerata", limit = 1000)
gbifrecord$data
key <- name_backbone(name='Tabebuia glomerata')$speciesKey
dat <- occ_search(taxonKey=key, return='data', decimalLatitude='0,18', limit=3000)
dat <- occ_search(phylumKey = 7707728, return='data', decimalLatitude='42,44', decimalLongitude ='-87,-85', limit=3000)
dat2 <- subset(dat, key =='415961877')
gbifmap(dat)

occcount <- occ_search(limit=0,
           phylumKey = 7707728,
           decimalLatitude='30,44', decimalLongitude ='-90,-89')$meta$count
occcount
#----
#run through lat lon to count records
library(rgbif)
lltab<- as.data.frame(cbind(lat0=c(0),lon0=c(0),lat1=c(0),lon1=c(0),ct=c(0)))
latmin <- 5
latmax <- 85
lonmin <- -180
lonmax <- -20
latincmax <- 20
lonincmax <- 32
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


row0<- c(17, 25, -165, -150)
row1<- c(49, 85, -180, -25)
row2 <- c(37, 49, -130, -110)
row3 <- c(37, 49, -110, -80)
row4 <- c(37, 49, -80, -50)
row5 <- c(25, 37, -105, -70)
row6 <- c(25, 37, -110, -105)
row7 <- c(13, 25, -115, -95)
row8 <- c(13, 25, -95, -55)
row9 <- c(5, 13, -90, -60)
coordtable <- rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9)











library(stringi)
stri_enc_detect(filepath)
#----
classes <- "character"
classes <- sapply(df, class)




Species <- read.delim("acuatica.txt", encoding = 'UTF-8')
USASpp <- read.delim('C:/a/bio/biodiversity/2019/0007369-191105090559680/0007369-191105090559680.csv')
testSpp <- read.delim('C:/a/bio/biodiversity/2019/0007389-191105090559680/0007389-191105090559680.csv')
testSpp <- read.delim('C:/a/bio/biodiversity/2019/0007993-191105090559680/0007993-191105090559680.csv')
testSpp$decimalLatitude <- as.numeric(as.character(testSpp$decimalLatitude))
testSpp$decimalLongitude <- as.numeric(as.character(testSpp$decimalLongitude))

testSppinat <- subset(testSpp, institutionCode %in% 'iNaturalist' & species %in% 'Valeriana sitchensis')
testSppnotinat <- subset(testSpp, !institutionCode %in% 'iNaturalist' & species %in% 'Valeriana sitchensis')


plot(testSppinat$decimalLongitude, testSppinat$decimalLatitude, 
     col = rgb(red = 0, green = 1, blue = 0, alpha = 0.15))
points(testSppnotinat$decimalLongitude, testSppnotinat$decimalLatitude,
       col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), new=FALSE)
testagg <- aggregate(testSpp[,'species'], by=list(testSpp$species), FUN='length')
testSpp2 <- unique(testSpp[,c('species','decimalLongitude', 'decimalLatitude')])

#----
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
points2 <- merge(points2, st_drop_geometry(fips_sfna[,c('ID','FIPS')]), by.x='layer',by.y='ID', all.x = TRUE)
points3 <- merge(points1, points2, by=c('x','y'))
#preproject
fips_sfnarepro <- st_transform(fips_sfna, st_crs(Tw))
plot(fips_sfnarepro[1])
namap <- st_rasterize(fips_sfnarepro, Tw, field="GROUP", fun='last', background=NA)
ptsfit<-st_rasterize(ptsALL,Tw,field="best", fun='last', background=NA)





library(raster)
library(fasterize)
library(sf)
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
