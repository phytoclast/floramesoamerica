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
df <- read.delim(filepath, skip = 150000, nrows = 800, header = FALSE, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
#This determines the column classes to apply to next table
df.colclass <- sapply(df,class)
df2 <- read.delim(filepath, skip = 110000, nrows = 800, header = FALSE, fileEncoding="UTF-8-BOM",
                  stringsAsFactors=FALSE, colClasses=df.colclass)
#This renames the headers
colnames(df) <- colnames(read.delim(filepath, nrows = 1, header = TRUE, fileEncoding="UTF-8-BOM"))

#this determines the number of lines in the file.
library(readr)
filelinecount <- length(read_lines(filepath))

#----
tab5rows <- read.table('C:/a/bio/biodiversity/2019/0007389-191105090559680/0007389-191105090559680.csv',
                       sep = "\t", header = TRUE, encoding = 'UTF-8', nrows = 5)
classes <- sapply(tab5rows, class)
tabAll <- read.table('C:/a/bio/biodiversity/2019/0007389-191105090559680/0007389-191105090559680.csv',
                     sep = "\t", header = TRUE, encoding = 'UTF-8', colClasses = classes)



bigfile.sample <- read.table('C:/a/bio/biodiversity/2019/0007389-191105090559680/0007389-191105090559680.csv',
                             sep = "\t", stringsAsFactors=FALSE, header=T, nrows=20)  

bigfile.colclass <- sapply(bigfile.sample,class)

bigfile.raw <- tbl_df('C:/a/bio/biodiversity/2019/0007389-191105090559680/0007389-191105090559680.csv',
                      stringsAsFactors=FALSE, header=T,nrow=10000, 
                               colClasses=attendance.colclass, comment.char=""))  

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
