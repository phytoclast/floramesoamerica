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

# make simplefeatures spatial dataframe ----------------------------------------------------------
biogeopts <- readRDS('data/prebiogeopts.RDS')

#----
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
               'Eutrochium steelei',
               'Eutrochium maculatum',
               'Eutrochium fistulosum',
               'Rhododendron maximum',
               'Rhododendron catawbiense',
               'Rhododendron macrophyllum',
               'Pinus echinata',
               'Pinus rigida')
               
taxonlist <- c('Cercis canadensis','Sassafras albidum', 'Lindera benzoin', 'Carya ovata')               


for (i in 4:4){
taxon <- taxonlist[i]

biogeopts1 <- occ_search(limit=25000,
                         phylumKey = 7707728, scientificName = taxon, 
                         decimalLatitude=lat, decimalLongitude =lon)$
  data[,c('scientificName', 'decimalLatitude', 'decimalLongitude')]
biogeopts1 <- cbind(taxon=taxon, biogeopts1)
biogeopts <- rbind(biogeopts, biogeopts1)
saveRDS(biogeopts, 'data/prebiogeopts.RDS')

}
