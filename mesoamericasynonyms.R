library(stringr)
library(sf)
library(raster)
library(rgdal)
library(Taxonstand)
####----
#Flora Mesoamericana http://www.tropicos.org/ProjectAdvSearch.aspx?projectid=3&langid=66 ----
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
#Vascular Plants of the Americas https://www.tropicos.org/ProjectAdvSearch.aspx?projectid=83 ----
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
####----Crosstab ----
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
nakedlist <- unique(NA_Flora$Scientific.Name)
finallist <- Taxonstand::TPL(nakedlist[1:10])
for (i in 1:60){

synolist <- Taxonstand::TPL(nakedlist[c(((i*1000)+10):(((i+1)*1000+10)))])
finallist <-  rbind(finallist, synolist)
saveRDS(finallist, 'output/finallist.RDS')
}

#finallist2 <- unique(finallist)
#saveRDS(finallist2, 'output/finallist2.RDS')
nakedlist2 <- unique(finallist2$Taxon)
diff <- setdiff(nakedlist2, nakedlist)


pregbiflist <- read.delim( 'gbigspplist.csv', encoding = 'UTF-8')
nakedlist <- unique(pregbiflist$scientificName)
gbiflist <- Taxonstand::TPL(nakedlist[1:10])
for (i in 1:159){
  
  synolist <- Taxonstand::TPL(nakedlist[c(((i*1000)+10):(((i+1)*1000+10)))])
  gbiflist <-  rbind(gbiflist, synolist)
  saveRDS(gbiflist, 'output/gbiflist.RDS')
}
#gbiflist2 <- unique(gbiflist)
#saveRDS(gbiflist2, 'output/gbiflist2.RDS')

