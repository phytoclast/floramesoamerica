library(raster)
library(sf)
library(fasterize)
library(sp)
library(ranger)
library(terra)
library(stringr)
# Load data ----
Statechecklist <- readRDS('data/BONAPState2015.RDS')
Countychecklist <- readRDS('data/BONAPFips2015.RDS')
MexiStatechecklist <- read.delim('data/mexstaterecords.txt')
MexiStatelist <- readRDS('data/MexiStatelist.RDS')
MexiStatelist$State <- trimws(MexiStatelist$State)

states <- readRDS('data/states.RDS')
states.list <- st_drop_geometry(states)
states.list <- merge(states.list, MexiStatelist, by.x = 'STATE', by.y = 'name2', all.x = T,  all.y = T)
# process data ----
MexiStatechecklist <- merge(MexiStatechecklist, states.list[,c('Abrev','STATECODE')],
                                                            by.x='stateabr', by.y='Abrev')
lumplist <- c(
  'Alisma plantago-aquatica',
  'Athyrium filix-femina',
  'Osmunda regalis'
)
newlist <- c(
  'Alisma triviale',
  'Athyrium cyclosorum',
  'Osmunda spectabilis'
)
replacement <- as.data.frame(cbind(lumplist, newlist))
#check to see if code works #MexiStatechecklist[grepl('Osmunda', MexiStatechecklist$binomial) , ]$binomial
for (i in 1:nrow(replacement)){try(
  MexiStatechecklist[MexiStatechecklist$binomial %in% replacement[i,'lumplist'] , ]$binomial <- 
    replacement[i,'newlist']
  ,silent = TRUE)
}

BONAP_Checklist <-  read.delim('data/BONAP_Checklist.txt', encoding = 'UTF-8')
b.ac <- BONAP_Checklist[,c('Newseq', 'Genus', 'Scientific.Name', 'Author')]
b.sy <- BONAP_Checklist[,c('Newac', 'Genus', 'Scientific.Name', 'Author')]
colnames(b.ac) <- c('id.ac', 'Genus.ac', 'Taxon.ac', 'Author.ac')
colnames(b.sy) <- c('id.ac', 'Genus', 'Taxon', 'Author')
b <- merge(b.ac, b.sy, by='id.ac')
b.auct <- subset(b, grepl('auct.', Author))
b <- subset(b, !grepl('auct.', Author))
mexlist <- unique(MexiStatechecklist$binomial)
Mexicansyn <- as.data.frame(mexlist)
Mexicansyn <- merge(Mexicansyn, b[,c('Taxon.ac', 'Taxon')], by.x='mexlist', by.y='Taxon', all.x=TRUE)
Mexicansyn$newname <- 'same'
Mexicansyn[!Mexicansyn$mexlist == Mexicansyn$Taxon.ac & !is.na(Mexicansyn$Taxon.ac),]$newname <- 'new'
Mexicansyn$Binomial.new <- paste(str_split_fixed(Mexicansyn$Taxon.ac, ' ',3)[,1],
                          str_split_fixed(Mexicansyn$Taxon.ac , ' ',3)[,2])


#write.csv(mexlist, 'mexlist.csv')


MexiStatechecklist$Nativity <- 'N'
MexiStatechecklist <- subset(MexiStatechecklist, select= c('stateabr','binomial','Nativity','STATECODE'))
MexiStatechecklist <- merge(MexiStatechecklist, b[,c('Taxon.ac', 'Taxon')], by.x='binomial', by.y='Taxon', all.x=TRUE)
MexiStatechecklist$Binomial.new <- paste(str_split_fixed(MexiStatechecklist$Taxon.ac, ' ',3)[,1],
                                 str_split_fixed(MexiStatechecklist$Taxon.ac , ' ',3)[,2])
MexiStatechecklist$Binomial.old <- MexiStatechecklist$binomial
MexiStatechecklist[!is.na(MexiStatechecklist$Binomial.new & !MexiStatechecklist$Binomial.new %in% ''),]$binomial <- 
  MexiStatechecklist[!is.na(MexiStatechecklist$Binomial.new & !MexiStatechecklist$Binomial.new %in% ''),]$Binomial.new
MexiStatechecklist <- subset(MexiStatechecklist, select= c('stateabr','binomial','Nativity','STATECODE'))

#combine with bonap...
BONAPState <- readRDS('data/BONAPState_americas.RDS')
colnames(MexiStatechecklist) <- colnames(BONAPState)
NAmericaState <- rbind(BONAPState, MexiStatechecklist)


#saveRDS(NAmericaState, 'data/NAmericaState.RDS')
# looking at synonyms again.----
# Probably not use all Tropicos checklist until taxonomy is fixed. Some species are missing from synonymy. May be map only adding Mexico. Could get Panama at most ...
NAmericaState <- readRDS('data/NAmericaState.RDS')
finallist2 <- readRDS('data/finallist2.RDS')

thesespp <- subset(finallist2, Genus %in% c('Osmunda', 'Osmundastrum'))

naklist <- nakedlist[grepl('Osmund', nakedlist)]

synolist <- Taxonstand::TPL(naklist)