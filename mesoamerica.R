library(stringr)
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
Species$habit <- 'H.E'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("hemiepifito.txt", encoding = 'UTF-8')
Species$habit <- '.Ehemi'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("hierba.txt", encoding = 'UTF-8')
Species$habit <- 'H.'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("parasito.txt", encoding = 'UTF-8')
Species$habit <- 'H.i'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("subarbusto.txt", encoding = 'UTF-8')  
Species$habit <- 'S1'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("suculento.txt", encoding = 'UTF-8')
Species$habit <- '.U'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("terrestre.txt", encoding = 'UTF-8')
Species$habit <- '.T'
SpeciesHabits <- rbind(SpeciesHabits, Species)
Species<- read.delim("trepadora.txt", encoding = 'UTF-8')
Species$habit <- 'L.trailing'
SpeciesHabits <- rbind(SpeciesHabits, Species)
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
  str_split_fixed(SpeciesHabits[toupper(substr(SpeciesHabits$second,1,1)) == substr(SpeciesHabits$second,1,1),]$Name, " ",2)[,2]

SpeciesHabits[SpeciesHabits$second == '×',]$split <-
  paste(str_split_fixed(SpeciesHabits[SpeciesHabits$second == '×',]$Name, " ",4)[,1],
        str_split_fixed(SpeciesHabits[SpeciesHabits$second == '×',]$Name , " ",4)[,2],
        str_split_fixed(SpeciesHabits[SpeciesHabits$second == '×',]$Name, " ",4)[,3])
SpeciesHabits[SpeciesHabits$second == '×',]$auth <-
  str_split_fixed(SpeciesHabits[SpeciesHabits$second == '×',]$Name, " ",4)[,4]


SpeciesHabits$split <- paste(str_split_fixed(SpeciesHabits$Name, " ",3)[,1],str_split_fixed(SpeciesHabits$Name , " ",3)[,2])
SpeciesHabits$auth <- str_split_fixed(SpeciesHabits$Name, " ",3)[,3]

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

