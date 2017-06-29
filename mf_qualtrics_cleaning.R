##MF Qualtrix cleaning script
#by Elizabeth Moore, Kati Moore, and Laura Hamon

#load necessary packages
library(readr)
library(plyr)
library(stringr)
library(tidyr)
library(lubridate)

#load data
#this is based on laura's working directory. change as needed.
mf<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.responses.6.29.2017.csv")

##cleaning data
#Gets rid of the first row, which has nonsense import data
#(ask elizabeth if this is the same as the aformentioned row deletion)
mf<-mf[-2,] 

#rename columns
names(mf)<-c("start.date", "end.date", "response.type", "ip.address", 
             "progress", "duration.sec","finished","recorded.date",
             "response.id", "recipient.last.name", "recipient.first.name",
             "recipient.email", "external.data.reference","location.lat",
             "location.long", "distribution.channel", "observer.name",
             "number.of.observers", "entry.name", "date.observed",
             "temp", "conditions", 
             "start", "end", "proficiency", "Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
             "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
             "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
             "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
             "Pontia.protodice", "Feniseca.tarquinis", "Cupido.comyntas",
             "Celastrina.neglecta", "Celastrina.ladon","Unknown.blue",
             "Strymon.melinus", "Calycopis.cecrops", "Mitoura.gryneus",
             "Callophrys.henrici", "Satyrium.calanus", "Atlides.halesus",
             "Parrhasius.m.album", "Callophrys.niphon", "Satyrium.titus",
             "Satyrium.favonus", "Satyrium.liparops", "Asterocampa.celtis",
             "Asterocampa.clyton", "Danaus.plexippus", "Speyeria.cybele",
             "Euptoieta.claudia", "Agraulis.vanillae", "Libytheana.carinenta",
             "Limenitis.arthemis.astyanax", "Limenitis.archippus", "Phyciodes.tharos",
             "Junonia.coenia", "Vanessa.atalanta", "Polygonia.interrogationis",
             "Polygonia.comma", "Nymphalis.antiopa", "Vanessa.virginiensis",
             "Vanessa.cardui", "Chlosyne.nycteis", "Hermeuptychia.sosybius",
             "Cyllopsis.gemma", "Megisto.cymela", "Cercyonis.pegala",
             "Lethe.anthedon", "Lethe.appalachia", "Unknown.satyr",
             "Atalopedes.campestris", "Lerema.accius", "Poanes.zabulon",
             "Polites.origenes", "Pompeius.verna", "Euphyes.vestris",
             "Anclyoxypha.numitor", "Wallengrenia otho", "Hylephila.phyleus",
             "Wallengrenia.egeremet", "Panoquina.ocola", "Nastra.lherminier",
             "Amblyscirtes.vialis", "Anatrytone.logan", "Atrytonopsis.hianna",
             "Polites.themistocles", "Euphyes.dion", "Unknown.grass.skipper",
             "Epargyreus.clarus", "Erynnis.juvenalis", "Erynnis.horatius",
             "Thorybes.pylades", "Thorybes.bathyllus", "Pyrgus.communis",
             "Erynnis.brizo", "Urbanus.proteus", "Erynnis.baptisiae", 
             "Erynnis.zarucco", "Unknown.spreadwing.skipper", "Unknown.species",
             "other.sp.text.1", "other.sp.num.1", "other.sp.text.2", "other.sp.num.2",
             "other.sp.text.3", "other.sp.num.3","other.sp.text.4", "other.sp.num.4",
             "other.sp.text.5", "other.sp.num.5","topics")

#i'm getting rid of the start date, end date columns because these just detail how long it takes for a person to do the survey
mf <- mf[ -c(1,2) ]
#also getting rid of garth and beck surveys
mf<-mf[ ! mf$observer.name %in% c("Test 1","test 2","Garth McBoatman", "Garth McBoatface","yo","test","Beck in 1995","Beck","beck","d"), ]

#remove unknown species columns
#we should keep them
#but i'm not sure how to reconcile them
mf<-mf[-c(109:119)]
mf$recorded.date<-NULL

##gets rid of weird creepy Latin
#finds rows that have more than 10 characters and deletes them
#set to look for this in date observed column
mf<-mf[!nchar(as.character(mf$date.observed))>10,]

#splits up observer names and puts into observer 1, observer 2 etc:
#gsub replaces the first argument with the second
#makes everything uniform in syntax so that str_split_fixed can split the columns.
#In this case MEM used "and", and had to finagle the gsubs for the specific syntax of each entry.
#can be altered depending on syntax of column.
mf$observer.name<- gsub("&", " and",mf$observer.name)
mf$observer.name<- gsub(", and", ",",mf$observer.name)
mf$observer.name<- gsub(",", " and",mf$observer.name)

obs<-str_split_fixed(mf$observer.name, c("and"), 3)

mf$obs1<-obs[,1]
mf$obs2<-obs[,2]
mf$obs3<-obs[,3]

##get initials for each observer
#make an object that has the first observer (obs object, column 1)
#and split the names into first and last using strsplit and a space ("") as the separator
#then use the sapply and function part to collapse into the first character
#and the toupper makes them all upper case
s <- strsplit(obs[,1], " ")

mf$obs1.in<- sapply(s, function(x){
  toupper(paste(substring(x, 1, 1), collapse = ""))
})

k<-strsplit(obs[,2], " ")

mf$obs2.in<- sapply(k, function(x){
  toupper(paste(substring(x, 1, 1), collapse = ""))
})


l<-strsplit(obs[,2], " ")

mf$obs3.in<- sapply(l, function(x){
  toupper(paste(substring(x, 1, 1), collapse = ""))
})


mf$obs1.in

##formatting the date columns
#have to eliminate non-date values for this last one. yeah I know there's a quicker way
mf$date.observed[mf$date.observed == "d"] <- NA
mf$date.observed[mf$date.observed=="test"] <- NA
mf$date.observed[mf$date.observed==""] <- NA
mf$date.observed<-parse_date_time(mf$date.observed,orders='mdy') 

#creates a column with just the year of observation
mf$year<-year(mf$date.observed)
mf$year

#creates a column with only the month
#can do as a number
#or use the argument 'label=TRUE'
#and it will give back the character month name
mf$month<-month(mf$date.observed,label = TRUE)  
mf$month

##reshape from wide to long 
# might want to change the number of new.row.names based on how many rows (responses)
#   there are in the data when you download it from Qualtrics.
longdat<-reshape(mf,
                 varying = c("Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                             "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                             "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
                             "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
                             "Pontia.protodice", "Feniseca.tarquinis", "Cupido.comyntas",
                             "Celastrina.neglecta", "Celastrina.ladon","Unknown.blue",
                             "Strymon.melinus", "Calycopis.cecrops", "Mitoura.gryneus",
                             "Callophrys.henrici", "Satyrium.calanus", "Atlides.halesus",
                             "Parrhasius.m.album", "Callophrys.niphon", "Satyrium.titus",
                             "Satyrium.favonus", "Satyrium.liparops", "Asterocampa.celtis",
                             "Asterocampa.clyton", "Danaus.plexippus", "Speyeria.cybele",
                             "Euptoieta.claudia", "Agraulis.vanillae", "Libytheana.carinenta",
                             "Limenitis.arthemis.astyanax", "Limenitis.archippus", "Phyciodes.tharos",
                             "Junonia.coenia", "Vanessa.atalanta", "Polygonia.interrogationis",
                             "Polygonia.comma", "Nymphalis.antiopa", "Vanessa.virginiensis",
                             "Vanessa.cardui", "Chlosyne.nycteis", "Hermeuptychia.sosybius",
                             "Cyllopsis.gemma", "Megisto.cymela", "Cercyonis.pegala",
                             "Lethe.anthedon", "Lethe.appalachia", "Unknown.satyr",
                             "Atalopedes.campestris", "Lerema.accius", "Poanes.zabulon",
                             "Polites.origenes", "Pompeius.verna", "Euphyes.vestris",
                             "Anclyoxypha.numitor", "Wallengrenia otho", "Hylephila.phyleus",
                             "Wallengrenia.egeremet", "Panoquina.ocola", "Nastra.lherminier",
                             "Amblyscirtes.vialis", "Anatrytone.logan", "Atrytonopsis.hianna",
                             "Polites.themistocles", "Euphyes.dion", "Unknown.grass.skipper",
                             "Epargyreus.clarus", "Erynnis.juvenalis", "Erynnis.horatius",
                             "Thorybes.pylades", "Thorybes.bathyllus", "Pyrgus.communis",
                             "Erynnis.brizo", "Urbanus.proteus", "Erynnis.baptisiae", 
                             "Erynnis.zarucco", "Unknown.spreadwing.skipper", "Unknown.species"),
                 v.names="num.indv",
                 timevar="species",
                 times = c("Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                           "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                           "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
                           "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
                           "Pontia.protodice", "Feniseca.tarquinis", "Cupido.comyntas",
                           "Celastrina.neglecta", "Celastrina.ladon","Unknown.blue",
                           "Strymon.melinus", "Calycopis.cecrops", "Mitoura.gryneus",
                           "Callophrys.henrici", "Satyrium.calanus", "Atlides.halesus",
                           "Parrhasius.m.album", "Callophrys.niphon", "Satyrium.titus",
                           "Satyrium.favonus", "Satyrium.liparops", "Asterocampa.celtis",
                           "Asterocampa.clyton", "Danaus.plexippus", "Speyeria.cybele",
                           "Euptoieta.claudia", "Agraulis.vanillae", "Libytheana.carinenta",
                           "Limenitis.arthemis.astyanax", "Limenitis.archippus", "Phyciodes.tharos",
                           "Junonia.coenia", "Vanessa.atalanta", "Polygonia.interrogationis",
                           "Polygonia.comma", "Nymphalis.antiopa", "Vanessa.virginiensis",
                           "Vanessa.cardui", "Chlosyne.nycteis", "Hermeuptychia.sosybius",
                           "Cyllopsis.gemma", "Megisto.cymela", "Cercyonis.pegala",
                           "Lethe.anthedon", "Lethe.appalachia", "Unknown.satyr",
                           "Atalopedes.campestris", "Lerema.accius", "Poanes.zabulon",
                           "Polites.origenes", "Pompeius.verna", "Euphyes.vestris",
                           "Anclyoxypha.numitor", "Wallengrenia otho", "Hylephila.phyleus",
                           "Wallengrenia.egeremet", "Panoquina.ocola", "Nastra.lherminier",
                           "Amblyscirtes.vialis", "Anatrytone.logan", "Atrytonopsis.hianna",
                           "Polites.themistocles", "Euphyes.dion", "Unknown.grass.skipper",
                           "Epargyreus.clarus", "Erynnis.juvenalis", "Erynnis.horatius",
                           "Thorybes.pylades", "Thorybes.bathyllus", "Pyrgus.communis",
                           "Erynnis.brizo", "Urbanus.proteus", "Erynnis.baptisiae", 
                           "Erynnis.zarucco", "Unknown.spreadwing.skipper", "Unknown.species"),
                 direction="long") 

row.names(longdat)=1:nrow(longdat) 

#remove nas from spp columns otherwise they're counted as an entry
longdat$num.indv<-sapply(longdat$num.indv, function(f){is.na(f)<-which(f == '');f}) #first convert blanks to NAs
removenas <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
longdat<-removenas(longdat,"num.indv")#drop rows with NA in num.indv column

#eliminate more non-useful columns (may be modified as necessary) 
#(also figure out what to do about unknown spp in previously dropped columsn)
longdat <- longdat[ -c(1:13) ]

#write the cleaned data frame to a csv file
#row.names=FALSE prevents it from having an extra row with just numbers.
#spits out the csv in whatever folder the working directory is set.
#This is laura's working directory
write.csv(longdat,file="C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.responses.cleaned.csv",row.names=FALSE)

####################################################################################
################ADDING IN THE UBR DATA for the purpose of comparing proportions
####################################################################################

#subset necessary columns from qualtrics data 
qualtrics.dat<-longdat[,c("species","num.indv","year")]

#load UBR files
ubr.folder <- "C:/Users/lhamo/Documents/Biology/mf bflies 2017/ubr.data/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=ubr.folder, pattern="*.csv") # create list of all .csv files in folder
ubr.dat <- do.call("rbind", lapply(file_list, function(x)read.csv(paste(ubr.folder, x, sep=''), 
                            stringsAsFactors = FALSE)))

#subset necessary columns of UBR dat
ubr.dat<-ubr.dat[,c("time.date", "speciesScientific")]
#add column w/ num.indv
ubr.dat$num.indv<-rep(1)
#rename columns
names(ubr.dat)<-c("date.observed","species","num.indv")

#update ubr species name to modern parlance 
ubr.dat$species<- gsub("Eurema nicippe", "Abaeis nicippe", ubr.dat$species)
ubr.dat$species<- gsub("Eurema lisa", "Pyrisita lisa", ubr.dat$species)
ubr.dat$species<- gsub("Limenitis arthemis", "Limenitis arthemis astyanax", ubr.dat$species)
ubr.dat$species<- gsub("Everes comyntas", "Cupido comyntas", ubr.dat$species)
ubr.dat$species<- gsub("ancyloxpha numitor", "Ancyloxypha numitor", ubr.dat$species)
ubr.dat$species<- gsub("enodia anthedon", "Lethe anthedon", ubr.dat$species)

#convert date of ubr.dat to just year
ubr.dat$year<-format(as.Date(ubr.dat$date.observed, format="%Y-%m-%d"),"%Y")
ubr.dat<-ubr.dat[,c("species","num.indv","year")] #subsetting again yeah i know I made this complicated

#trying to get the the species names formatted the same way
#removing incompatible/unknown names in ubr data
ubr.dat<-ubr.dat[ ! ubr.dat$species %in% c("?Erynnis horatius","fritillary","black skipper","duskywing","snout", "cymela","blue","skipper","Pearl crescent?","polygonia","papilio","enodia","unknown"), ]
#removing incompatible/unknown names in qualtrics data
qualtrics.dat<-qualtrics.dat[ ! qualtrics.dat$species %in% c("Unknown.blue","Unknown.satyr","Unknown.grass.skipper","Unknown.species","Unknown.sulphur", "Unknown.spreadwing.skipper"), ]
#omit rows in no date in qualtrics data
qualtrics.dat<-na.omit(qualtrics.dat)
#make all characters lowercase in both datasets
ubr.dat<-data.frame(lapply(ubr.dat, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))
qualtrics.dat<-data.frame(lapply(qualtrics.dat, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))
#replace space w/ period in ubr data
ubr.dat$species<-gsub(" ", ".", ubr.dat$species) 

#rbind qualtrics dat and ubr dat
combined.dat<-rbind(qualtrics.dat, ubr.dat)
#nicer names
names(combined.dat)<-c("species","number","year")

#save output
write.csv(combined.dat,file="C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.with.ubr.cleaned.csv",row.names=FALSE)

