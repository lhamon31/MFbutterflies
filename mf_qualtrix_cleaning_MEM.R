##MF Qualtrix cleaning script
#by Elizabeth Moore

#load necessary packages
library(readr)
library(plyr)
library(stringr)
library(tidyr)
library(lubridate)

#load data
#before loading data, maybe manually delete the first row of the qualtrix csv.
#There should be a way to do this but...naw
#this is laura's working directory
mf<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.responses.5.15.2017.csv")

##cleaning data
#Gets rid of the first row, which has nonsense import data
#(ask elizabeth if this is the same as the aformentioned row deletion)
mf<-mf[-1,] 

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
                 "Limenitis.artemis.astyanax", "Limenitis.archippus", "Phyciodes.tharos",
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

#remove unknown species columns
#we should keep them
#but i'm not sure how to reconcile them
mf$other.sp.text.1<-NULL
mf$other.sp.num.1<-NULL
mf$other.sp.text.2<-NULL
mf$other.sp.num.2<-NULL
mf$other.sp.text.3<-NULL
mf$other.sp.num.3<-NULL
mf$other.sp.text.4<-NULL
mf$other.sp.num.4<-NULL
mf$other.sp.text.5<-NULL
mf$other.sp.num.5<-NULL
mf$topics<-NULL

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
#using parse_date_time returns a POSIX class 
#and automatically assigns coordinated universal time.
#You have to assign east coast time zone with 'tz+"America/New_York"'
#If you convert to date class, you lose time- there should be a way around this
#but mem hasn't found it yet.
#might be ok to stay in POSX, as there are some handy ways to extract info from columns within class.
mf$start.date<-parse_date_time(mf$start.date,orders='mdy_HM',tz="America/New_York")
mf$end.date<-parse_date_time(mf$end.date,orders='mdy_HM',tz="America/New_York")
mf$recorded.date<-parse_date_time(mf$recorded.date,orders='mdy_HM',tz="America/New_York")
  #have to eliminate non-date values for this last one. yeahI know there's a quicker way
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
                             "Limenitis.artemis.astyanax", "Limenitis.archippus", "Phyciodes.tharos",
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
                           "Limenitis.artemis.astyanax", "Limenitis.archippus", "Phyciodes.tharos",
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
                 new.row.names=1:4165, #i know there's a way to make an arbitrary end
                 direction="long") 

#it would be good to reorder the columns now
#and to maybe put the ones we don't like at the end
#or to eliminate them completely

#write the cleaned data frame to a csv file
#row.names=FALSE prevents it from having an extra row with just numbers.
#spits out the csv in whatever folder the working directory is set.
#This is laura's working directory
write.csv(mf,file="C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.responses.long.5.16.2017.csv",row.names=FALSE)


