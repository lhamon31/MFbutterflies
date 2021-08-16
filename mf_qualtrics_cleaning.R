##MF Qualtrix cleaning script
#by Elizabeth Moore, Kati Moore, and Laura Hamon

####################################################################################
################LOAD AND CLEAN QUALTRICS DATA
####################################################################################

#load necessary packages
library(readr)
library(plyr)
library(stringr)
library(tidyr)
library(lubridate)

#load latest version of qualtrics data
#this is based on laura's working directory. change as needed.
mf<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/MFbutterflies.qualtricsdownload.06.03.2021.csv")

#Get rid of the first two rows, which has nonsense import data and extraneous variable names
mf<-mf[-1:-2,] 

#rename columns so that they're easier to work with
names(mf)<-c("startDate", "endDate", "status", "ipAddress", 
             "progress", "durationInSeconds","finished","recordedDate",
             "responseId", "recipientLastName", "recipientFirstName",
             "recipientEmail", "externalDataReference","locationLatitude",
             "locationLongitude", "distributionChannel", "userLanguage", "observerName",
             "numberObservers", "dataEntererName", "dateObserved",
             "temperature", "siteConditions", 
             "startTime", "endTime", "experienceLevel", "Papilio.glaucus", "Papilio.troilus", 
             "Battus.philenor",
             "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
             "Colias.philodice", "Colias.eurytheme", "Abaeis.nicippe",
             "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
             "Pontia.protodice", "Feniseca.tarquinius", "Cupido.comyntas",
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
             "Enodia.anthedon", "Satyrodes.appalachia", "Unknown.satyr",
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

#Get rid of test surveys entered with fake names
mf<-mf[ ! mf$observerName %in% c("Test 1","test 2","Garth McBoatman","foo", "Garth McBoatface","yo","test","Beck in 1995","Beck","beck","d"), ]

#finds rows that have more than 10 characters in the date column and deletes them
#to remove Lorem ipsum
mf<-mf[!nchar(as.character(mf$dateObserved))>10,]

#remove "other" columns for now
#we should keep them
#but i'm not sure how to reconcile them
mf<-mf[-c(112:122)]

#splits up observer names and puts into observer 1, observer 2 etc:
#gsub replaces the first argument with the second
#makes everything uniform in syntax so that str_split_fixed can split the columns.
#In this case MEM used "and", and had to finagle the gsubs for the specific syntax of each entry.
#can be altered depending on syntax of column.
mf$observerName<- gsub("&", " and",mf$observerName)
mf$observerName<- gsub(", and", ",",mf$observerName)
mf$observerName<- gsub(",", " and",mf$observerName)

obs<-str_split_fixed(mf$observerName, c("and"), 3)

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
#First, let's fix an early entry that lists the year as "216". This will fail to parse otherwise
levels(mf$dateObserved)[match("08/21/216",levels(mf$dateObserved))] <- "08/21/2016"
#Also correct Sebastion and Chloe dates that are listed as 1986 (instead of 2020)
levels(mf$dateObserved)[match("3/29/1986",levels(mf$dateObserved))] <- "3/29/2020"
#convert to date-time object
mf$dateObserved<-parse_date_time(mf$dateObserved,orders='mdy') 
#it will say that '1 failed to parse'. This is because this date is missing on one entry. 
#creates a column with just the year of observation
mf$year<-year(mf$dateObserved)
#creates a column with only the month
#can do as a number
#or use the argument 'label=TRUE'
#and it will give back the character month name
mf$month<-month(mf$dateObserved,label = TRUE)  

##reshape from wide to long, varying around species names
longdat<-reshape(mf,
                 varying = c("Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                             "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                             "Colias.philodice", "Colias.eurytheme", "Abaeis.nicippe",
                             "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
                             "Pontia.protodice", "Feniseca.tarquinius", "Cupido.comyntas",
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
                             "Enodia.anthedon", "Satyrodes.appalachia", "Unknown.satyr",
                             "Atalopedes.campestris", "Lerema.accius", "Poanes.zabulon",
                             "Polites.origenes", "Pompeius.verna", "Euphyes.vestris",
                             "Anclyoxypha.numitor", "Wallengrenia otho", "Hylephila.phyleus",
                             "Wallengrenia.egeremet", "Panoquina.ocola", "Nastra.lherminier",
                             "Amblyscirtes.vialis", "Anatrytone.logan", "Atrytonopsis.hianna",
                             "Polites.themistocles", "Euphyes.dion", "Unknown.grass.skipper",
                             "Epargyreus.clarus", "Erynnis.juvenalis", "Erynnis.horatius",
                             "Thorybes.pylades", "Thorybes.bathyllus", "Pyrgus.communis",
                             "Erynnis.brizo", "Urbanus.proteus", "Erynnis.baptisiae", 
                             "Erynnis.zarucco", "Unknown.spreadwing.skipper"),
                 v.names="num.indv",
                 timevar="species",
                 times = c("Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                           "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                           "Colias.philodice", "Colias.eurytheme", "Abaeis.nicippe",
                           "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
                           "Pontia.protodice", "Feniseca.tarquinius", "Cupido.comyntas",
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
                           "Enodia.anthedon", "Satyrodes.appalachia", "Unknown.satyr",
                           "Atalopedes.campestris", "Lerema.accius", "Poanes.zabulon",
                           "Polites.origenes", "Pompeius.verna", "Euphyes.vestris",
                           "Anclyoxypha.numitor", "Wallengrenia otho", "Hylephila.phyleus",
                           "Wallengrenia.egeremet", "Panoquina.ocola", "Nastra.lherminier",
                           "Amblyscirtes.vialis", "Anatrytone.logan", "Atrytonopsis.hianna",
                           "Polites.themistocles", "Euphyes.dion", "Unknown.grass.skipper",
                           "Epargyreus.clarus", "Erynnis.juvenalis", "Erynnis.horatius",
                           "Thorybes.pylades", "Thorybes.bathyllus", "Pyrgus.communis",
                           "Erynnis.brizo", "Urbanus.proteus", "Erynnis.baptisiae", 
                           "Erynnis.zarucco", "Unknown.spreadwing.skipper"),
                 direction="long") 

row.names(longdat)=1:nrow(longdat) 

#remove nas from spp columns otherwise they're counted as an entry
#you should see the number of observations drop precipitously after this step
longdat$num.indv<-sapply(longdat$num.indv, function(f){is.na(f)<-which(f == '');f}) #first convert blanks to NAs
removenas <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
longdat<-removenas(longdat,"num.indv")#drop rows with NA in num.indv column


#if desired, the qualtrics data can be exported and examined at this point
#for end-of-year statistics

####################################################################################
################ADDING THE UBR DATA TO THE QUALTRICS DATA
################UBR DATA INCLUDES DATA COLLECTED BY KATI MOORE IN 2016 USING THE UBR APP
####################################################################################

#adding in the ubr dat
#load UBR files
ubr.folder <- "C:/Users/lhamo/Documents/Biology/mf bflies 2017/ubr.data/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=ubr.folder, pattern="*.csv") # create list of all .csv files in folder
ubr.dat <- do.call("rbind", lapply(file_list, function(x)read.csv(paste(ubr.folder, x, sep=''), 
                                                                  stringsAsFactors = FALSE)))

#for cloudapp, we only need a few variables as follows: 
  #family, subfamily_sci, subfamily_common, sciName, comName, BAMONA, 
  #date.observed, number, observer,year, month, day, location, source, image
#therefore, we could save some time by keeping only columns we need in both ubr and mf dat

#subset ubr.dat to relevant variables
ubr.vars <- c("speciesCommon", "speciesScientific", "subfamilyCommon", "subfamilyScientific", "number", "time.date")
ubr <- ubr.dat[ubr.vars]
#rename ubr columns to match 
names(ubr)<-c("comName", "sciName","subfamily_common", "subfamily_sci", "number", "date.observed")

#subset mf dat to relevant variables
qualtrics.vars <- c("observerName", "dateObserved","species", "num.indv")
qualtrics <- longdat[qualtrics.vars]
#rename mf columns to match 
names(qualtrics) <- c("observer", "date.observed", "sciName", "number")

#add columns to ubr to make it match mf
#might as well specify that the observer is Kati every time for this data
ubr["observer"] = "K. Moore"
ubr["source"] = "ubr" #specify data source

#add columns to mb to make it match ubr
qualtrics["comName"] = ""
qualtrics["subfamily_common"] = ""
qualtrics["subfamily_sci"] = ""
qualtrics["source"] = "qualtrics" #specify data source

#let's make sure our data types are uniform
ubr$date.observed<-parse_date_time(ubr$date.observed,orders='ymd')

#let's bind the ubr and qualtrics dat
fulldat1 <- rbind (ubr, qualtrics)

#create year, month, and day
#convert to date-time object
#creates a column with just the year of observation
fulldat1$year<-year(fulldat1$date.observed)
#creates a column with only the month (numeric)
fulldat1$month<-month(fulldat1$date.observed)  
#creates a column with only the day 
fulldat1$day<-day(fulldat1$date.observed)

#make species names up-to-date (ubr data specifically)
fulldat1$sciName<- gsub("Eurema nicippe", "Abaeis nicippe", fulldat1$sciName)
fulldat1$sciName<- gsub("Eurema lisa", "Pyrisita lisa", fulldat1$sciName)
fulldat1$sciName<- gsub("Limenitis arthemis", "Limenitis arthemis astyanax", fulldat1$sciName)
fulldat1$sciName<- gsub("Everes comyntas", "Cupido comyntas", fulldat1$sciName)
fulldat1$sciName<- gsub("ancyloxpha numitor", "Ancyloxypha numitor", fulldat1$sciName)
fulldat1$sciName<- gsub("enodia anthedon", "Enodia anthedon", fulldat1$sciName)

#Capitalize that first letter
#This is a function to do that.
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

fulldat1$sciName <- capFirst(fulldat1$sciName)

#replace period w/ space 
fulldat1$sciName<-gsub('[.]', " ", fulldat1$sciName) 

#make location Mason Farm
fulldat1["location"] = "mf"

####################################################################################
################ADDING IN LEGRAND AND 2015 HAMON DATA  
####################################################################################

#This addds pre-project LeGrand data, as well as data collected by Laura in 2015
#I am cheating a little bit, and using a pre-cleaned data set ("legrand_hamon_ONLY.csv"), 
#assuming this data is more-or-less static.
#Conceivably one could make a script that pulls observations from the yearly butterfly approx.
#which mention "Mason Farm" as the location. 
#This will have to do for now. 

#The version of the data right before the cloudapp has certain data that fulldat1
#doesn't have, like family names and BAMONA links
#to rectify this, I've made a document with those things filled in
#to merge into fulldat1
#load document with links. This directory is Laura's but it's also in the Drive
speciesfacts<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/species.BAMONA.links.7.26.2021.csv")
fulldat1facts <- merge(fulldat1,speciesfacts,by="sciName")

#some columns have been doubled in the merge. let's keep the ones we need
fulldat1facts.vars <- c("sciName", "number", "date.observed", "observer",
                        "source", "year", "month", "day", "location", "family",
                        "subfamily_sci.y", "subfamily_common.y", "comName.y",
                        "BAMONA", "image")
fulldat1facts <- fulldat1facts[fulldat1facts.vars]

#rename the columns (to get rid of the "y"s)
names(fulldat1facts)<-c("sciName", "number", "date.observed", "observer",
             "source", "year", "month", "day", "location", "family",
             "subfamily_sci", "subfamily_common", "comName",
             "BAMONA", "image")

#load legrand/hamon data. This directory is Laura's but it's also in the Drive
legrand<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/legrand.hamon.ONLY.7.26.2021.csv")

#we need to convert the date in legrand to a...date
legrand$date.observed<-parse_date_time(legrand$date.observed,orders='mdy') 

#bind legrand and fulldat (qualtrics + ubr)
fulldat2 <- rbind(legrand, fulldat1facts)



##Some leftover questions
#is there overlap between ubr data and qualtrics data?
#there are observations (ex. Doxocopa laure) that seem straight-up unlikely
#converted Lethe anthedon -> Enodia anthedon
#converted Lethe appalachia -> Satyrodes appalachia 

#export data
write.csv(fulldat2, "C:/Users/lhamo/Documents/Biology/mf bflies 2017/myapp_legrand.kingsolver.mf.dat.6.3.2021.csv")

####################################################################################
################REFORMATTING TO ALIGN WITH CLOUDAPP FORMAT
####################################################################################



