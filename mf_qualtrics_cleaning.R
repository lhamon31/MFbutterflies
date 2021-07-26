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
mf<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.responses.12.12.2018.csv")

#Get rid of the first two rows, which has nonsense import data and extraneous variable names
mf<-mf[-1:-2,] 

#rename columns so that they're easier to work with
names(mf)<-c("start.date", "end.date", "status", "ip.address", 
             "progress", "duration.in.seconds","finished","recorded.date",
             "response.id", "recipient.last.name", "recipient.first.name",
             "recipient.email", "external.data.reference","latitude",
             "longitude", "distribution.channel", "user.language", "observerName",
             "numberObservers", "dataEntererName", "dateObserved",
             "temperature", "siteConditions", 
             "startTime", "endTime", "experienceLevel", "Papilio.glaucus", "Papilio.troilus", 
             "Battus.philenor",
             "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
             "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
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

#Get rid of the start date, end date columns 
#These detail how long it takes for a person to do the survey, and are not necessary
mf <- mf[ -c(1:2) ]

#Get rid of test surveys entered with fake names
mf<-mf[ ! mf$observerName %in% c("Test 1","test 2","Garth McBoatman","foo", "Garth McBoatface","yo","test","Beck in 1995","Beck","beck","d"), ]

#remove completely unknown species columns for now
#we should keep them
#but i'm not sure how to reconcile them
mf<-mf[-c(109:119)]

#recorded.date refers to when the data was entered into the form. 
#since it is not necessary, it is removed here.
mf$recorded.date<-NULL

#Removes Lorem ipsum
#finds rows that have more than 10 characters in the date column and deletes them
mf<-mf[!nchar(as.character(mf$dateObserved))>10,]

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

#Eliminate non-date values for this last one. yeah I know there's a quicker way
mf$dateObserved<-parse_date_time(mf$dateObserved,orders='mdy') 

#creates a column with just the year of observation
mf$year<-year(mf$dateObserved)

#creates a column with only the month
#can do as a number
#or use the argument 'label=TRUE'
#and it will give back the character month name
mf$month<-month(mf$dateObserved,label = TRUE)  

##reshape from wide to long 
# might want to change the number of new.row.names based on how many rows (responses)
#   there are in the data when you download it from Qualtrics.
longdat<-reshape(mf,
                 varying = c("Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                             "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                             "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
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
                             "Erynnis.zarucco", "Unknown.spreadwing.skipper"),
                 v.names="num.indv",
                 timevar="species",
                 times = c("Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                           "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                           "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
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
                           "Erynnis.zarucco", "Unknown.spreadwing.skipper"),
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
#(also figure out what to do about unknown spp in previously dropped columns)
longdat <- longdat[ -c(1:14) ]

####################################################################################
################ADDING THE UBR DATA TO THE QUALTRICS DATA
################UBR DATA INCLUDES DATA COLLECTED BY KATI MOORE USING THE UBR APP
####################################################################################

#adding in the ubr dat
#load UBR files
ubr.folder <- "C:/Users/lhamo/Documents/Biology/mf bflies 2017/ubr.data/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=ubr.folder, pattern="*.csv") # create list of all .csv files in folder
ubr.dat <- do.call("rbind", lapply(file_list, function(x)read.csv(paste(ubr.folder, x, sep=''), 
                                                                  stringsAsFactors = FALSE)))
#add columns to longdat that exist in ubr.dat
newcolumnnames1 <- c("subfamilyCommon", "speciesCommon", "subfamilyScientific", "familyCommon",
                    "familyScientific", "windSpeed", "windDirection", "cloudCover", "behavior",
                    "gender", "comment", "transect", "photoName", "pressure", "ambientTemperature", 
                    "illuminance", "relativeHumidity", "wingLength", "markFound", "markAdded",
                    "locationAccuracy", "point", "certainty", "voucherID", "voucherType", 
                    "voucherLocation", "dataSource", "timeOfSighting", "latitude", "longitude",
                    "condition")
longdat[ , newcolumnnames1] <- NA

#add columns to ubr.dat that exist in longdat
newcolumnnames2 <- c("status", "ipAddress", "progress", "durationSeconds", "finished", 
                     "recipientLastName", "recipientFirstName", "recipientEmail", 
                     "externalDataReference", "distributionChannel", "userLanguage", 
                     "observerName", "numberObservers", "dataEntererName", "experienceLevel",
                     "obs1", "obs2", "obs3", "obs1Initials", "obs2Initials", "obs3Initials", 
                     "dataSource", "submissionDate", "ipLat", "ipLong", "siteConditions", "startTime",
                     "endTime")
ubr.dat[ , newcolumnnames2] <- NA

#rename columns longdat to match (may not be necessary)
names(longdat)<-c("observerName", "numberObservers","dataEntererName",  "dateObserved", 
                  "temperature", "siteConditions", "startTime", "endTime",        
                 "experienceLevel", "topics", "obs1", "obs2",               
                  "obs3", "obs1Initials", "obs2Initials", "obs3Initials",            
                  "year", "month", "species", "numberIndividuals",          
                  "id", "subfamilyCommon", "speciesCommon", "subfamilyScientific",
                  "familyCommon", "familyScientific", "windSpeed", "windDirection",      
                  "cloudCover", "behavior", "gender", "comment",            
                  "transect", "photoName", "pressure", "ambientTemperature", 
                  "illuminance", "relativeHumidity", "wingLength", "markFound",          
                  "markAdded", "locationAccuracy", "point", "certainty",          
                  "voucherID", "voucherType", "voucherLocation", "dataSource",         
                  "timeOfSighting", "latitude", "longitude", "condition")
      
#rename ubr.dat to match
names(ubr.dat)<-c("responseID","surveyID","speciesCommon", "speciesScientific",
                  "subfamilyCommon","subfamilyScientific","familyCommon","familyScientific",
                  "numberIndividuals","temperature","windSpeed","windDirection","cloudCover",
                  "dateObserved","timeOfSighting","behavior","gender","condition","comment",
                  "transect","photoName","pressure", "temperature","illuminance",
                  "relativeHumidity","wingLength","markFound","markAdded","latitude","longitude",
                  "locationTime.date","locationTime.time", "locationAccuracy","point",
                  "certainty","voucherID","voucherType","voucherLocation", "status", "ipAddress", "progress", "durationSeconds", "finished", 
                  "recipientLastName", "recipientFirstName", "recipientEmail", 
                  "externalDataReference", "distributionChannel", "userLanguage", 
                  "observerName", "numberObservers", "dataEntererName", "experienceLevel",
                  "obs1", "obs2", "obs3", "obs1Initials", "obs2Initials", "obs3Initials", 
                  "dataSource", "submissionDate", "ipLat", "ipLong", "siteConditions", "startTime",
                  "endTime")

#drop irrelevant columns from ubr.dat
drops <- c("locationTime.date","locationTime.time")
ubr.dat <- ubr.dat[ , !(names(ubr.dat) %in% drops)]

#make data types uniform
ubr.dat$numberIndividuals <- factor(ubr.dat$numberIndividuals)
  longdat$numberIndividuals <-factor(longdat$numberIndividuals)
ubr.dat$temperature <- factor(ubr.dat$temperature)
  longdat$temperature <-factor(longdat$temperature)
ubr.dat$dateObserved <- as.Date(ubr.dat$dateObserved, "%Y-%m-%d")
ubr.dat$illuminance <- factor(ubr.dat$illuminance)
ubr.dat$latitude <- factor(ubr.dat$latitude)
ubr.dat$longitude <- factor(ubr.dat$longitude)
ubr.dat$locationAccuracy <- factor(ubr.dat$locationAccuracy)

#also modify dataSource to designate qualtrics and ubr data as such
longdat$dataSource <- "qualtrics"
ubr.dat$dataSource <- "ubr"

#Kati is the observer for every observation in the UBR dat. Let's go ahead and add this
ubr.dat$observerName <- "Kati Moore"
ubr.dat$obs1 <- "Kati Moore"
ubr.dat$obs1Initials <- "KM"

#stack the data sets
library(dplyr)
fulldat<-bind_rows(list(longdat,ubr.dat), .id=NULL)

#make species in ubr up-to-date
fulldat$speciesScientific<- gsub("Eurema nicippe", "Abaeis nicippe", fulldat$speciesScientific)
fulldat$speciesScientific<- gsub("Eurema lisa", "Pyrisita lisa", fulldat$speciesScientific)
fulldat$speciesScientific<- gsub("Limenitis arthemis", "Limenitis arthemis astyanax", fulldat$speciesScientific)
fulldat$speciesScientific<- gsub("Everes comyntas", "Cupido comyntas", fulldat$speciesScientific)
fulldat$speciesScientific<- gsub("ancyloxpha numitor", "Ancyloxypha numitor", fulldat$speciesScientific)
fulldat$speciesScientific<- gsub("enodia anthedon", "Lethe anthedon", fulldat$speciesScientific)

#make all characters in species names lowercase in both datasets
fulldat$speciesScientific <- tolower(fulldat$speciesScientific)

#replace space w/ period in ubr data
fulldat$speciesScientific<-gsub(" ", ".", fulldat$speciesScientific) 

####################################################################################
################ADDING THE 2015 HAMON DATA
################THIS INCLUDES DATA COLLECTED BY LAURA IN 2015, BEFORE THE ONLINE FORM WAS MADE
####################################################################################

#add in 2015 data
#again, this is based on laura's working directory. change as needed.
dat2015<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/2015.hamon.longdat.csv") 

#rename columns to match
colnames(dat2015)<-c("dateObserved","speciesScientific","numberIndividuals","observerName")

#convert date to appropriate date format
dat2015$dateObserved <- as.Date(dat2015$dateObserved, "%Y-%m-%d")

#add column indicating the data source
dat2015$dataSource <- rep("2015 observations", length(92))

#format numberIndividuals as factor
dat2015$numberIndividuals <- factor(dat2015$numberIndividuals)
  
#combine with UBR data and qualtrics data, generated above and stored under fulldat
fulldat2<-bind_rows(list(fulldat,dat2015), .id=NULL)

#remove extraneous NA columns generated at some point
fulldat2<-fulldat2[,-57:-84] 

####check for date similarities (i.e. dates from Kati's ubr data that were also entered in qualtrics)
#subset each
UBRdat <- fulldat2[ which(fulldat2$dataSource=='ubr'), ]
qualtdat <- fulldat2[ which(fulldat2$dataSource=='qualtrics'), ]

#find unique names
ubrDates<-unique(UBRdat$dateObserved)
qualtDates<-unique(qualtdat$dateObserved)

#eliminate dates in ubr dat which overlap with qualt dat
fulldat3<-fulldat2[fulldat2$dataSource != "ubr" & fulldat2$dateObserved != "2017-07-16", ] 
fulldat3<-fulldat2[fulldat2$dataSource !="ubr" & fulldat2$dateObserved !="2017-07-16 UTC",]
fulldat4<-fulldat3[fulldat3$dataSource !="ubr" & fulldat3$dateObserved !="2017-07-23",]
fulldat5<-fulldat4[fulldat4$dataSource !="ubr" & fulldat4$dateObserved !="2017-07-30",]

####################################################################################
################REFORMATTING TO ALIGN WITH CLOUDAPP FORMAT
####################################################################################



####################################################################################
################FUTURE: ADDING IN LEGRAND DATA?
####################################################################################
