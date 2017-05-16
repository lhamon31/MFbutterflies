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

#rename columns to R friendly format using plyr package
mf<-rename(mf, c("Swallowtails...Eastern.Tiger.Swallowtail..Papilio.glaucus....Number.Seen" = "Papilio.glaucus",
                 "Swallowtails...Spicebush.Swallowtail..Papilio.troilus....Number.Seen" = "Papilio.troilus",
                 "Swallowtails...Pipevine.Swallowtail..Battus.philenor....Number.Seen" = "Battus.philenor",
                 "Swallowtails...Black.Swallowtail..Papilio.polyxenes....Number.Seen" = "Papilio.polyxenes",
                 "Swallowtails...Zebra.Swallowtail..Eurytides.marcellus....Number.Seen" = "Eurytides.marcellus"))

mf<-rename(mf, c("Sulphurs...Cloudless.Sulphur..Phoebis.sennae..."="Phoebis.sennae",
                 "Sulphurs...Clouded.Sulphur.Colias.philodice..."="Colias.philodice",
                 "Sulphurs...Orange.Sulphur..Colias.eurythme..."="Colias.eurythme",
                 "Sulphurs...Sleepy.Orange..Abaeis.nicippe..."="Abaeis.nicippe",
                 "Sulphurs...Little.Yellow..Pyrisita.lisa..."="Pyrisita.lisa",
                 "Sulphurs...Unknown.Sulphur.."="Unknown.sulphur"))

mf<-rename(mf, c("Whites...Cabbage.White..Pieris.rapae..."="Pieris.rapae",
                 "Whites...Falcate.Orangetip..Anthocharis.midea..."="Anthocharis.midea",
                 "Whites...Checkered.White..Pontia.protodice..."="Pontia.protodice"))

mf<-rename(mf, c("Harvesters.and.Woolly.Legs...Harvester..Feniseca.tarquinis..."="Feniseca.tarquinis",
                 "Blues...Eastern.Tailed.blue..Cupido.comyntas..."="Cupido.comyntas",
                 "Blues...Summer.Azure..Celastrina.neglecta..."="Celastrina.neglecta",
                 "Blues...Spring.Azure..Celastrina.ladon..."="Celastrina.ladon",
                 "Blues...Unknown.Blue.."="Unknown blue",
                 "Hairstreaks...Grey.Hairstreak..Strymon.melinus..."="Strymon.melinus",
                 "Hairstreaks...Red.banded.Hairstreak..Calycopis.cecrops..."="Calycopis.cecrops",
                 "Hairstreaks...Juniper.Hairstreak..Mitoura.gryneus..."="Mitoura.gryneus",
                 "Hairstreaks...Henryâ..s.Elfin..Callophrys.henrici..."="Callophrys.henrici",
                 "Hairstreaks...Banded.Hairstreak..Satyrium.calanus..."="Satyrium.calanus",
                 "Hairstreaks...Great.Purple.Hairstreak..Atlides.halesus..."="Atlides.halesus",
                 "Hairstreaks...White.M.Hairstreak..Parrhasius.m.album..."="Parrhasius.m.album",
                 "Hairstreaks...Eastern.Pine.Elfin..Callophrys.niphon..."="Callophrys.niphon",
                 "Hairstreaks...Coral.Hairstreak..Satyrium.titus..."="Satyrium.titus",
                 "Hairstreaks...Oak.Hairstreak.Satyrium.favonus..."="Satyrium.favonus",
                 "Hairstreaks...Striped.Hairstreak..Satyrium.liparops..."="Satyrium.liparops"))

mf<-rename(mf, c("Emperors...Hackberry.Emperor..Asterocampa.celtis..."="Asterocampa.celtis",
                 "Emperors...Tawny.Emperor..Asterocampa.clyton..."="Asterocampa.clyton",
                 "Milkweed.Butterflies...Monarch..Danaus.plexippus..."="Danaus.plexippus",
                 "Heliconians.and.Fritillaries...Great.spangled.Fritillary..Speyeria.cybele..."="Speyeria.cybele",
                 "Heliconians.and.Fritillaries...Variegated.Fritillary..Euptoieta.claudia..."="Euptoieta.claudia",
                 "Heliconians.and.Fritillaries...Gulf.Fritillary..Agraulis.vanillae..."="Agraulis.vanillae",
                 "Snout.Butterflies...American.Snout..Libytheana.carinenta..."="Libytheana.carinenta",
                 "Admirals...Red.Spotted.Purple..Limenitis.artemis.astyanax..."="Limenitis.artemis.astyanax",
                 "Admirals...Viceroy..Limenitis.archippus..."="Limenitis.archippus"))

mf<-rename(mf, c("True.Brush.foots...Pearl.Crescent..Phyciodes.tharos..."="Phyciodes.tharos",
                 "True.Brush.foots...Common.Buckeye..Junonia.coenia..."="Junonia.coenia",
                 "True.Brush.foots...Red.Admiral..Vanessa.atalanta..."="Vanessa.atalanta",
                 "True.Brush.foots...Question.Mark..Polygonia.interrogationis..."="Polygonia.interrogationis",
                 "True.Brush.foots...Eastern.Comma..Polygonia.comma..."="Polygonia.comma",
                 "True.Brush.foots...Mourning.Cloak..Nymphalis.antiopa..."="Nymphalis.antiopa",
                 "True.Brush.foots...American.Lady..Vanessa.virginiensis..."="Vanessa.virginiensis",
                 "True.Brush.foots...Painted.Lady..Vanessa.cardui..."="Vanessa.cardui",
                 "True.Brush.foots...Silvery.Checkerspot..Chlosyne.nycteis..."="Chlosyne.nycteis"))

mf<-rename(mf, c("Satyrs...Carolina.Satyr..Hermeuptychia.sosybius..."="Hermeuptychia.sosybius",
                 "Satyrs...Gemmed.Satyr..Cyllopsis.gemma..."="Cyllopsis.gemma",
                 "Satyrs...Little.Wood.Satyr..Megisto.cymela..."="Megisto.cymela",
                 "Satyrs...Common.Wood.nymph..Cercyonis.pegala..."="Cercyonis.pegala",
                 "Satyrs...Northern.Pearly.eye..Lethe.anthedon..."="Lethe.anthedon",
                 "Satyrs...Appalachian.Brown..Lethe.appalachia..."="Lethe.appalachia",
                 "Satyrs...Unknown.Satyr.."="Unknown satyr"))

mf<-rename(mf, c("Grass.Skippers...Sachem..Atalopedes.campestris..."="Atalopedes.campestris",
                 "Grass.Skippers...Clouded.Skipper..Lerema.accius..."="Lerema.accius",
                 "Grass.Skippers...Zabulon.Skipper..Poanes.zabulon..."="Poanes.zabulon",
                 "Grass.Skippers...Crossline.Skipper..Polites.origenes..."="Polites.origenes",
                 "Grass.Skippers...Little.Glassywing..Pompeius.verna..."="Pompeius.verna",
                 "Grass.Skippers...Dun.Skipper..Euphyes.vestris..."="Euphyes.vestris",
                 "Grass.Skippers...Least.Skipper..Anclyoxypha.numitor..."="Anclyoxypha.numitor",
                 "Grass.Skippers...Southern.Broken.Dash..Wallengrenia.otho..."="Wallengrenia otho",
                 "Grass.Skippers...Fiery.Skipper..Hylephila.phyleus..."="Hylephila.phyleus",
                 "Grass.Skippers...Northern.Broken.Dash..Wallengrenia.egeremet..."="Wallengrenia.egeremet",
                 "Grass.Skippers...Ocola.Skipper..Panoquina.ocola..."="Panoquina.ocola",
                 "Grass.Skippers...Swarthy.Skipper..Nastra.lherminier..."="Nastra.lherminier",
                 "Grass.Skippers...Common.Roadside.Skipper..Amblyscirtes.vialis..."="Amblyscirtes.vialis",
                 "Grass.Skippers...Delaware.Skipper..Anatrytone.logan..."="Anatrytone.logan",
                 "Grass.Skippers...Dusted.Skipper..Atrytonopsis.hianna..."="Atrytonopsis.hianna",
                 "Grass.Skippers...Tawny.Edged.Skipper..Polites.themistocles..."="Polites.themistocles",
                 "Grass.Skippers...Dion.Skipper..Euphyes.dion..."="Euphyes.dion",
                 "Grass.Skippers...Unknown.Grass.Skipper.."="Unknown.grass.skipper"))


mf<-rename(mf, c("Spread.wing.Skippers...Silver.spotted.Skipper..Epargyreus.clarus..."="Epargyreus.clarus",
                 "Spread.wing.Skippers...Juvenalâ..s.Duskywing..Erynnis.juvenalis..."="Erynnis.juvenalis",
                 "Spread.wing.Skippers...Horaceâ..s.Duskywing..Erynnis.horatius..."="Erynnis.horatius",
                 "Spread.wing.Skippers...Northern.Cloudywing..Thorybes.pylades..."="Thorybes.pylades",
                 "Spread.wing.Skippers...Southern.Cloudywing..Thorybes.bathyllus..."="Thorybes.bathyllus",
                 "Spread.wing.Skippers...Common.Checkered.Skipper..Pyrgus.communis..."="Pyrgus.communis",
                 "Spread.wing.Skippers...Sleepy.Duskywing..Erynnis.brizo..."="Erynnis.brizo",
                 "Spread.wing.Skippers...Long.tailed.Skipper..Urbanus.proteus..."="Urbanus.proteus",
                 "Spread.wing.Skippers...Wild.Indigo.Duskywing..Erynnis.baptisiae..."="Erynnis.baptisiae",
                 "Spread.wing.Skippers...Zarucco.Duskywing..Erynnis.zarucco..."="Erynnis.zarucco",
                 "Spread.wing.Skippers...Unknown.Spread.wing.Skipper.."="Unknown.spreadwing.skipper",
                 "Unknown.Species.."="unk.sp","Other species..Fill.in.species.name...Other..Text."="other.sp.name",
                 "Other.species...Fill.in.species.name...Other.."="other.sp.num"))


mf<-rename(mf, c("Start.Date"="start.date",
                 "End.Date"="end.date",
                 "Response.Type"="response.type",
                 "IP.Address"="ip.address",
                 "Duration..in.seconds."="dur.sec",
                 "Recorded.Date"="recorded.date",
                 "Recipient.Last.Name"="last.name1",
                 "Recipient.First.Name"="first.name1",
                 "Location.Latitude"="location.lat",
                 "Location.Longitude"="location.long",
                 "Observer.name.s.."="observer.name",
                 "If.you.are.entering.these.observations.for.someone.else..what.is.your.name."="entering.name",
                 "Date.observed..MM.DD.YYYY."="date.observed",
                 "Temperature..F...optional."="tempf",
                 "Site.conditions..optional."="site.conditions",
                 "Start.time..ex..09.00AM...optional."="start.time",
                 "End.time..ex..12.00PM...optional."="end.time",
                 "How.would.you.rate.your.experience.level.in.identifying.butterflies."="self.rate"))

#hey here's another way
names(mf)<-c("start.date", "end.date", "response.type", "ip.address", 
                 "progress", "duration.sec","finished","recorded.date",
                 "response.id", "recipient.last.name", "recipient.first.name",
                 "recipient.email", "external.data.reference","location.lat",
                  "location.long", "distribution.channel", "observer.name",
                 "number.of.observers", "entry.name", "observation.date",
                 "temp", "conditions", 
                 "start", "end", "Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                 "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                 "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
                 "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
                 "Pontia.protodice", "Feniseca.tarquinis", "Cupido.comyntas",
                 "Celastrina.neglecta", "Celastrina.ladon","Unknown blue",
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
                 "Lethe.anthedon", "Lethe.appalachia", "Unknown satyr",
                 "Atalopedes.campestris", "Lerema.accius", "Poanes.zabulon",
                 "Polites.origenes", "Pompeius.verna", "Euphyes.vestris",
                 "Anclyoxypha.numitor", "Wallengrenia otho", "Hylephila.phyleus",
                 "Wallengrenia.egeremet", "Panoquina.ocola", "Nastra.lherminier",
                 "Amblyscirtes.vialis", "Anatrytone.logan", "Atrytonopsis.hianna",
                 "Polites.themistocles", "Euphyes.dion", "Unknown.grass.skipper",
                 "Epargyreus.clarus", "Erynnis.juvenalis", "Erynnis.horatius",
                 "Thorybes.pylades", "Thorybes.bathyllus", "Pyrgus.communis",
                 "Erynnis.brizo", "Urbanus.proteus", "Erynnis.baptisiae", 
                 "Erynnis.zarucco", "Unknown.spreadwing.skipper", "unknown.sp",
                 "other.sp.text.1", "other.sp.num.1","other.sp.text.2", "other.sp.num.2",
                 "other.sp.text.3", "other.sp.num.3","other.sp.text.4", "other.sp.num.4",
                 "other.sp.text.5", "other.sp.num.5","topics")

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

longdat<-reshape(alldat,
                 varying = c("Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                             "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                             "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
                            "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
                            "Pontia.protodice", "Feniseca.tarquinis", "Cupido.comyntas",
                            "Celastrina.neglecta", "Celastrina.ladon","Unknown blue",
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
                            "Lethe.anthedon", "Lethe.appalachia", "Unknown satyr",
                            "Atalopedes.campestris", "Lerema.accius", "Poanes.zabulon",
                            "Polites.origenes", "Pompeius.verna", "Euphyes.vestris",
                            "Anclyoxypha.numitor", "Wallengrenia otho", "Hylephila.phyleus",
                            "Wallengrenia.egeremet", "Panoquina.ocola", "Nastra.lherminier",
                            "Amblyscirtes.vialis", "Anatrytone.logan", "Atrytonopsis.hianna",
                            "Polites.themistocles", "Euphyes.dion", "Unknown.grass.skipper",
                            "Epargyreus.clarus", "Erynnis.juvenalis", "Erynnis.horatius",
                            "Thorybes.pylades", "Thorybes.bathyllus", "Pyrgus.communis",
                            "Erynnis.brizo", "Urbanus.proteus", "Erynnis.baptisiae", 
                            "Erynnis.zarucco", "Unknown.spreadwing.skipper", "unk.sp",
                            "other.sp.name", "other.sp.num"),
                 v.names="num.indv",
                 timevar="species",
                 times = c("Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
                           "Papilio.polyxenes", "Eurytides.marcellus", "Phoebis.sennae",  
                           "Colias.philodice", "Colias.eurythme", "Abaeis.nicippe",
                           "Pyrisita.lisa", "Unknown.sulphur", "Pieris.rapae", "Anthocharis.midea",
                           "Pontia.protodice", "Feniseca.tarquinis", "Cupido.comyntas",
                           "Celastrina.neglecta", "Celastrina.ladon","Unknown blue",
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
                           "Lethe.anthedon", "Lethe.appalachia", "Unknown satyr",
                           "Atalopedes.campestris", "Lerema.accius", "Poanes.zabulon",
                           "Polites.origenes", "Pompeius.verna", "Euphyes.vestris",
                           "Anclyoxypha.numitor", "Wallengrenia otho", "Hylephila.phyleus",
                           "Wallengrenia.egeremet", "Panoquina.ocola", "Nastra.lherminier",
                           "Amblyscirtes.vialis", "Anatrytone.logan", "Atrytonopsis.hianna",
                           "Polites.themistocles", "Euphyes.dion", "Unknown.grass.skipper",
                           "Epargyreus.clarus", "Erynnis.juvenalis", "Erynnis.horatius",
                           "Thorybes.pylades", "Thorybes.bathyllus", "Pyrgus.communis",
                           "Erynnis.brizo", "Urbanus.proteus", "Erynnis.baptisiae", 
                           "Erynnis.zarucco", "Unknown.spreadwing.skipper", "unk.sp"),
                 new.row.names=1:208,
                 direction="long") 

#write the cleaned data frame to a csv file
#row.names=FALSE prevents it from having an extra row with just numbers.
#spits out the csv in whatever folder the working directory is set.
#This is laura's working directory
write.csv(mf,file="C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.responses.cleaned.csv",row.names=FALSE)


