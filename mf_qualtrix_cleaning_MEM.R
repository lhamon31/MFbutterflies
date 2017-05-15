##MF Qualtrix cleaning script
#by Elizabeth Moore

#this is a test change

#load necessary packages
library(readr)
library(plyr)
library(stringr)
library(tidyr)
library(lubridate)

#load data
#before loading data, delete the first row of the qualtrix csv.
mf<-read.csv("~/Mason Farm/Data sets/qualtrix uned.csv")
mf<-mf[-1,]
view(mf)

##cleaning data
#Gets rid of the first row, which has nonsense import data
#(ask elizabeth if this is the same as the aformentioned row deletion)
mf<-mf[-c(1),]

#rename columns to R friendly format using plyr package
mf<-rename(mf, c("Swallowtails - Eastern Tiger Swallowtail (Papilio glaucus) - Number Seen"="Pap.glaucus",
                 "Swallowtails - Spicebush Swallowtail (Papilio troilus) - Number Seen"="Pap.troilus",
                 "Swallowtails - Pipevine Swallowtail (Battus philenor) - Number Seen"="Bat.philenor",
                 "Swallowtails - Black Swallowtail (Papilio polyxenes) - Number Seen"="Pap.polyxenes",
                 "Swallowtails - Zebra Swallowtail (Eurytides marcellus) - Number Seen"="Eur.marcellus"))

mf<-rename(mf, c("Sulphurs - Cloudless Sulphur (Phoebis sennae) -"="Pho.sennae",
                 "Sulphurs - Clouded Sulphur(Colia philodice) -"="Col.philodice",
                 "Sulphurs - Orange Sulphur (Colias eurythme) -"="Col.eurythme",
                 "Sulphurs - Sleepy Orange (Abaeis nicippe) -"="Aba.nicippe",
                 "Sulphurs - Little Yellow (Pyrisita lisa) -"="Pyr.lisa",
                 "Sulphurs - Unknown Sulphur -"="unk.sulphur"))

mf<-rename(mf, c("Whites - Cabbage White (Pieris rapae) -"="Pie.rapae",
                 "Whites - Falcate Orangetip (Anthocaris midea) -"="Ant.midea",
                 "Whites - Checkered White (Pontia protodice) -"="Pon.protodice"))

mf<-rename(mf, c("Harvesters and Woolly Legs - Harvester (Feniseca tarquinis) -"="Fen.tarquinis",
                 "Blues - Eastern Tailed-blue (Cupido comyntas) -"="Cup.comyntas",
                 "Blues - Summer Azure (Celastrina neglecta) -"="Cel.neglecta",
                 "Blues - Spring Azure (Celastrina ladon) -"="Cel.ladon",
                 "Blues - Unknown Blue -"="unk.blue",
                 "Hairstreaks - Grey Hairstreak (Strymon melinus) -"="Sty.melinus",
                 "Hairstreaks - Red-banded Hairstreak (Calycopsis cecrops) -"="Cal.cecrops",
                 "Hairstreaks - Juniper Hairstreak (Mitoura gryneus) -"="Mit.gryneus",
                 "Hairstreaks - Henrys Elfin (Callophrys henrici) -"="Cal.henrici",
                 "Hairstreaks - Banded Hairstreak (Satyrium calanus) -"="Sat.calanus",
                 "Hairstreaks - Great Purple Hairstreak (Atlides halesus) -"="Atl.halesus",
                 "Hairstreaks - White-M Hairstreak (Parrhasius m-album) -"="Par.m-album",
                 "Hairstreaks - Eastern Pine Elfin (Callphrys niphon) -"="Cal.niphon",
                 "Hairstreaks - Coral Hairstreak (Satyrium titus) -"="Sat.titus",
                 "Hairstreaks - Oak Hairstreak(Satyrium favonus) -"="Sat.favonus",
                 "Hairstreaks - Striped Hairstreak (Satyrium liparops) -"="Sat.liparops"))

mf<-rename(mf, c("Emperors - Hackberry Emperor (Asterocampa celtis) -"="Ast.celtis",
                 "Emperors - Tawny Emperor (Asterocampa clyton) -"="Ast.clyton",
                 "Milkweed Butterflies - Monarch (Danaus plexippus) -"="Dan.plexippus",
                 "Heliconians and Fritillaries - Great-spangled Fritillary (Speyeria cybele) -"="Spe.cybele",
                 "Heliconians and Fritillaries - Variegated Fritillary (Euptoieta claudia) -"="Eup.claudia",
                 "Heliconians and Fritillaries - Gulf Fritillary (Agraulis vanillae) -"="Agr.vanillae",
                 "Snout Butterflies - American Snout (Libytheana carinenta) -"="Lib.carinenta",
                 "Admirals - Red-Spotted Purple (Limenitis artemis astyanax) -"="Lim.artemis.ast",
                 "Admirals - Viceroy (Limenitis archippus) -"="Lim.archippus"))

mf<-rename(mf, c("True Brush-foots - Pearl Crescent (Phyciodes tharos) -"="Phy.tharos",
                 "True Brush-foots - Common Buckeye (Junonia coenia) -"="Jun.coenia",
                 "True Brush-foots - Red Admiral (Vanessa atalanta) -"="Van.atalanta",
                 "True Brush-foots - Question Mark (Polygonia interrogationis) -"="Pol.interrogationis",
                 "True Brush-foots - Eastern Comma (Polygonia comma) -"="Pol.comma",
                 "True Brush-foots - Mourning Cloak (Nymphalis antiopa) -"="Nym.antiopa",
                 "True Brush-foots - American Lady (Vanessa virginiensis) -"="Van.virginiensis",
                 "True Brush-foots - Painted Lady (Vanessa cardui) -"="Van.cardui",
                 "True Brush-foots - Silvery Checkerspot (Chlosyne nycteis) -"="Chl.nycteis"))

mf<-rename(mf, c("Satyrs - Carolina Satyr (Hermeuptychia sosybius) -"="Her.sosybius",
                 "Satyrs - Gemmed Satyr (Cyllopsis gemma) -"="Cyl.gemma",
                 "Satyrs - Little Wood-Satyr (Megisto cymela) -"="Meg.cymela",
                 "Satyrs - Common Wood-nymph (Cercyonis pegala) -"="Cer.pegala",
                 "Satyrs - Northern Pearly-eye (Lethe anthedon) -"="Let.anthedon",
                 "Satyrs - Appalachian Brown (Lethe appalachia) -"="Let.appalachia",
                 "Satyrs - Unknown Satyr -"="unk.satyr"))

mf<-rename(mf, c("Grass Skippers - Sachem (Atalopedes campestris) -"="Ata.campestris",
                 "Grass Skippers - Clouded Skipper (Lerema accius) -"="Ler.accius",
                 "Grass Skippers - Zabulon Skipper (Poanes zabulon) -"="Poa.zabulon",
                 "Grass Skippers - Crossline Skipper (Polites origenes) -"="Pol.origenes",
                 "Grass Skippers - Little Glassywing (Pompeius verna) -"="Pom.verna",
                 "Grass Skippers - Dun Skipper (Euphyes vestris) -"="Eup.vestris",
                 "Grass Skippers - Least Skipper (Anclyoxypha numitor) -"="Anc.numitor",
                 "Grass Skippers - Southern Broken-Dash (Wallengrenia otho) -"="Wal.otho",
                 "Grass Skippers - Fiery Skipper (Hylephila phyleus) -"="Hyl.phyleus",
                 "Grass Skippers - Northern Broken-Dash (Wallengrenia egeremet) -"="Wal.egeremet",
                 "Grass Skippers - Ocola Skipper (Panoquina ocola) -"="Pan.ocola",
                 "Grass Skippers - Swarthy Skipper (Nastra lherminier) -"="Nas.iherminier",
                 "Grass Skippers - Common Roadside-Skipper (Amblyscirtes vialis) -"="Amb.vialis",
                 "Grass Skippers - Delaware Skipper (Anatrytone logan) -"="Ana.logan",
                 "Grass Skippers - Dusted Skipper (Atrytonopsis hianna) -"="Atr.hianna",
                 "Grass Skippers - Tawny-Edged Skipper (Polites themistocles) -"="Pol.themistocles",
                 "Grass Skippers - Dion Skipper (Euphyes dion) -"="Eup.dion",
                 "Grass Skippers - Unknown Grass Skipper -"="unk.grass.skipper"))


mf<-rename(mf, c("Spread-wing Skippers - Silver-spotted Skipper (Epargyreus clarus) -"="Epa.clarus",
                 "Spread-wing Skippers - Juvenals Duskywing (Erynnis juvenalis) -"="Ery.juvenalis",
                 "Spread-wing Skippers - Horaces Duskywing (Erynnis horatius) -"="Ery.horatius",
                 "Spread-wing Skippers - Northern Cloudywing (Thorybes pylades) -"="Tho.pylades",
                 "Spread-wing Skippers - Southern Cloudywing (Thorybes bathyllus) -"="Tho.bathyllus",
                 "Spread-wing Skippers - Common Checkered-Skipper (Pyrgus communis) -"="Pyr.communis",
                 "Spread-wing Skippers - Sleepy Duskywing (Erynnis brizo) -"="Ery.brizo",
                 "Spread-wing Skippers - Long-tailed Skipper (Urbanus proteus) -"="Urb.proteus",
                 "Spread-wing Skippers - Wild Indigo Duskywing (Erynnis baptisiae) -"="Ery.baptisiae",
                 "Spread-wing Skippers - Zarucco Duskywing (Erynnis zarucco) -"="Ery.zarucco",
                 "Spread-wing Skippers - Unknown Spread-wing Skipper -"="unk.spreadwing.skipper",
                 "Unknown Species -"="unk.sp","Other species ( Fill in species name) - Other - Text"="other.sp.name",
                 "Other species ( Fill in species name) - Other -"="other.sp.num"))


mf<-rename(mf, c("Start Date"="start.date",
                 "End Date"="end.date",
                 "Response Type"="resp.type",
                 "IP Address"="ip.add",
                 "Duration (in seconds)"="dur.sec",
                 "Recorded Date"="rec.date",
                 "Recipient Last Name"="last.name1",
                 "Recipient First Name"="first.name1",
                 "Location Latitude"="loc.lat",
                 "Location Longitude"="loc.long",
                 "Observer name(s):"="obs.name",
                 "If you are entering these observations for someone else, what is your name?"="enter.name",
                 "Date observed (MM/DD/YYYY)"="date.obs",
                 "Temperature (F) (optional)"="tempf",
                 "Site conditions (optional)"="site.cond",
                 "Start time (ex. 09:00AM) (optional)"="start.time",
                 "End time (ex. 12:00PM) (optional)"="end.time",
                 "How would you rate your experience level in identifying butterflies?"="self.rate"))

##gets rid of weird creepy Latin
#finds rows that have more than 10 characters and deletes them
#MEM set it to look for this in one of the butterfly count columns
#since it's not likely we would see a billion butterflies
mf<-mf[!nchar(as.character(mf$Pap.glaucus))>10,]

#splits up observer names and puts into observer 1, observer 2 etc:
#gsub replaces the first argument with the second
#makes everything uniform in syntax so that str_split_fixed can split the columns.
#In this case MEM used "and", and had to finagle the gsubs for the specific syntax of each entry.
#can be altered depending on syntax of column.
mf$obs.name<- gsub("&", " and",mf$obs.name)
mf$obs.name<- gsub(", and", ",",mf$obs.name)
mf$obs.name<- gsub(",", " and",mf$obs.name)

obs<-str_split_fixed(mf$obs.name, c("and"), 3)

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
mf$rec.date<-parse_date_time(mf$rec.date,orders='mdy_HM',tz="America/New_York")
mf$date.obs<-parse_date_time(mf$date.obs,orders='mdy') 

#creates a column with just the year of observation
mf$year<-year(mf$date.obs)
mf$year

#creates a column with only the month
#can do as a number
#or use the argument 'label=TRUE'
#and it will give back the character month name
mf$month<-month(mf$date.obs,label = TRUE)  
mf$month

#write the cleaned data frame to a csv file
#row.names=FALSE prevents it from having an extra row with just numbers.
#spits out the csv in whatever folder the working directory is set.
write.csv(mf, "mf.clean.csv", row.names = FALSE)

