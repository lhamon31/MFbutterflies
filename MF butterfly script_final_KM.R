###MF Butterfly data manipulation###

setwd("~/Research Summer 2016")

alldat<-read.csv("Mason_Farm_Butterflies.csv")

#this is laura's directory
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.responses.5.16.2017.csv")

alldat<-as.data.frame(alldat)
library(plyr)

#remove unecessary columns
alldat$StartDate<-NULL
alldat$EndDate<-NULL
alldat$Status<-NULL
alldat$IPAddress<-NULL
alldat$Progress<-NULL
alldat$Duration..in.seconds.<-NULL
alldat$Finished<-NULL
alldat$RecordedDate<-NULL
alldat$ResponseId<-NULL
alldat$RecipientLastName<-NULL
alldat$RecipientFirstName<-NULL
alldat$RecipientEmail<-NULL
alldat$ExternalReference<-NULL
alldat$LocationLatitude<-NULL  
alldat$LocationLongitude<-NULL	
alldat$DistributionChannel<-NULL	
alldat$Q33<-NULL	
alldat$Q35<-NULL	
alldat$Q32<-NULL	

#remove row 1
alldat<-alldat[-1,]              
                 
##rename columns
names(alldat)<-c("Obsv.Name", "Date", "Temp", "Conditions", 
                 "Start", "End", "Papilio.glaucus", "Papilio.troilus", "Battus.philenor",
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
                 "other.sp.name", "other.sp.num","yep1","yep2","yep3","yep4","yep5",
                 "yep6", "yep7")

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
                             "other.sp.name", "other.sp.num","yep1","yep2","yep3","yep4","yep5",
                             "yep6", "yep7"),
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
                           "Erynnis.zarucco", "Unknown.spreadwing.skipper", "unk.sp",
                           "other.sp.name", "other.sp.num","yep1","yep2","yep3","yep4","yep5",
                           "yep6", "yep7"),
                 direction="long")   

#make subset of data excluding rows where num.indv=0
longdat2 <- longdat[!(longdat$num.indv == ""), ]
