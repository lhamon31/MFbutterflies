###MF Butterfly data manipulation###

setwd("~/Research Summer 2016")

alldat<-read.csv("Mason_Farm_Butterflies.csv")

alldat<-as.data.frame(alldat)
library(plyr)

#remove unecessary columns
alldat$ï..V1<-NULL
alldat$V2<-NULL
alldat$V3<-NULL
alldat$V4<-NULL
alldat$V5<-NULL
alldat$V6<-NULL
alldat$V7<-NULL
alldat$V8<-NULL
alldat$V9<-NULL
alldat$V10<-NULL
alldat$Q10<-NULL
alldat$Q27_1_TEXT<-NULL
alldat$Q27_2_1_TEXT<-NULL
alldat$Q27_2_TEXT<-NULL  
alldat$Q27_3_1_TEXT<-NULL	
alldat$Q27_3_TEXT<-NULL	
alldat$Q27_4_1_TEXT<-NULL	
alldat$Q27_4_TEXT<-NULL	
alldat$Q27_5_1_TEXT<-NULL	
alldat$Q27_5_TEXT<-NULL
alldat$LocationLatitude<-NULL
alldat$LocationLongitude<-NULL
alldat$LocationAccuracy<-NULL
alldat$X<-NULL

#remove row 1
alldat<-alldat[-1,]              
                 
##rename columns
names(alldat)<-c("Obsv.Name", "Date", "Temp", "Conditions", 
                 "Start", "End", 
                 "Papilio glaucus", "Papilio troilus", 
                 "Battus philenor","Papilio polyxenes", 
                 "Eurytides marcellus", "Papilio cresphontes", 
                 "Papilio palamedes", "Asterocampa celtis",
                 "Asterocampa clyton", "Danaus plexippus",
                 "Danaus gilippus", "Speyeria cybele", 
                 "Euptoieta claudia", "Argraulis vanillae", 
                 "Heliconius charithonia", "Libytheana carinenta",
                 "Limenitis artemis astyanax", "Limenitis archippus",
                 "Phyciodes tharos","Junonia coenia", 
                 "Vanessa atalanta","Polygonia interrogationis", 
                 "Polygonia comma", "Nymphalis antiopa", 
                 "Vanessa virginiensis", "Chlosyne nycteis",
                 "Vanessa cardui", "Hermeuptychia sosybius",
                 "Cyllopsis gemma", "Megisto cymela", 
                 "Cercyonis pegala", "Lethe anthedon",
                 "Lethe appalachia", "Lethe portlandia", 
                 "Lethe creola","Feniseca tarquinis", 
                 "Cupido comyntas", "Celastrina neglecta",
                 "Celastrina ladon", "Strymon melinus", 
                 "Calycopsis cecrops", "Mitoura gryneus", 
                 "Callophrys henrici", "Callophrys augustinus", 
                 "Satyrium calanus", "Atlides halseus", 
                 "Parrhasius m-album","Callophrys niphon",
                 "Satyrium titus", "Satyrium favonus", 
                 "Satyrium liparops", "Phoebis sennae", 
                 "Colias philodice", "Colias eurytheme",
                 "Abaeis nicippe", "Pyrisita lisa", 
                 "Nathalis iole", "Eurema daira", 
                 "Zerene cesonia", "Pieris rapae", 
                 "Anthocaris midea","Pontia protodice", 
                 "Atalopedes campestris", "Lerema accius",
                 "Poanes zabulon", "Polites origenes", 
                 "Pompeius verna","Euphyes vestris",
                 "Anclyoxypha numitor","Wallengrenia otho",
                 "Hylephila phyleus", "Wallengrenia egeremet", 
                 "Panoquina ocola","Nastra lherminier",
                 "Amblyscirtes vialis", "Anatrytone logan", 
                 "Atrytonopsis hianna","Polites themistocles",
                 "Euphyes dion", "Amblyscirtes hegon", 
                 "Lerodea eufala", "Hesperia metea",
                 "Poanes hobomok","Calpodes ethlius",
                 "Hesperia leonardus","Ambylscirtes carolina", 
                 "Amblyscirtes aesculapius", "Epargyreus clarus",
                 "Erynnis juvenalis","Erynnis horatius",
                 "Thorybes pylades","Thorybes bathyllus",
                 "Pyrgus communis","Erynnis brizo",
                 "Urbanus proteus","Erynnis baptisiae",
                 "Erynnis zarucco","Staphylus hayhurstii",
                 "Pholisora catallus","Thorybes confusis",
                 "Autochton cellus","Urbanus dorantes",
                 "Erynnis martialis","Unknown")

##reshape from wide to long 
# might want to change the number of new.row.names based on how many rows (responses)
#   there are in the data when you download it from Qualtrics.

longdat<-reshape(alldat,
                 varying = c("Papilio glaucus", "Papilio troilus", 
                             "Battus philenor","Papilio polyxenes", 
                             "Eurytides marcellus", "Papilio cresphontes", 
                             "Papilio palamedes", "Asterocampa celtis",
                             "Asterocampa clyton", "Danaus plexippus",
                             "Danaus gilippus", "Speyeria cybele", 
                             "Euptoieta claudia", "Argraulis vanillae", 
                             "Heliconius charithonia", "Libytheana carinenta",
                             "Limenitis artemis astyanax", "Limenitis archippus",
                             "Phyciodes tharos","Junonia coenia", 
                             "Vanessa atalanta","Polygonia interrogationis", 
                             "Polygonia comma", "Nymphalis antiopa", 
                             "Vanessa virginiensis", "Chlosyne nycteis",
                             "Vanessa cardui", "Hermeuptychia sosybius",
                             "Cyllopsis gemma", "Megisto cymela", 
                             "Cercyonis pegala", "Lethe anthedon",
                             "Lethe appalachia", "Lethe portlandia", 
                             "Lethe creola","Feniseca tarquinis", 
                             "Cupido comyntas", "Celastrina neglecta",
                             "Celastrina ladon", "Strymon melinus", 
                             "Calycopsis cecrops", "Mitoura gryneus", 
                             "Callophrys henrici", "Callophrys augustinus", 
                             "Satyrium calanus", "Atlides halseus", 
                             "Parrhasius m-album","Callophrys niphon",
                             "Satyrium titus", "Satyrium favonus", 
                             "Satyrium liparops", "Phoebis sennae", 
                             "Colias philodice", "Colias eurytheme",
                             "Abaeis nicippe", "Pyrisita lisa", 
                             "Nathalis iole", "Eurema daira", 
                             "Zerene cesonia", "Pieris rapae", 
                             "Anthocaris midea","Pontia protodice", 
                             "Atalopedes campestris", "Lerema accius",
                             "Poanes zabulon", "Polites origenes", 
                             "Pompeius verna","Euphyes vestris",
                             "Anclyoxypha numitor","Wallengrenia otho",
                             "Hylephila phyleus", "Wallengrenia egeremet", 
                             "Panoquina ocola","Nastra lherminier",
                             "Amblyscirtes vialis", "Anatrytone logan", 
                             "Atrytonopsis hianna","Polites themistocles",
                             "Euphyes dion", "Amblyscirtes hegon", 
                             "Lerodea eufala", "Hesperia metea",
                             "Poanes hobomok","Calpodes ethlius",
                             "Hesperia leonardus","Ambylscirtes carolina", 
                             "Amblyscirtes aesculapius", "Epargyreus clarus",
                             "Erynnis juvenalis","Erynnis horatius",
                             "Thorybes pylades","Thorybes bathyllus",
                             "Pyrgus communis","Erynnis brizo",
                             "Urbanus proteus","Erynnis baptisiae",
                             "Erynnis zarucco","Staphylus hayhurstii",
                             "Pholisora catallus","Thorybes confusis",
                             "Autochton cellus","Urbanus dorantes",
                             "Erynnis martialis","Unknown" ),
                 v.names="num.indv",
                 timevar="species",
                 times = c("Papilio glaucus", "Papilio troilus", 
                           "Battus philenor","Papilio polyxenes", 
                           "Eurytides marcellus", "Papilio cresphontes", 
                           "Papilio palamedes", "Asterocampa celtis",
                           "Asterocampa clyton", "Danaus plexippus",
                           "Danaus gilippus", "Speyeria cybele", 
                           "Euptoieta claudia", "Argraulis vanillae", 
                           "Heliconius charithonia", "Libytheana carinenta",
                           "Limenitis artemis astyanax", "Limenitis archippus",
                           "Phyciodes tharos","Junonia coenia", 
                           "Vanessa atalanta","Polygonia interrogationis", 
                           "Polygonia comma", "Nymphalis antiopa", 
                           "Vanessa virginiensis", "Chlosyne nycteis",
                           "Vanessa cardui", "Hermeuptychia sosybius",
                           "Cyllopsis gemma", "Megisto cymela", 
                           "Cercyonis pegala", "Lethe anthedon",
                           "Lethe appalachia", "Lethe portlandia", 
                           "Lethe creola","Feniseca tarquinis", 
                           "Cupido comyntas", "Celastrina neglecta",
                           "Celastrina ladon", "Strymon melinus", 
                           "Calycopsis cecrops", "Mitoura gryneus", 
                           "Callophrys henrici", "Callophrys augustinus", 
                           "Satyrium calanus", "Atlides halseus", 
                           "Parrhasius m-album","Callophrys niphon",
                           "Satyrium titus", "Satyrium favonus", 
                           "Satyrium liparops", "Phoebis sennae", 
                           "Colias philodice", "Colias eurytheme",
                           "Abaeis nicippe", "Pyrisita lisa", 
                           "Nathalis iole", "Eurema daira", 
                           "Zerene cesonia", "Pieris rapae", 
                           "Anthocaris midea","Pontia protodice", 
                           "Atalopedes campestris", "Lerema accius",
                           "Poanes zabulon", "Polites origenes", 
                           "Pompeius verna","Euphyes vestris",
                           "Anclyoxypha numitor","Wallengrenia otho",
                           "Hylephila phyleus", "Wallengrenia egeremet", 
                           "Panoquina ocola","Nastra lherminier",
                           "Amblyscirtes vialis", "Anatrytone logan", 
                           "Atrytonopsis hianna","Polites themistocles",
                           "Euphyes dion", "Amblyscirtes hegon", 
                           "Lerodea eufala", "Hesperia metea",
                           "Poanes hobomok","Calpodes ethlius",
                           "Hesperia leonardus","Ambylscirtes carolina", 
                           "Amblyscirtes aesculapius", "Epargyreus clarus",
                           "Erynnis juvenalis","Erynnis horatius",
                           "Thorybes pylades","Thorybes bathyllus",
                           "Pyrgus communis","Erynnis brizo",
                           "Urbanus proteus","Erynnis baptisiae",
                           "Erynnis zarucco","Staphylus hayhurstii",
                           "Pholisora catallus","Thorybes confusis",
                           "Autochton cellus","Urbanus dorantes",
                           "Erynnis martialis","Unknown"),
                 new.row.names=1:208,
                 direction="long")   

#make subset of data excluding rows where num.indv=0
longdat2 <- longdat[!(longdat$num.indv == ""), ]
