#script for comparing our mf sightings vs. those seen at the county level
#asking a few questions
#do we see things in different proportions vs. orange county as a whole?
#which plants could be used to encourage diff. species?

#load up hlg data
ncdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/approximation_thru_2016.csv")
#subset orange county data
orangedat<-subset(ncdat, county=="Orange")

#load up cleaned qualtrics data
mfdat<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.with.ubr.6.21.2017.cleaned.csv")
#replace NA in "number of individuals" with 1
mfdat$num.indv[is.na(mfdat$num.indv)] <- 1
#ideally get this to go for every spp for every year
#oc
#create an empty dataframe
species=unique(orangedat$species)
year=2015:2016
oc.output<-data.frame(species=character(),
                      number=integer(),
                      proportion=integer())

#generate for loop
for (s in species) { 
  oc.spp<-subset(orangedat,year=2016,species==s)
  oc.others <- subset(orangedat, year=2016, !species==s)
  proportion<-sum(oc.spp$number)/(sum(oc.spp$number)+sum(oc.others$number))
  propoutput<-data.frame(species=s, number=sum(oc.spp$number), prop=proportion)
  oc.output = rbind(oc.output,propoutput)
}

#mf
#create an empty dataframe
species=unique(mfdat$species)
year=2015:2016
mf.output<-data.frame(species=character(),
                      number=integer(),
                      proportion=integer())

#generate for loop
for (s in species) { 
  mf.spp<-subset(mfdat,year=2016,species==s)
  mf.others <- subset(mfdat, year=2016, !species==s)
  proportion<-sum(mf.spp$num.indv)/(sum(mf.spp$num.indv)+sum(mf.others$num.indv))
  propoutput<-data.frame(species=s, number=sum(mf.spp$num.indv), prop=proportion)
  mf.output = rbind(mf.output,propoutput)
}

#combine
#you gotta make the species names the same first 
oc.output<-data.frame(lapply(oc.output, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
})) #makes all characters lowercase
oc.output$species<-gsub(" ", ".", oc.output$species) #replaces space with "." 

#combine oc dat and mf dat
compare.dat<-merge(mf.output,oc.output,by="species")
colnames(compare.dat)<-c("species","mf.num","mf.prop","oc.num","oc.prop")
compare.dat$prop.difference<-(compare.dat$mf.prop-compare.dat$oc.prop)#makes a column for the diff. b/w proportions

#and then compare how diff. the two are
#comparison of 2 proportions. parametric?
z.prop = function(x1,x2,n1,n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1+x2) / (n1+n2)
  denominator = sqrt(p.common * (1-p.common) * (1/n1 + 1/n2))
  z.prop.ris = numerator / denominator
  return(z.prop.ris)
}
compare.dat$z<-z.prop(compare.dat$mf.prop, compare.dat$oc.prop, compare.dat$mf.num, compare.dat$oc.num)

#calculate p-values
compare.dat$p<-2*pnorm(-abs(compare.dat$z)) #2-sided?

#write csv
write.csv(compare.dat,file="C:/Users/lhamo/Documents/Biology/mf bflies 2017/compare.oc.mf.csv",row.names=FALSE)
