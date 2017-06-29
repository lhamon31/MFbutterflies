#script for comparing our mf sightings vs. those seen at the county level
#asking a few questions
#do we see things in different proportions vs. orange county as a whole?
#which plants could be used to encourage diff. species?

#load up data
    #hlg
    ncdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/approximation_thru_2016.csv")
      #subset orange county data
      orangedat<-subset(ncdat, county=="Orange")

    #cleaned mf 
    mfdat<-read.csv("C:/Users/lhamo/Documents/Biology/mf bflies 2017/qualtrics.data/qualtrics.with.ubr.cleaned.csv")

#subset appropriate columns for orangedat
orangedat<-orangedat[,c("species","number","year")]

#change variable formatting of orangedat to match mfdat
orangedat$species<-gsub(" ", ".", orangedat$species) #replaces space with "."
orangedat<-data.frame(lapply(orangedat, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
})) #makes all characters lowercase

#subset 2016 data for each
orangedat<-subset(orangedat,year==2016)
mfdat<-subset(mfdat,year==2016)

#note repeated species due to misspellings in mfdat. I'm gonna reconcile this
mfdat$species[mfdat$species == "ancyloxpha.numitor"] <- "ancyloxypha.numitor"
mfdat$species[mfdat$species == "limenitis.artemis.astyanax"] <- "limenitis.arthemis.astyanax"
  #i'm also gonna remove that "unknown" and vague, unhelpful "papilio"
  mfdat<-mfdat[ which( ! mfdat$species %in% "unknown") , ]
  mfdat<-mfdat[ which( ! mfdat$species %in% "papilio") , ] 
  
#find total number of obs. for each spp for that year
orangedat<-aggregate(number ~ species, data=orangedat, FUN=sum)
mfdat<-aggregate(number ~ species, data=mfdat, FUN=sum)

#combine orangedat and mfdat
compare.dat<-merge(mfdat,orangedat,by="species")
colnames(compare.dat)<-c("species","mf.num","oc.num")


#generate proportions for each row
#ideally get this to go for every spp for every year
  #oc
      #create an empty dataframe
      species=unique(orangedat$species)
      oc.output<-data.frame(species=character(),
                            number=integer(),
                            proportion=integer())

      #generate for loop
      for (s in species) { 
        oc.spp<-subset(orangedat,species==s)
        oc.others <- subset(orangedat,!species==s)
        proportion<-sum(oc.spp$number)/(sum(oc.spp$number)+sum(oc.others$number))
        propoutput<-data.frame(species=s, number=sum(oc.spp$number), prop=proportion)
        oc.output = rbind(oc.output,propoutput)
      }

  #mf
      #create an empty dataframe
      species=unique(mfdat$species)
      mf.output<-data.frame(species=character(),
                            number=integer(),
                            proportion=integer())

      #generate for loop
      for (s in species) { 
      mf.spp<-subset(mfdat,species==s)
      mf.others <- subset(mfdat,!species==s)
      proportion<-sum(mf.spp$number)/(sum(mf.spp$number)+sum(mf.others$number))
      propoutput<-data.frame(species=s, number=sum(mf.spp$number), prop=proportion)
      mf.output = rbind(mf.output,propoutput)
    }

#combine orangedat and mfdat
compare.dat<-merge(mf.output,oc.output,by="species")
colnames(compare.dat)<-c("species","mf.num","mf.prop","oc.num","oc.prop")
      
#makes a column for the diff. b/w proportions
compare.dat$prop.difference<-(compare.dat$mf.prop-compare.dat$oc.prop)

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

t.test(compare.dat$mf.prop, compare.dat$oc.prop, alternative=c("two.sided"), mu=0, paired=FALSE, var.equal=FALSE, conf.level=0.95)

#write csv
write.csv(compare.dat,file="C:/Users/lhamo/Documents/Biology/mf bflies 2017/compare.oc.mf.csv",row.names=FALSE)
