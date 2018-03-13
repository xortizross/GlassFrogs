
#pref_xl<-read_excel("data/Pref_data_13Sep17.xlsx",sheet='data') %>% ## I NEED TO CHANGE THIS!
  #write_csv('data/pref-raw.csv')

Pref<-read_csv('data/pref-raw.csv')

Pref<-subset(Pref,day.night=='N')

Pref<-subset(Pref,species!='UK')

Pref<-Pref[c('ID','habitat','site','transect','species','leaf area')]

### abundance / site

abund.site <- table(Pref$site)
chit<-chisq.test(abund.site)
chit$expected
chit$observed

# more than expected: P1, P5, TIR
# less than expected: P13, P15, P16, POT4

### abundance / habitat
abund.habitat <- table(Pref$habitat)
chit.ah<-chisq.test(abund.habitat)
chit.ah$expected
chit.ah$observed

# old more than expected, pasture less than expected, secondary as expected

### abundance / species / habitat

abund.habitat.sp <- table(Pref$habitat, Pref$species)
chit.ahs<-chisq.test(abund.habitat.sp)
chit.ahs$expected
chit.ahs$observed

##### abundance / frog species / site ####

Pref_t<-table(Pref$species, Pref$site)

chit<-chisq.test(Pref_t)
chit$expected

#Pref$habitat<-if

#######How much does leaf area vary by site?——Levene's test#######
library(car)
hist(habitat$avg.leaf.area)
#Trim habtiat to reflect the sites we're actually working with
habitat = subset(habitat, site=="P1"|site=="P5"|site=="TIR"|site=="P13"|site=="P15"|site=="P16"|site=="POT4")
leveneTest(habitat$avg.leaf.area, habitat$site)
#P = 0.0948
#We fail to reject the null that there is difference in population variance but this is fairly close.
#How strong is this test compared to our data?

#####How much does leaf area vary by habitat?####
#P = 0.05382
#We fail to reject the null that there is a difference in population variance.
#However, it's a very close thing!

####Do frogs prefer certain leaf areas, given the available area at each site?###

HP1boot=NULL
HP13boot=NULL
HP15boot=NULL
HP16boot=NULL
HP5boot=NULL
HPOT4boot=NULL

EP1boot=NULL
EP13boot=NULL
EP15boot=NULL
EP16boot=NULL
EP5boot=NULL
EPOT4boot=NULL

TP1boot=NULL
TP13boot=NULL
TP15boot=NULL
TP16boot=NULL
TP5boot=NULL
TPOT4boot=NULL
#Replace relevant fields in code for each series of bootstrapping
corrData <- unlist(P5[,c("leaf.area1", "leaf.area2", "leaf.area3")], use.names = FALSE)
long = length(pref_raw$site == "P5"&pref_raw$species=="ESPPRO")

for (i in c(1:9999)) {
  samp=sample(corrData,size=length(which(pref_raw$site=="P5"&pref_raw$species=="ESPPRO")),replace=T)
  EP5boot[i]=samp
}

#Write CSVs
write.csv(HP1boot, "data/HP1boot.csv");write.csv(HP13boot, "data/HP13boot.csv");write.csv(HP15boot, "data/HP15boot.csv");write.csv(HP16boot, "data/HP16boot.csv");write.csv(HP5boot, "data/HP5boot.csv");write.csv(HPOT4boot, "data/HPOT4boot.csv")
write.csv(EP1boot, "data/EP1boot.csv");write.csv(EP13boot, "data/EP13boot.csv");write.csv(EP15boot, "data/EP15boot.csv"); write.csv(EP16boot, "data/EP16boot.csv");write.csv(EP5boot, "data/EP5boot.csv");write.csv(EPOT4boot, "data/EPOT4boot.csv")
write.csv(TP1boot, "data/TP1boot.csv"); write.csv(TP13boot, "data/TP13boot.csv"); write.csv(TP15boot, "data/TP15boot.csv"); write.csv(TP16boot, "data/TP16boot.csv"); write.csv(TP5boot, "data/TP5boot.csv"); write.csv(TPOT4boot, "data/TPOT4boot.csv")

###Create histograms###
par(mfrow=c(2,3))
hist(log(HP1boot)); hist(log(HP13boot));hist(log(HP15boot));hist(log(HP5boot))
hist(log(EP1boot)); hist(log(EP16boot));hist(log(EP5boot));hist(log(EPOT4boot))
hist(log(TP1boot)); hist(log(TP13boot));hist(log(TP15boot));hist(log(TP16boot));hist(log(TP5boot));hist(log(TPOT4boot))
