
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

#####Do frogs prefer certain leaf areas, given the available area at each site?####

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

###Create objects for actual
P1obs = subset(pref_raw, site=="P1")
P13obs = subset(pref_raw, site=="P13")
P15obs = subset(pref_raw, site=="P15")
P16obs = subset(pref_raw, site=="P16")
POT4obs = subset(pref_raw, site=="POT4")
TIRobs = subset(pref_raw, site=="TIR")
P5obs = subset(pref_raw, site=="P5")
HP1obs = unlist(subset(P1obs, species=="HYAVAL", select='leaf area'), use.names=FALSE)
HP13obs = unlist(subset(P13obs, species=="HYAVAL", select='leaf area'), use.names = FALSE)
HP15obs = unlist(subset(P15obs, species=="HYAVAL", select='leaf area'), use.names = FALSE)
HP16obs = unlist(subset(P16obs, species=="HYAVAL", select='leaf area'), use.names = FALSE)
HP5obs = unlist(subset(P5obs, species=="HYAVAL", select='leaf area'), use.names = FALSE)
HPOT4obs = unlist(subset(POT4obs, species=="HYAVAL", select='leaf area'), use.names = FALSE)
HTIRobs = unlist(subset(TIRobs, species=="HYAVAL", select='leaf area'), use.names = FALSE)
EP1obs = unlist(subset(P1obs, species=="ESPPRO", select='leaf area'), use.names = FALSE)
EP13obs = unlist(subset(P13obs, species=="ESPPRO", select='leaf area'), use.names = FALSE)
EP15obs = unlist(subset(P15obs, species=="ESPPRO", select='leaf area'), use.names = FALSE)
EP16obs = unlist(subset(P16obs, species=="ESPPRO", select='leaf area'), use.names = FALSE)
EP5obs = unlist(subset(P5obs, species=="ESPPRO", select='leaf area'), use.names = FALSE)
EPOT4obs = unlist(subset(POT4obs, species=="ESPPRO", select='leaf area'), use.names = FALSE)
ETIRobs = unlist(subset(TIRobs, species=="ESPPRO", select='leaf area'), use.names = FALSE)
TP1obs = unlist(subset(P1obs, species=="TERSPI", select='leaf area'), use.names = FALSE)
TP13obs = unlist(subset(P13obs, species=="TERSPI", select='leaf area'), use.names = FALSE)
TP15obs = unlist(subset(P15obs, species=="TERSPI", select='leaf area'), use.names = FALSE)
TP16obs = unlist(subset(P16obs, species=="TERSPI", select='leaf area'), use.names = FALSE)
TP5obs = unlist(subset(P5obs, species=="TERSPI", select='leaf area'), use.names = FALSE)
TPOT4obs = unlist(subset(POT4obs, species=="TERSPI", select='leaf area'), use.names = FALSE)
TTIRobs = unlist(subset(TIRobs, species=="TERSPI", select='leaf area'), use.names = FALSE)

##Write means for observations##
obsmean = matrix(c(mean(log(HP1obs), na.rm = TRUE), mean(log(HP13obs), na.rm = TRUE), 0,
                   0, mean(log(HP5obs), na.rm = TRUE), mean(log(HPOT4obs), na.rm = TRUE),
                   0,mean(log(EP1obs), na.rm = TRUE), 0, 0,
                   mean(log(EP16obs), na.rm = TRUE), mean(log(EP5obs), na.rm = TRUE), 0,
                   mean(log(ETIRobs), na.rm = TRUE), mean(log(TP1obs), na.rm = TRUE), mean(log(TP13obs), na.rm = TRUE), mean(log(TP15obs), na.rm = TRUE),
                   mean(log(TP16obs), na.rm = TRUE), mean(log(TP5obs), na.rm = TRUE), mean(log(TPOT4obs), na.rm = TRUE),
                   mean(log(TTIRobs), na.rm = TRUE)), ncol = 3)
dimnames(obsmean)= list(c("P1", "P13", "P15", "P16", "P5", "POT4", "TIR"), c("HYAVAL", "ESPPRO", "TERSPI"))
##Graph results
par(mfrow=c(2,2), ps = 20, mar=c(5,6,4,1)+.1)
hist(log(HP1boot), xlim = c(1,8), xlab = "", main = "P1: HYAVAL bootstrap", col = "lightblue1"); arrows(obsmean[1,1], 750, obsmean[1,1], 0, lwd = 2, length = 0.1)
hist(log(HP13boot), xlim=c(0,9), xlab = "", main = "P13: HYAVAL bootstrap", col = "lightblue1"); arrows(obsmean[2,1], 750, obsmean[2,1], 0, lwd = 2, length = 0.1)
hist(log(HP5boot), xlim=c(0,9), xlab = "Leaf area (log)", main = "P5: HYAVAL bootstrap", col = "lightblue1"); arrows(obsmean[5,1], 750, obsmean[5,1], 0, lwd = 2, length = 0.1)
hist(log(HPOT4boot), xlim=c(0,9), xlab = "Leaf area (log)", main = "P13: HYAVAL bootstrap", col = "lightblue1"); arrows(obsmean[6,1], 750, obsmean[6,1], 0, lwd = 2, length = 0.1)
par(mfrow=c(2,2), ps = 20, mar=c(5,6,4,1)+.1)
hist(log(EP1boot), xlim = c(1,8), xlab = "", main = "P1: ESPPRO bootstrap", col = "palegreen3");arrows(obsmean[1,2], 750, obsmean[1,2], 0, lwd = 2, length = 0.1)
hist(log(EP16boot), xlim = c(1,8), xlab = "", main = "P16: ESPPRO bootstrap", col = "palegreen3");arrows(obsmean[4,2], 750, obsmean[4,2], 0, lwd = 2, length = 0.1)
hist(log(EP5boot), xlim = c(0,8), xlab = "Leaf area (log)", main = "P5: ESPPRO bootstrap", col = "palegreen3");arrows(obsmean[4,2], 750, obsmean[4,2], 0, lwd = 2, length = 0.1)
hist(log(ETIRboot), xlim = c(1,8), xlab = "Leaf area (log)", main = "TIR: ESPPRO bootstrap", col = "palegreen3");arrows(obsmean[7,2], 750, obsmean[7,2], 0, lwd = 2, length = 0.1)
par(mfrow=c(2,3), ps = 20, mar=c(5,6,4,1)+.1)
hist(log(TP1boot), xlab = "", main = "P1: TERSPI bootstrap", col = "lightblue1", xlim = c(0,9));arrows(obsmean[1,3], 750, obsmean[1,3], 0, lwd = 2, length = 0.1)
hist(log(TP13boot), xlab = "", main = "P13: TERSPI bootstrap", col = "lightblue1", xlim = c(0,10));arrows(obsmean[2,3], 750, obsmean[2,3], 0, lwd = 2, length = 0.1)
hist(log(TP15boot), xlab = "", main = "P15: TERSPI bootstrap", col = "lightblue1", xlim=c(0,9));arrows(obsmean[3,3], 750, obsmean[3,3], 0, lwd = 2, length = 0.1)
hist(log(TP16boot), xlab = "", main = "P16: TERSPI bootstrap", col = "palegreen3", xlim=c(1,9));arrows(obsmean[4,3], 750, obsmean[4,3], 0, lwd = 2, length = 0.1)
hist(log(TP5boot), xlab = "Leaf area (log)", main = "P5: TERSPI bootstrap", col = "palegreen3",xlim=(c(0,9)));arrows(obsmean[5,3], 750, obsmean[5,3], 0, lwd = 2, length = 0.1)
hist(log(TPOT4boot), xlab = "Leaf area (log)", main = "POT4: TERSPI bootstrap", col = "salmon1", xlim=c(0,9)); arrows(obsmean[6,3], 750, obsmean[6,3], 0, lwd = 2, length = 0.1)
hist(log(TTIRboot), xlab = "Leaf area (log)", main = "TIR: TERSPI bootstrap", col = "salmon1", xlim=c(0,9)); arrows(obsmean[7,3], 750, obsmean[7,3], 0, lwd = 2, length = 0.1)
##Test potentially significant graphs##
sortHP1boot = sort(log(HP1boot))
sortHP1boot[9749:9999]
###HP1 is ranked at 111
sortHP13boot = sort(log(HP13boot))
sortHP13boot[9002:9252]
##HP13 not significant
sortHP5boot = sort(log(HP5boot))
sortHP5boot[9550:9800]
##HP5 not significant
sortHPOT4boot=sort(log(HPOT4boot))
sortHPOT4boot[9002:9252]
##HPOT4 not significant


#######Descriptive statistics#####
##Leaf area##
par(mfrow = c(1,1), ps = 20, cex = 1.5)
hist(corrData, main = "Leaf area histogram", xlab = "Leaf area (cm2)", col = "darkcyan")
hist(log(corrData), main = "Transformed leaf area histogram", xlab = "Leaf area (log)", col = "darkcyan")
##HYAVAL P1 example##
hist(log(HP1boot), xlim = c(1,8), xlab = "", main = "P1: HYAVAL bootstrap", col = "palegreen3"); arrows(obsmean[1,1], 750, obsmean[1,1], 0, lwd = 4, length = 0.4)
##HYAVAL P5 example##
hist(log(HP5boot), xlim=c(0,9), xlab = "Leaf area (log)", main = "P5: HYAVAL bootstrap", col = "salmon1"); arrows(obsmean[5,1], 750, obsmean[5,1], 0, lwd = 4, length = 0.4)

