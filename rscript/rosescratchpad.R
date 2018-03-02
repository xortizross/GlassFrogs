##Habitat visualizations
habitat = read.csv("data/habitat_data.csv")
Pref=read.csv("data/pref-raw.csv")
str(habitat)
str(Pref)
table(unique(Pref$site))


#Create subsets
P1 = subset(habitat, site=="P1")
P13 = subset(habitat, site=="P13")
P15 = subset(habitat, site=="P15")
P16 = subset(habitat, site=="P16")
POT4 = subset(habitat, site=="POT4")
TIR = subset(habitat, site=="TIR")
P5 = subset(habitat, site=="P5")
secondary = subset(habitat, habitat=="secondary")
old.growth=subset(habitat, habitat=="old")
pasture=subset(habitat,habitat=="pasture")

#Isolate the vegetation
P1veg=table(P1$family)
P13veg=table(P13$family)
P15veg=table(P15$family)
P16veg=table(P16$family)
P5veg=table(P5$family)
TIRveg=table(TIR$family)
POT4veg=table(POT4$family)
secondaryveg = table(secondary$family)
oldveg = table(old.growth$family)
pastureveg = table(pasture$family)

#Create the barplots
par(mfrow=c(2,2))
barplot(P1veg[P1veg>0], las=2, main = "P1 plant families",cex.names=0.8)
barplot(P13veg[P13veg>0], las=2, main = "P13 plant families",cex.names=0.8)
barplot(P15veg[P15veg>0], las=2, main = "P15 plant families",cex.names=0.8)
barplot(P16veg[P16veg>0], las=2, main = "P16 plant families",cex.names=0.8)
barplot(P5veg[P5veg>0], las=2, main = "P5 plant families",cex.names=0.8)
barplot(TIRveg[TIRveg>0], las=2, main = "TIR plant families",cex.names=0.8)
barplot(POT4veg[POT4veg>0], las=2, main = "POT4 plant families",cex.names=0.8)
barplot(secondaryveg[secondaryveg>0], las=2, main = "Secondary growth plant families",cex.names=0.8)
barplot(oldveg[oldveg>0], las=2, main = "Old growth plant families",cex.names=0.8)
barplot(pastureveg[pastureveg>0], las=2, main = "Pasture plant families",cex.names=0.8)

#Count how many different families in each
library(plyr)
count.families=matrix(c(count(old.growth$family), count(secondary$family), count(pasture$family),
                        count(P1$family),count(P5$family), count(P13$family), count(P15$family), 
                        count(P16$family), count(TIR$family), count(POT4$family))); colnames(count.families)=c(count)
no.families=matrix(c(32,41,16,21,17,23,16,20,15,16));rownames(no.families)=c("old.growth","secondary","pasture","P1","P5","P13","P15","P16","TIR","POT4");
colnames(no.families)=c("count")
no.families
