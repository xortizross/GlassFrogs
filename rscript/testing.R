library(tidyverse)

#pref_xl<-read_excel("data/Pref_data_13Sep17.xlsx",sheet='data') %>% ## I NEED TO CHANGE THIS!
  #write_csv('data/pref-raw.csv')

Pref<-read_csv('data/pref-raw.csv')

Pref<-subset(Pref,day.night=='N')

Pref<-subset(Pref,species!='UK')

Pref<-Pref[c('ID','habitat','site','transect','species','leaf area')]

library(readxl) 

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

stream <- read_excel_allsheets("data/Stream_13Sep17.xlsx")

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

##### abundance / frog species / site #### CAN'T

Pref_t<-table(Pref$species[Pref$species!='ESPPRO'], Pref$site)
Pref_t.noesppro <- table(Pref$species[Pref$species!='ESPPRO'], Pref$site[Pref$species!='ESPPRO'])
Pref_t.nopasture<- table(Pref$species[Pref$site!='POT4'], Pref$site[Pref$site!='POT4'])

chit<-chisq.test(Pref_t)
chit$expected

chit.noesppro <- chisq.test(Pref_t.noesppro)
chit.noesppro$expected
chit.noesppro$observed

chit.nopasture <- chisq.test(Pref_t.nopasture)
chit.nopasture$expected
chit.nopasture$observed

  # I want to run a goodness of fit against a poisson distribution

#### Mean selected leaf area / site ####

hist(Pref$`leaf area`)
hist(log(Pref$`leaf area`)) # much better

tapply(Pref$`leaf area`, Pref$site, mean, na.rm=T)
tapply(log(Pref$`leaf area`), Pref$site, mean, na.rm=T)

anova.ls<- aov(log(Pref$`leaf area`)~Pref$site)
summary(anova.ls)

TukeyHSD(anova.ls)

anova.lh<- aov(log(Pref$`leaf area`)~Pref$habitat)
summary(anova.lh)

#### Mean selectted leaf area / species ####

anova.lsp<- aov(log(Pref$`leaf area`)~Pref$species)
summary(anova.lsp)

TukeyHSD(anova.lsp)

par(mfrow=c(3,1))
hist(log(Pref$`leaf area`[Pref$species=="HYAVAL"]),xlim=c(3,10),ylim=c(0,25))
hist(log(Pref$`leaf area`[Pref$species=="ESPPRO"]),xlim=c(3,10),ylim=c(0,25))
hist(log(Pref$`leaf area`[Pref$species=="TERSPI"]),xlim=c(3,10),ylim=c(0,25))

mean.leaf.sp<-tapply(log(Pref$`leaf area`), Pref$species, mean, na.rm=T)

par(mfrow=c(1,1))

barplot(mean.leaf.sp)

#### visualizing stream ####

library(dbplyr)
stream %>% goupby(site)
P1 <- as.data.frame(stream$P1)
P1$site<- paste('P1')
P5 <- as.data.frame(stream$P5)
P5$site <- paste('P5')
TIR <- as.data.frame(stream$TIR)
TIR$site<-paste('TIR')
A <- rbind(P1,P5,TIR)

P13<-as.data.frame(stream$P13)
P13$site<-paste('P13')
P15<-as.data.frame(stream$P15)
P15$site<-paste('P15')
POTDL<-as.data.frame(stream$POTDL)
POTDL$site<-paste('POTDL')
P16<-as.data.frame(stream$P16)
P16<-P16[-9]
P16$site<-paste('P16')
B <- rbind(P13,P15,P16,POTDL)

hist(A)
