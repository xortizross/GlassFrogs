
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
leveneTest(habitat$avg.leaf.area, habitat$habitat)
#P = 0.05382
#We fail to reject the null that there is a difference in population variance.
#However, it's a very close thing!