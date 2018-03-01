habitat = read.csv("data/habitat_data.csv")
str(habitat)
P1 = subset(habitat, site=="P1")
hist(P1$family)
P1veg=table(P1$family)
barplot(P1veg[P1veg>0])
str(P1veg)

##Visualize vegetation