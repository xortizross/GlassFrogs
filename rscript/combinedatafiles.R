#First, we need a new package
install.packages("readbulk")
#Make sure all files are saved as CSV and in the same folder.
#Run read_bulk
all.habitat=read_bulk("/Users/Rose/Documents/GlassFrogs/habitat_data")
write.table(all.habitat, "/Users/Rose/Documents/GlassFrogs/habitat_data.csv", sep=",")
