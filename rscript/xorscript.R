#### Making my pref data set ####

Pref2<-read.csv('data/Pref2.csv',h=T)

#Subset imported preference data frame to only contain night surveys

pref<-subset(Pref2,Day.Night=='N')

# add a Site.T column that unites site and transect just in case

pref$site.t<-paste(pref$Site, pref$Transect, sep='.')

#### Uploading excel workbooks with multiple sheets at once ####

# uploading excel workbook (yes, with multiple sheets!)

library(readxl) 

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("data/Stream_13Sep17.xlsx")

# A Recommended Workflow

# From: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
# More on readxl: http://readxl.tidyverse.org

library(tidyverse)
library(readxl)

# upload an excel sheet and immediately save a .csv version
pref_xl<-read_excel("data/Pref_data_13Sep17.xlsx",sheet='data') %>%
  write_csv('data/pref-raw.csv')

# double checking that it is imported
head(pref_xl)

# did we actually write a csv copy?
upload_check<-read.csv('data/pref-raw.csv')
head(upload_check)

# have R check it
all.equal(pref_xl,upload_check) # column names are different but I think the rest is the same

# Now with a workbook with multiple sheets
path <- 'data/Stream_13Sep17.xlsx'

path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

read_then_csv <- function(sheet, path) {
  pathbase <- path %>%
    basename() %>%
    tools::file_path_sans_ext()
  path %>%
    read_excel(sheet = sheet) %>% 
    write_csv(paste0(pathbase, "-", sheet, ".csv"))
}

path %>%
  excel_sheets() %>%
  set_names() %>% 
  map(read_then_csv, path = path)

streams <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map_df(~ read_excel(path = path, sheet = .x), .id = "sheet")
print(streams, n = Inf)
