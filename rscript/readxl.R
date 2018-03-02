#### Uploading excel workbooks with multiple sheets at once ####

# More on readxl: http://readxl.tidyverse.org
# A recommended workflow: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html

# uploading excel workbook (yes, with multiple sheets!)

library(readxl) 

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("data/Stream_13Sep17.xlsx")