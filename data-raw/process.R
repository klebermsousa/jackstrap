# data-raw/process.R
# Data import and processing pipeline


library(readr)
#library(readxl)

##municipalities <- read_csv("data-raw/municipalites.csv")
#demographics <- read_excel("data-raw/Demographics.xlsx")

##data_jackstrap <- load("~/Documents/Salários, Remuneração e Incentivos/DifSalpub/jackstrapr/jackstrap/data-raw/municipalities.RData")

# Data processing code here...

# This should be the last line
usethis::use_data(municipalities, overwrite = T)
