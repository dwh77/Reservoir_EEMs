#### Generate indivual absorbance csvs from CDOM all
## DWH 10jul24

# read in packages 
library(tidyverse)

#set wd to folder of run date
setwd("./Processed_Data/20240823_DH")


#### read in data 
#first need to rename columns of txt file and save as csv
cdomall <- read.csv("./20240823_CDOMall.csv")

run_date <- "20240823"


###Run for loop to make csvs

for(i in colnames(cdomall)[2:15]) {
  
  # print(i)
  df <- cdomall |> 
    select(1, i) 

   write.table(df, file = paste0(getwd(), "/abs_", run_date, "_", i, ".csv" ),
               sep = ",", row.names = F, col.names = F) 
  
}

