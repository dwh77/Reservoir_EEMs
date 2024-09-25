#### Generate indivual absorbance csvs from CDOM all
## DWH 10jul24

# read in packages 
library(tidyverse)

#set wd to folder of run date
setwd("./Processed_Data/20240923_DHET")


#### read in data 
#first need to rename columns of txt file and save as csv
cdomall <- read.csv("./CDOM_all_23SEP24.csv")

run_date <- "20240923"


###Run for loop to make csvs

for(i in colnames(cdomall)[2:14]) {
  
  # print(i)
  df <- cdomall |> 
    select(1, i) 

   write.table(df, file = paste0(getwd(), "/abs_", run_date, "_", i, ".csv" ),
               sep = ",", row.names = F, col.names = F) 
  
}

