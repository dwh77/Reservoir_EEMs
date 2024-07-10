#### Generate indivual absorbance csvs from CDOM all
## DWH 10jul24

# read in packages 
library(tidyverse)

#set wd to folder of run date
setwd("./Processed_Data/20240708_DH")

#### read in data 
cdomall <- read.csv("./20240708_CDOMall.csv")

run_date <- "20240708"


###Run for loop to make csvs

for(i in colnames(cdomall)[2:17]) {
  
  # print(i)
  df <- cdomall |> 
    select(1, i) 

   write.table(df, file = paste0(getwd(), "/abs_", run_date, "_", i, ".csv" ),
               sep = ",", row.names = F, col.names = F) 
  
}

