#### testing way to break apart EEMs sample name string
## DWH 26jul24

library(tidyverse)

####function to add undescore between depth and rep number

add_underscore <- function(x, position = 2) {
  # Length of the string
  n <- nchar(x)
  # Check if the length is greater than 2
  if (n > position) {
    # Insert underscore before the last two characters
    paste0(substr(x, 1, n - position), "_", substr(x, n - 1, n))
  } else {
    # Return the original string if it's too short
    x
  }
}


#### Read in all results files and make once csv for given fluorometer day


########### ADD Code here ############






####Make sample names

##trying to do eems renaming based on sample id
Site_code_number <- data.frame(Site_code = c("CS1", "CS2", "CP1", "CP2", "CC1", "CC2", "CC3", "CC4", "C50"),
                               Site_number = c(101, 100, 98, 96, 94, 92, 90, 88, 50))


eemsZ <- eems |> 
  select(Sample.Name) |> 
  mutate(Name = substr(Sample.Name, 10, nchar(Sample.Name)),   #get rid of yyyymmdd_ at start       
         Name = str_sub(Name, 1, str_length(Name)-4)) |> #remove .csv from end
  mutate(Name = sapply(Name, add_underscore)) |>  # add underscore between depth and rep
  separate(Name, into = paste0("part", 1:4), sep = "_", fill = "right")







