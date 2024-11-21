#### Making EEMs name and Abs correction per run date
## DWH 26jul24

library(tidyverse)
library(readxl)

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


#### Read in all EEM results files and make once csv for given fluorometer day ----

list.files(path = "./Processed_Data/20241118_DHET", pattern = "results*", full.names = T)

results <- list.files(path = "./Processed_Data/20241118_DHET", pattern = "results*", full.names = T) |> 
  base::lapply(read_xls, sheet = 'EEM Results') |> 
  bind_rows() |> 
  rename(SampleName = 1)


####Make sample names ----

#assign site numbers to label codes
Site_code_number <- data.frame(Site_code = c("CS1", "CS2", "CP1", "CP2", "CC1", "CC2", "CC3", "CC4", "C50"),
                               Site_number = c(101, 100, 98, 96, 94, 92, 90, 88, 50))


#many string mutation to make df w/ just sample info
names <- results |> 
  select(SampleName) |> 
  mutate(Name = substr(SampleName, 10, nchar(SampleName)),   #get rid of yyyymmdd_ at start       
         Name = str_sub(Name, 1, str_length(Name)-4)) |> #remove .csv from end
  mutate(Name = sapply(Name, add_underscore)) |>  # add underscore between depth and rep
  separate(Name, into = paste0("part", 1:4), sep = "_", fill = "right") |> #break Name into four columns
  rename(Site_code = part1,
         Date = part2,
         Depth_m = part3,
         Rep = part4) |>  #give columns meaningful names
  mutate(Rep = ifelse(Rep %in% c("r1", "R1"), 1, 2)) |> #update rep values 
  mutate(Depth_m = ifelse(Depth_m == "01", "0.1", Depth_m),
         Depth_m = ifelse(Depth_m == "15", "1.5", Depth_m)) |>  #update depths that are easy, will assign BOT manually later
  mutate(Date = dmy(Date),
         Reservoir = "CCR",
         Dilution = 1) |> 
  left_join(Site_code_number, by = "Site_code") |> 
  select(Reservoir, Site_code, Site_number, Depth_m, Date, Rep, Dilution, SampleName)

#bring sample names to data
eems_names <- left_join(names, results, by = c("SampleName"))


#### Bring in Abs values ----
run_date <- "20241118"
abs <- read.csv("./Processed_Data/20241118_DHET/CDOMall_20241118.csv")

#works to get a254 and a350
abs_forbind <- abs |> 
  rename(wavelength = 1,
         MilliQ = 2) |> 
  filter(wavelength %in% c(254, 350)) |> 
  pivot_longer(-(1:2), names_to = "Sample", values_to = "Abs_raw") |> 
  mutate(naparian_abs = (2.303 * (Abs_raw - MilliQ)) / 0.01 ) |>
  pivot_longer(-c(1:3)) |> 
  select(-MilliQ) |> 
  mutate(name_new = ifelse(name == "Abs_raw" & wavelength == 254, "A254", NA),
         name_new = ifelse(name == "naparian_abs" & wavelength == 254, "a254", name_new),
         name_new = ifelse(name == "Abs_raw" & wavelength == 350, "A350", name_new),
         name_new = ifelse(name == "naparian_abs" & wavelength == 350, "a350", name_new)) |> 
  select(Sample, name_new, value) |> 
  pivot_wider(names_from = name_new, values_from = value) |> 
  mutate(SampleName = paste0(run_date, "_", Sample, ".csv", sep = "")) |> 
  select(-Sample)


#### Bind eems names to abs and export 
export <- left_join(eems_names, abs_forbind, by = "SampleName")


# FIX FILE NAMES!!!!!!!!!!!!!!!!!!
#write.csv(export, "./Processed_Data/20241118_DHET/Results_EEMs_Abs_20241118.csv", row.names = F)













