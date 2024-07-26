### testing way to break apart EEMs sample name string 

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

##trying to do eems renaming based on sample id
Site_code_number <- data.frame(Site_code = c("CS1", "CS2", "CP1", "CP2", "CC1", "CC2", "CC3", "CC4", "C50"),
                               Site_number = c(101, 100, 98, 96, 94, 92, 90, 88, 50))


eemsZ <- eems |> 
  select(Sample.Name) |> 
  mutate(Name = substr(Sample.Name, 10, nchar(Sample.Name)),   #get rid of yyyymmdd_ at start       
         Name = str_sub(Name, 1, str_length(Name)-4)) |> #remove .csv from end
  mutate(Name = sapply(Name, add_underscore)) |>  # add underscore between depth and rep
  separate(Name, into = paste0("part", 1:4), sep = "_", fill = "right")






###trying EEM abosrbance calcs 


#packages 
library(tidyverse)
# library(nlmrt) #for slope ratio calcs

run_20240725 <- read.csv("./Processed_Data/20240725_DH/CDOM_all_20240725.csv")

#works to get a254 and a350
z <- run_20240725 |> 
  rename(wavelength = 1) |> 
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
  mutate("Sample.Name" = paste0("20240725_", Sample, ".csv", sep = "")) |> 
  select(-Sample)


#### REad in EEMs results and bind
eems <- read.csv("./EEMs_Results_2024.csv")

zz <- left_join(eems, z, by = c("Sample.Name")) |> 
  rename(a254 = "a254.x",
         A254 = "A254.x",
         a350 = "a350.x", 
         A350 = "A350.x") |> 
  mutate(a254 = ifelse(is.na(a254), a254.y, a254),
         A254 = ifelse(is.na(A254), A254.y, A254),
         a350 = ifelse(is.na(a350), a350.y, a350),
         A350 = ifelse(is.na(A350), A350.y, A350)) |> 
  select(-a254.y, -A254.y, -a350.y, -A350.y)

write.csv(zz, "./EEMs_Results_2024.csv", row.names = F)


#try getting AGH Sr
# run_20240614 |> 
#   rename(wavelength = 1) |> 
#   filter(wavelength %in% c(254, 275:295, 350:400)) |> 
#   pivot_longer(-(1:2), names_to = "Sample", values_to = "Abs_raw") |> 
#   mutate(naparian_abs = (2.303 * (Abs_raw - MilliQ)) / 0.01,
#          ln_naparian_abs = log(naparian_abs)) |>
#   pivot_longer(-c(1:3)) |> 
#   select(-MilliQ) |> 
#   mutate(name_new = ifelse(name == "Abs_raw" & wavelength == 254, "A254", NA),
#          name_new = ifelse(name == "naparian_abs" & wavelength == 254, "a254", name_new),
#          name_new = ifelse(name == "Abs_raw" & wavelength == 350, "A350", name_new),
#          name_new = ifelse(name == "naparian_abs" & wavelength == 350, "a350", name_new)) |> 
#   select(Sample, name_new, value) |> 
#   pivot_wider(names_from = name_new, values_from = value)

 



##################################### TRIALS #############################################

#### trial data test 
trial_data <- read.csv("C:/Users/dwh18/OneDrive/Desktop/R_Projects/Reservoir_EEMs/Processed_Data/20240206_DHET/20240206_CDOMall.csv")
head(trial_data)

trial_data |> 
  pivot_longer(-1) |> 
  pivot_wider(names_from = Wavelength.nm., values_from = value)


### Want to wind up w/ four columns for each sample A254, a254, A350, a350 (A is raw, a is naperian/blank corrected)

trial_data |> 
  rename(wavelength = 1) |> 
  filter(wavelength %in% c(254, 350)) |> 
  pivot_longer(-(1:2), names_to = "Sample", values_to = "Abs_raw") |> 
  mutate(naparian_abs = (2.303 * (Abs_raw - milliQ)) / 0.01 ) |>
  pivot_longer(-c(1:3)) |> 
  select(-milliQ) |> 
  mutate(name_new = ifelse(name == "Abs_raw" & wavelength == 254, "A254", NA),
         name_new = ifelse(name == "naparian_abs" & wavelength == 254, "a254", name_new),
         name_new = ifelse(name == "Abs_raw" & wavelength == 350, "A350", name_new),
         name_new = ifelse(name == "naparian_abs" & wavelength == 350, "a350", name_new)) |> 
  select(Sample, name_new, value) |> 
  pivot_wider(names_from = name_new, values_from = value)
  
  
  
#### Trying to figure out slope ratios
#Mark's code seems to be adapted from Spencer 2007, w/ same equation as Hepp 2008, but his eqn is slighly diff
#Hep seems like goal, see highlighting in that paper, AGH uses similar approach but averages instead of linear model

## Adapting Mark Johnson code from ABPs email
calculate.SR <- function(x) {

  # S1 = nonlinear fit of an exponential function for S over n275 - n295
  temp <- x |> filter(wavelength %in% c(275:295)) |> rename(y = ln_naparian_abs, x = wavelength)
  regmod <- "y ~ a * exp(-b * x)"
  ones <- c(a=1, b=1) # all ones start
  test.start <- c(a=100, b=0.01)
  anmrtx <- try(nlxb(regmod, start=test.start, trace=FALSE, data=temp))
  S1 <- as.numeric(anmrtx$coef[2])
  
  # S3 = nonlinear fit of an exponential function for S over n350 - n400
  temp <- x |> filter(wavelength %in% c(350:400)) |> rename(y = ln_naparian_abs, x = wavelength)
  regmod <- "y ~ a * exp(-b * x)"
  ones <- c(a=1, b=1) # all ones start
  test.start <- c(a=100, b=0.01)
  anmrtx <- try(nlxb(regmod, start=test.start, trace=FALSE, data=temp))
  S3 <- as.numeric(anmrtx$coef[2])
  SR <- S1/S3
  S13R <- paste(S1, S3, SR, sep = ",")
  return(S13R)
  
}#end MJ function

z <- trial_data |> 
  rename(wavelength = 1) |> 
  pivot_longer(-(1:2), names_to = "Sample", values_to = "Abs_raw") |> 
  filter(Sample == "s1")  |> 
  filter(wavelength %in% c(254, 275:295, 350:400)) |>   
  mutate(naparian_abs = (2.303 * (Abs_raw - milliQ)) / 0.01,
         ln_naparian_abs = log(naparian_abs)) 

calculate.SR(z)
