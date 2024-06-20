###trying EEM abosrbance calcs 

#packages 
library(tidyverse)
# library(nlmrt) #for slope ratio calcs

run_20240614 <- read.csv("./Processed_Data/20240614_DH/20240614_CDOMall.csv")

#works to get a254 and a350
z <- run_20240614 |> 
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
  mutate(Sample = paste0("20240614_", Sample, ".csv", sep = ""))


#### REad in EEMs results and bind
eems <- read.csv("./EEMs_Results_2024.csv")

zz <- left_join(eems, z, by = c("Sample.Name" = "Sample"))

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
