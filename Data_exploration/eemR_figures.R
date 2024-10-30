### abbrevaited script from AGH to make heatmaps for an indivual EEMs via eemR and staRdom

### 29 Feb. 2024, A. Hounshell
###############################################################################
# Load in packages
pacman::p_load(ggplot2,tidyverse,lattice,ggpubr,eemR,htmltools, staRdom)

###############################################################################
## Load in files
file <- "./Processed_Data/20240822_DH/20240822_CS1_15aug24_01r1.csv"
eem <- eem_read(file, recursive = TRUE, import_function = eem_csv)

## Load in blank
file <- "./Processed_Data/20240822_DH/20240822_MilliQ.csv"
blank_media <- eem_read(file, recursive = TRUE, import_function = eem_csv)

## Load in instrument correction file
# inst_corr <- read.csv("./Emcorr_220 to 600_standard.csv",skip=1) %>% 
#   rename(em = Wavelength..nm., Intensity = Intensity..a.u..)
# 
# ## Subset as needed - following eem file
# sub_out <- setdiff(x=inst_corr$em,y=round(eem[[1]]$em))
# 
# inst_corr <- inst_corr[!inst_corr$em %in% sub_out, ]
# 
# for (i in 1:length(eem[[1]]$ex)){
#   eem[[1]]$x[,i] <- as.numeric(eem[[1]]$x[,i])*inst_corr[,2]
#   blank_media[[1]]$x[,i] <- as.numeric(blank_media[[1]]$x[,i]*inst_corr[,2])
# }

## Blank correction
eem_blank_corr <- eem_remove_blank(eem, blank_media)

## Raman/Rayleigh scattering correction
## 1st order correction
eem_corr <- eem_remove_scattering(eem_blank_corr, "rayleigh", 1, 7)

## 2nd order correction
eem_corr <- eem_remove_scattering(eem_corr, type = "rayleigh", order = 2, width = 10)

## Remove negative values and set to 0
eem_corr[[1]]$x[eem_corr[[1]]$x<0] <- 0

## Visualize corrections
plot(blank_media)

plot(eem)

plot(eem_corr)
