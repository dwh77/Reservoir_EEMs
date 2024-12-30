# Reservoir_EEMs

Project for processing EEMs collected at Carvins Cove and Beaverdam Reservoirs from 2021 - 2024.
Processing scripts and SOP are from: https://github.com/CareyLabVT/Reservoirs/tree/master/Data/DataNotYetUploadedToEDI/Raw_EEMs and
Hounshell et al. 2021 https://portal.edirepository.org/nis/mapbrowse?packageid=edi.841.1

## Folder organization

### Correction_Files
Contains matlab script and excel files needed to run correction function 

### Processed_Data
Folder containing raw and processed data from each EEMs analysis date 
Each folder represent date samples were run and contains raw results from fluorometer and photometer as well as processed sample results 

### EEMs_Results csvs
These csvs contain data from 2021-2024 based on the years. 
These csv contains final EEMs results from every analysis date that are compiled from the results_YYYYMMDD_sampleID excel files 
The other results csv contains a subset of samples from 2023 plunge point samples in a format for easier plotting

### Data_exploration

This contains scripts to visualize data related to 2024 spatial carbon quality sampling
