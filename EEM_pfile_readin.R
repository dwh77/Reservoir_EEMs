### Code to make pfile into a data frame in R 
## DWH 29july25

#### Read in one unknown file and set up as data frame ----

#read in pfile
ptest <- readLines("./Processed_Data/20250424_DH/p_20250424_CC4_16apr25_BOTR1")

## break data into appropriate columns
split_lines <- strsplit(ptest, "[ \t]+")

df <- do.call(rbind, lapply(split_lines, function(x) {
  length(x) <- 45  # There should be 43 columns but this catches a blank in column one and makes sure nothing missed at the end
  x
}))

#make a data frame
df <- as.data.frame(df, stringsAsFactors = FALSE)

df <- df |> dplyr::select(2:44) #select just the columns with data
colnames(df) <- NULL #remove the off numbered column names


#### function to save all pfiles as csvs in one folder ----

library(dplyr)

# Step 1: Create output folder if it doesn't exist
output_dir <- "./Processed_Data/P_files"

# Step 2: Find all p_ files recursively
p_files <- list.files("./Processed_Data", pattern = "^p_", recursive = TRUE, full.names = TRUE)

# Step 3: Define processing function
process_pfile <- function(file_path) {
  # Read lines
  lines <- readLines(file_path)
  
  # Split on space or tab
  split_lines <- strsplit(lines, "[ \t]+")
  
  # Pad and bind
  df <- do.call(rbind, lapply(split_lines, function(x) {
    length(x) <- 45
    x
  }))
  
  # Clean up
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- df |> select(2:44)
  colnames(df) <- NULL
  
  # Extract just the filename (no path)
  file_name <- basename(file_path)
  
  # Save to centralized folder
  output_path <- file.path(output_dir, paste0(file_name, ".csv"))
  write.csv(df, output_path, row.names = FALSE)
}

# Step 4: Apply to all files
lapply(p_files, process_pfile)


##### Trying function again to have column and row be wavelenghts ---------------------------------
library(dplyr)

# Step 1: Create output folder if it doesn't exist
output_dir <- "./Processed_Data/P_files2"

# Step 2: Find all p_ files recursively
p_files <- list.files("./Processed_Data/20250424_DH", pattern = "^p_", recursive = TRUE, full.names = TRUE)


process_pfile <- function(file_path) {
  # Read lines
  lines <- readLines(file_path)
  
  # Split on space or tab
  split_lines <- strsplit(lines, "[ \t]+")
  
  # Pad each line to 45 elements
  padded_lines <- lapply(split_lines, function(x) {
    length(x) <- 45
    x
  })
  
  # Convert to data frame
  df <- as.data.frame(do.call(rbind, padded_lines), stringsAsFactors = FALSE)
  
  # Select columns 2 to 44 (as before)
  df <- df |> select(2:44)
  
  # Convert to numeric
  df[] <- lapply(df, as.numeric)
  
  # Create new column (300 to 600 by 2)
  new_col <- seq(300, 600, by = 2)
  
  # Ensure row count matches length of new_col
  df <- df[seq_along(new_col), ]
  
  # Add the new column as first column
  df <- cbind(X = new_col, df)
  
  # Create new row (240 to 450 by 5)
  new_row <- c(NA, seq(240, 450, by = 5))  # NA for top-left corner
  
  # Add the new row as first row
  df <- rbind(new_row, df) 
  
  # Extract just the filename (no path)
  file_name <- basename(file_path)
  
  # Save to centralized folder
  output_path <- file.path(output_dir, paste0(file_name, ".csv"))
  write.table(df, output_path, sep = ",", row.names = FALSE, col.names = FALSE)
  
}

# Step 4: Apply to all files
lapply(p_files, process_pfile)

