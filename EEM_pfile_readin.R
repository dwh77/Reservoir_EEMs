### Code to make one pfile a data frame in R 
## DWH 29july25

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

