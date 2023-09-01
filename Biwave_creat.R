## Clear RStudio
rm(list = ls())  # Clear the global environment
graphics.off()  # Clear all plots
cat("\014")  # Clear the console
## ----------------------------------------------------------------------------

# Load the required libraries
library(ggpubr)
library(tidyverse)
library(readxl)
library(biwavelet)
library(openxlsx)
library(readxl)

## -----------------------------------------------------------------------------
## Setting R to fetch the data from my saved location
getwd()

setwd("D:/Research Assistant - SU/Creativity Data")

# Importing the data:

# As multiple files reflecting multiple data frames

# Reading in multiple .txt files to create a list of data frames

my_files <- list.files(pattern = "\\.txt$")

# Check to see that the files has been read correctly

my_files[1]

# Format the data, removing the first 12 lines of text

my_data <- lapply(my_files, read.table, sep = '\t', header = TRUE,  skip = 12,
                  na.strings = c(".")) 

##-----------------------------------------------------------------------------
## Concerting the Video.Time columns from a chr string to time

# Set the starting time and format
starting_time <- as.POSIXlt("2023-09-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")

# Set the sampling rate (at 15 frames per second)
sampling_rate <- 15

# Looping through the my_data list
for (i in seq_along(my_data)) {
  if ("Video.Time" %in% colnames(my_data[[i]])) {
    num_rows <- nrow(my_data[[i]])
    
    # Setting the intervals in line with the sampling rate of 15 fps
    time_intervals <- 1000 / sampling_rate
    
    # Calculating the different time points using the intervals, sampling rate etc.
  
    time_points <- starting_time + ((1:num_rows) - 1) * time_intervals / 1000
    
    # Now making sure the time points are set as time in the "Video.Time" column
    my_data[[i]]$Video.Time <- time_points 
  }
}

## Here is where our variables are selected in both instances of the time series
# For instance, columns 16 and 99 contain Pitch data for participant 1 and 2 
# respectively 

# Task, Time, Pitch, Yaw and Roll
# Specifying alternative columns for df6 due to the differing structure

# Define a function to extract the appropriate columns based on the data frame's position
extract_columns <- function(df, index) {
  if (index == 6) {
    # For data frame six, extract columns 99, 100, and 101
    return(df[c(1, 2, 16, 17, 18, 99, 100, 101)])
  } else {
    # For all other data frames, extract columns 98, 99, and 100
    return(df[c(1, 2, 16, 17, 18, 98, 99, 100)])
  }
}

# Use lapply to extract the columns for all data frames
dyadlist <- lapply(seq_along(my_data), function(i) extract_columns(my_data[[i]], i))

# Just a quick check for df 6
head(dyadlist[[6]])
##-----------------------------------------------------------------------------
## Now to subset the dataframes into separate frames for each of the tasks
# First, rename all columns so that they are the same across all data frames

# Create the new column names
new_column_names <- c("Task", "Time (ms)", "Pitch.1", "Yaw.1", "Roll.1", "Pitch.2",
                      "Yaw.2", "Roll.2")

# Apply this to all the data frames in 'dyadlist'
dyadlist_renamed <- lapply(dyadlist, function(df) {
  colnames(df) <- new_column_names
  df
})

##-----------------------------------------------------------------------------
## Removing missing values/ NAs by row for all dfs within the list
# Access each data frame in dyadlist_renamed and extract the dataframes
df_1 <- dyadlist_renamed[[1]]
df_2 <- dyadlist_renamed[[2]]
df_3 <- dyadlist_renamed[[3]]
df_4 <- dyadlist_renamed[[4]]
df_5 <- dyadlist_renamed[[5]]
df_6 <- dyadlist_renamed[[6]]
df_7 <- dyadlist_renamed[[7]]
df_8 <- dyadlist_renamed[[8]]
df_9 <- dyadlist_renamed[[9]]
df_10 <- dyadlist_renamed[[10]]
df_11 <- dyadlist_renamed[[11]]
df_12 <- dyadlist_renamed[[12]]
df_13 <- dyadlist_renamed[[13]]
df_14 <- dyadlist_renamed[[14]]
df_15 <- dyadlist_renamed[[15]]

##------------------------------------------------------------------------------
# Looping through the list does not appear to work due to differing lengths of rows for 
# the 15 dataframes. Repeat code for each df...

# Remove rows with any missing values from df_1
df_1_clean <- df_1[rowSums(is.na(df_1)) == 0, ]

# Remove rows with any missing values from df_2
df_2_clean <- df_2[rowSums(is.na(df_2)) == 0, ]

# Remove rows with any missing values from df_3
df_3_clean <- df_3[rowSums(is.na(df_3)) == 0, ]

# Remove rows with any missing values from df_4
df_4_clean <- df_4[rowSums(is.na(df_4)) == 0, ]

# Remove rows with any missing values from df_5
df_5_clean <- df_5[rowSums(is.na(df_5)) == 0, ]

# Remove rows with any missing values from df_6
df_6_clean <- df_6[rowSums(is.na(df_6)) == 0, ]

# Remove rows with any missing values from df_7
df_7_clean <- df_7[rowSums(is.na(df_7)) == 0, ]

# Remove rows with any missing values from df_8
df_8_clean <- df_8[rowSums(is.na(df_8)) == 0, ]

# Remove rows with any missing values from df_9
df_9_clean <- df_9[rowSums(is.na(df_9)) == 0, ]

# Remove rows with any missing values from df_10
df_10_clean <- df_10[rowSums(is.na(df_10)) == 0, ]

# Remove rows with any missing values from df_11
df_11_clean <- df_11[rowSums(is.na(df_11)) == 0, ]

# Remove rows with any missing values from df_12
df_12_clean <- df_12[rowSums(is.na(df_12)) == 0, ]

# Remove rows with any missing values from df_13
df_13_clean <- df_13[rowSums(is.na(df_13)) == 0, ]

# Remove rows with any missing values from df_14
df_14_clean <- df_14[rowSums(is.na(df_14)) == 0, ]

# Remove rows with any missing values from df_15
df_15_clean <- df_15[rowSums(is.na(df_15)) == 0, ]

##---------------------------------------------------------------------------
## Data frames are now cleaned for NAs. Now to subset based upon task, 0 - 4. 
# Looping through all 15 cleaned dfs

# List of cleaned data frames
cleaned_dfs <- list(df_1_clean, df_2_clean, df_3_clean, df_4_clean, df_5_clean,
                    df_6_clean, df_7_clean, df_8_clean, df_9_clean, df_10_clean,
                    df_11_clean, df_12_clean, df_13_clean, df_14_clean, df_15_clean)

# Now to loop through each cleaned data frame based upon experimental task
for (i in seq_along(cleaned_dfs)) {
  df_name <- paste0("df_", i, "_clean")
  df <- cleaned_dfs[[i]]
  
  # Loop through values 0 to 3, 0 as pre-trial and so on
  for (value in 0:3) {
    subset_df <- df[df$Task == value, ]
    assign(paste0(df_name, "_", value), subset_df)
  }
}

##-----------------------------------------------------------------------------
## Now to down-sample each of the data frame, taking a proportion of the data 
# each task within each data frame

# Create a new list to store the down-sampled data frames
downsampled_combined_dfs <- list()

# Loop through each of the cleaned data frames
for (i in seq_along(cleaned_dfs)) {
  df <- cleaned_dfs[[i]]
  
  # Create an empty list to store downsampled data frames for this data frame
  downsampled_combined_task_dfs <- list()
  
  # Loop through values 0 to 3 (tasks)
  for (value in 0:3) {
    subset_df <- df[df$Task == value, ]
    
    # Calculate the number of rows to sample (25%)
    num_rows <- nrow(subset_df)
    num_samples <- ceiling(num_rows * 0.25)
    
    # Sample 25% of the data frame
    sampled_subset_df <- subset_df[sample(num_rows, num_samples), ]
    
    # Add the sampled data frame to the downsampled_combined_task_dfs list
    downsampled_combined_task_dfs[[value + 1]] <- sampled_subset_df
  }
  
  # Combine the downsampled data frames for each task
  combined_df <- do.call(rbind, downsampled_combined_task_dfs)
  
  # Add the combined data frame to the downsampled_combined_dfs list
  downsampled_combined_dfs[[i]] <- combined_df
}

##-----------------------------------------------------------------------------
## Now to compute the wavelet coherence for Pitch data for each dyad 

# Firstly, testing it on the first dyad/df
pitch_df1 <- downsampled_combined_dfs[[1]]  

# Extract column 3 into a matrix with a matching length column for wtc function
Pitch_Part1_matrix <- cbind(1:2207, pitch_df1[, 3])

# Extract column 6 into a matrix with a matching length column for wtc function
Pitch_Part2_matrix <- cbind(1:2207, pitch_df1[, 6])

# Compute wavelet coherence with randomizations set to 10
pitch_1_coherence_result <- wtc(Pitch_Part1_matrix, Pitch_Part2_matrix, 
                                nrands = 10)
  
## Plot wavelet coherence and phase difference (arrows)
## Make room to the right for the color bar (showing power)
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
plot(pitch_1_coherence_result, plot.cb = TRUE, plot.phase = TRUE)

##------------------------------------------------------------------------------
## Now generate a loop so the wtc function can iterate through the down-sampled
# list for Pitch (columns 3 & 6), including plotting each

# Loop through the list of data frames
for (i in seq_along(downsampled_combined_dfs)) {
  # Extract the current data frame
  pitch_df <- downsampled_combined_dfs[[i]]
  
  # Extract column 3 into a matrix with a matching length column
  Pitch_Part1_matrix <- cbind(1:nrow(pitch_df), pitch_df[, 3])
  
  # Extract column 6 into a matrix with a matching length column
  Pitch_Part2_matrix <- cbind(1:nrow(pitch_df), pitch_df[, 6])
  
  # Compute wavelet coherence with specified parameters
  pitch_coherence_result <- wtc(Pitch_Part1_matrix, Pitch_Part2_matrix, nrands = 10)
  
  ## Plot wavelet coherence and phase difference (arrows)
  ## Make room to the right for the color bar
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
  plot(pitch_coherence_result, plot.cb = TRUE, plot.phase = TRUE)
}

##-----------------------------------------------------------------------------
# list for Yaw(columns 4 & 7), including plotting each

# Loop through the list of data frames
for (i in seq_along(downsampled_combined_dfs)) {
  # Extract the current data frame
  yaw_df <- downsampled_combined_dfs[[i]]
  
  # Extract column 3 into a matrix with a matching length column
  yaw_Part1_matrix <- cbind(1:nrow(yaw_df), yaw_df[, 4])
  
  # Extract column 6 into a matrix with a matching length column
  yaw_Part2_matrix <- cbind(1:nrow(yaw_df), yaw_df[, 7])
  
  # Compute wavelet coherence with specified parameters
  yaw_coherence_result <- wtc(yaw_Part1_matrix, yaw_Part2_matrix, nrands = 10)
  
  ## Plot wavelet coherence and phase difference (arrows)
  ## Make room to the right for the color bar
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
  plot(yaw_coherence_result, plot.cb = TRUE, plot.phase = TRUE)
}

##-----------------------------------------------------------------------------
### list for Roll (columns 5 & 8), including plotting each

# Loop through the list of data frames
for (i in seq_along(downsampled_combined_dfs)) {
  # Extract the current data frame
  roll_df <- downsampled_dfs[[i]]
  
  # Extract column 3 into a matrix with a matching length column
  roll_Part1_matrix <- cbind(1:nrow(roll_df), roll_df[, 5])
  
  # Extract column 6 into a matrix with a matching length column
  roll_Part2_matrix <- cbind(1:nrow(roll_df), roll_df[, 8])
  
  # Compute wavelet coherence with specified parameters
  roll_coherence_result <- wtc(roll_Part1_matrix, roll_Part2_matrix, nrands = 10)
  
  ## Plot wavelet coherence and phase difference (arrows)
  ## Make room to the right for the color bar
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
  plot(roll_coherence_result, plot.cb = TRUE, plot.phase = TRUE)
}
##------------------------------------------------------------------------------
## Now to identify the coherence values for each of the variables - Pitch, Yaw 
# and Roll

# Draw out the coherence values for Pitch 
pitch_coherence_values <- pitch_coherence_result$rsq

# Draw out the coherence values for Pitch 
yaw_coherence_values <- yaw_coherence_result$rsq

# Draw out the coherence values for Pitch 
roll_coherence_values <- roll_coherence_result$rsq

# Calculate the mean coherence values across scales (rows)
mean_pitch_coherence_scale <- rowMeans(pitch_coherence_values)

# Calculate the mean coherence values across frequencies (columns)
mean_pitch_freq <- colMeans(pitch_coherence_values)

# Flatten the rsq matrix into a vector
rsq_vector_p <- as.vector(pitch_coherence_values)

# Calculate the overall mean coherence value
overall_mean_coherence_pitch <- mean(rsq_vector_p)

##------------------------------------------------------------------------------
# Draw out the coherence values for Yaw
yaw_coherence_values <- yaw_coherence_result$rsq

# Calculate the mean coherence values across scales (rows) for Yaw
mean_yaw_coherence_scale <- rowMeans(yaw_coherence_values)

# Calculate the mean coherence values across frequencies (columns) for Yaw
mean_yaw_freq <- colMeans(yaw_coherence_values)

# Flatten the rsq matrix into a vector for Yaw
rsq_vector_yaw <- as.vector(yaw_coherence_values)

# Calculate the overall mean coherence value for Yaw
overall_mean_coherence_yaw <- mean(rsq_vector_yaw)
##-----------------------------------------------------------------------------

# Draw out the coherence values for Roll
roll_coherence_values <- roll_coherence_result$rsq

# Calculate the mean coherence values across scales (rows) for Roll
mean_roll_coherence_scale <- rowMeans(roll_coherence_values)

# Calculate the mean coherence values across frequencies (columns) for Roll
mean_roll_freq <- colMeans(roll_coherence_values)

# Flatten the rsq matrix into a vector for Roll
rsq_vector_roll <- as.vector(roll_coherence_values)

# Calculate the overall mean coherence value for Roll
overall_mean_coherence_roll <- mean(rsq_vector_roll)
##------------------------------------------------------------------------------
## Now to find the standard deviations of each of the overall means

# For Pitch
sd_overall_mean_coherence_pitch <- sd(rsq_vector_p)

# For Yaw
sd_overall_mean_coherence_yaw <- sd(rsq_vector_yaw)

# For Roll
sd_overall_mean_coherence_roll <- sd(rsq_vector_roll)

##------------------------------------------------------------------------------

# Create a data frame for storing mean and standard deviation values
overall_coherence_summary <- data.frame(
  Variable = c("Pitch", "Yaw", "Roll"),
  Overall_Mean_Coherence = c(overall_mean_coherence_pitch, overall_mean_coherence_yaw, overall_mean_coherence_roll),
  SD_Overall_Mean_Coherence = c(sd_overall_mean_coherence_pitch, sd_overall_mean_coherence_yaw, sd_overall_mean_coherence_roll)
)

# Print the data frame
print(overall_coherence_summary)

##------------------------------------------------------------------------------



