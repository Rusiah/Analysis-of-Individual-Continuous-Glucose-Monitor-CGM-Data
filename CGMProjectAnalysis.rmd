# Analysis-of-Individual-Continuous-Glucose-Monitor-CGM-Data
#Test
# Install and load required packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Set directory
setwd("/Users/alexanderghe/Desktop/CGM Project")

# Read the dataset
data <- read_delim("CGM_datavalues.s010 2", delim = "\t", col_names = FALSE)

# Assign column names
colnames(data) <- c("Timestamp", "Glucose_Value", "Subject_ID", "Internal_Time")

# Print the first few rows to confirm it is read correctly
print(head(data))

# Group by Subject_ID and count the number of readings
subject_counts <- data %>%
  group_by(Subject_ID) %>%
  summarise(reading_count = n())

# Print the counts
print(subject_counts)

# Identify the subject with the maximum number of readings
max_subject <- subject_counts %>%
  filter(reading_count == max(reading_count))

# Print the subject with the greatest number of readings
print(max_subject)

# Convert Timestamp to POSIXct for easier handling
data$Timestamp <- as.POSIXct(data$Timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Filter data for the subject with the greatest number of readings
subject_id <- max_subject$Subject_ID
subject_data <- data %>%
  filter(Subject_ID == subject_id)

# Single Hour Visualization
single_hour_data <- subject_data %>%
  filter(Timestamp >= as.POSIXct("2016-09-20 10:00:00") & Timestamp < as.POSIXct("2016-09-20 11:00:00"))

ggplot(single_hour_data, aes(x = Timestamp, y = Glucose_Value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_hline(yintercept = 70, linetype = "dashed") +
  labs(title = "CGM Data for a Single Hour",
       x = "Time",
       y = "CGM value [mg/dL]") +
  theme_minimal()

# 24-Hour Day Visualization
single_day_data <- subject_data %>%
  filter(Timestamp >= as.POSIXct("2016-09-20 00:00:00") & Timestamp < as.POSIXct("2016-09-21 00:00:00"))

ggplot(single_day_data, aes(x = Timestamp, y = Glucose_Value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_hline(yintercept = 70, linetype = "dashed") +
  labs(title = "CGM Data for a 24-Hour Day",
       x = "Time",
       y = "CGM value [mg/dL]") +
  theme_minimal()

# Week Visualization
week_data <- subject_data %>%
  filter(Timestamp >= as.POSIXct("2016-09-20 00:00:00") & Timestamp < as.POSIXct("2016-09-27 00:00:00"))

ggplot(week_data, aes(x = Timestamp, y = Glucose_Value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_hline(yintercept = 70, linetype = "dashed") +
  labs(title = "CGM Data for a Week",
       x = "Time",
       y = "CGM value [mg/dL]") +
  theme_minimal()
