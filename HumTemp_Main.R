# ==============================================================================
# Script for Data Analysis – Bachelor Thesis
#
# Author: Bart Bruijnen  
# Institution: Maastricht University, Faculty of Health, Medicine and Life Sciences (FHML)  
# Supervisors: Prof. Dr. S.M.E. Engelen, PhD W. Lasten  
# Date: 07-07-2025
#
# Description:
# This R script contains all code used for the statistical analysis and visualization 
# of data collected for the bachelor thesis project. It includes steps for data 
# preprocessing, summary statistics, visualization, and statistical testing.
#
# The analyses support the thesis entitled:
# "The Effect of Confounding Factors on the Volatile Organic Compound Composition 
# of Human Exhaled Breath".
#
# Structure of the script:
# 1. Preparatory analysis (data cleaning, transformations)
# 2. Visualization of temperature and relative humidity (RH)
# 3. Statistical testing (ANOVA)
# 4. Correlation Analysis
# 5. Result Comparison between Departments
#
# Note:
# - Ensure that all required packages are installed before execution.
# - Set the working directory to your local environment if different from the one defined below.


# Before running this script, ensure all required R packages are installed.
# Use the following command to install missing packages automatically.

# Create a vector containing the required packages. Check if installed and if not, install.
packages <- c("ggplot2", "readxl", "lubridate", "...")

# Check if required packages are installed, install if missing
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
} else {
  message("Packages are already installed")
}

# Source custom functions (e.g., plot_range, save.pdf, plot_range2)
source("HumTemp_Functions.R")
# Load required libraries
library(readxl)     # For reading Excel files
library(lubridate)  # For time parsing and manipulation
library(ggplot2)    # For data visualization

# Create an output directory if it doesn't already exist
if (!dir.exists("Output")) {
  dir.create("Output")
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Step 1. Preparatory Analysis

# Set working directory to local environment (adjust as needed)
setwd("~/Maastricht University/Biomedical Sciences/BMS year 3/BBS3006 - Thesis & Internship/Butterfly")

# Load data from Excel file and convert to data frame
weekdays <- read_excel("LAB.xlsx")
weekdays <- as.data.frame(weekdays)
# Convert 'Day' column to a factor (categorical)
weekdays$Day <- as.factor(weekdays$Day)

# Convert selected columns (which contain numbers as text) to numeric values
# Replace comma with dot for decimal, and remove spaces
titles <- c("RH Max (%)", "Temp Max (°C)", "RH Min (%)", "Temp Min (°C)")
weekdays[titles] <- lapply(weekdays[titles], 
                           function(x) as.numeric(gsub(",", ".", gsub(" ", "", x))))

# Compute relative humidity mean and range for each observation
weekdays$RH_Mean <- (weekdays$`RH Max (%)` + weekdays$`RH Min (%)`) / 2
weekdays$RH_Range <- weekdays$`RH Max (%)` - weekdays$`RH Min (%)`
# Compute temperature mean and range
weekdays$Temp_Mean <- (weekdays$`Temp Max (°C)` + weekdays$`Temp Min (°C)`) / 2
weekdays$Temp_Range <- weekdays$`Temp Max (°C)` - weekdays$`Temp Min (°C)`

# Extract RH and temperature columns for summary statistics
RH_Temp <- weekdays[3:10]
# Display summary statistics
summary(RH_Temp)
# Clean and standardize column name: rename "Time of day" to "Time.of.Day"
names(weekdays)[names(weekdays) == "Time of day"] <- "Time.of.Day"
# Convert 'Time.of.Day' column to HMS (hour-minute-second) time format
weekdays$Time.of.Day <- hms::as_hms(weekdays$Time.of.Day)
# Categorize time of day into three time bins: 08, 12, 16
weekdays$Time.Category <- cut(
  as.numeric(hms::as_hms(weekdays$Time.of.Day)),  # Convert to numeric seconds
  breaks = c(0, 10*3600, 14*3600, 24*3600),        # Define 3 intervals: 0–10h, 10–14h, 14–24h
  labels = c("08", "12", "16"),                   # Label bins for grouping
  include.lowest = TRUE, right = FALSE
)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Step 2. Data Visualization – Temperature & RH Range

# Use save.pdf (custom function) to save each plot to a PDF in the Output folder

# Plot relative humidity using boxplot across time categories
save.pdf(function(){
  plot_range(weekdays, "RH_Mean", "Mean Relative Humidity (%)", 
             "Relative Humidity Range per Measurement")
}, "RH Range")

# Plot temperature using boxplot across time categories
save.pdf(function(){
  plot_range(weekdays, "Temp_Mean", "Mean Temperature (°C)", 
             "Temperature Range per Measurement")
}, "Temperature Range")

# Additional line plots per day and time (old version retained for visual comparison)

# Line plot for RH max/min per day and time
plot_range2(weekdays, "RH Max (%)", "RH Min (%)", "Relative Humidity (%)",
            "Relative Humidity Range per Measurement (%)")

# Line plot for Temp max/min per day and time
plot_range2(weekdays, "Temp Max (°C)", "Temp Min (°C)", "Temperature (°C)", 
            "Temperature Range per Measurement (°C)")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Step 3. Statistical Testing

# --- RH ANOVA models by Time.of.Day nested within Day ---

# ANOVA for RH Max
aov_max <- aov(`RH Max (%)` ~ Time.of.Day + Error(Day/Time.of.Day), data = weekdays)
summary(aov_max)

# ANOVA for RH Min
aov_min <- aov(`RH Min (%)` ~ Time.of.Day + Error(Day/Time.of.Day), data = weekdays)
summary(aov_min)

# ANOVA for RH Mean
aov_mean <- aov(RH_Mean ~ Time.of.Day + Error(Day/Time.of.Day), data = weekdays)
summary(aov_mean)

# ANOVA for RH Range
aov_range <- aov(RH_Range ~ Time.of.Day + Error(Day/Time.of.Day), data = weekdays)
summary(aov_range)

# --- Temperature Analysis ---

# ANOVA for Temp Max
aov_temp_max <- aov(`Temp Max (°C)` ~ Time.of.Day + Error(Day/Time.of.Day), data = weekdays)
summary(aov_temp_max)

# ANOVA for Temp Min
aov_temp_min <- aov(`Temp Min (°C)` ~ Time.of.Day + Error(Day/Time.of.Day), data = weekdays)
summary(aov_temp_min)

# ANOVA for Temp Mean
aov_temp_mean <- aov(Temp_Mean ~ Time.of.Day + Error(Day/Time.of.Day), data = weekdays)
summary(aov_temp_mean)

# ANOVA for Temp Range
aov_temp_range <- aov(Temp_Range ~ Time.of.Day + Error(Day/Time.of.Day), data = weekdays)
summary(aov_temp_range)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Step 4. Correlation Analysis

# Pearson correlation between mean temperature and mean relative humidity
cor(weekdays$Temp_Mean, weekdays$RH_Mean, method = "pearson")
corrplot
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Step 5. Location Comparison – Statistical Testing Between Holding and Laboratory
#
# This step compares the environmental conditions (temperature and relative humidity)
# between the two measurement locations: 'Holding' and 'Laboratory'.
# For each variable (RH mean, RH range, Temp mean, Temp range), we test:
# - Normality (Shapiro-Wilk test)
# - Homogeneity of variance (F-test)
# - Difference in means (t-test if normal, Wilcoxon rank-sum if not)

# Load measurement data from separate Excel files for both locations
holding <- read_excel("HOLDING.xlsx")      # Load Holding location data
laboratory <- read_excel("LAB.xlsx")       # Load Laboratory location data

# Add a column to identify the location in each dataset
holding$Location <- "Holding"              # Label rows as "Holding"
laboratory$Location <- "Laboratory"        # Label rows as "Laboratory"

# Combine the two datasets into one
weekdays <- rbind(holding, laboratory)     # Merge both datasets into one dataframe

# Convert all measurement columns from text with comma to numeric with dot
weekdays$`RH Max (%)`     <- as.numeric(gsub(",", ".", weekdays$`RH Max (%)`))      # RH Max
weekdays$`RH Min (%)`     <- as.numeric(gsub(",", ".", weekdays$`RH Min (%)`))      # RH Min
weekdays$`Temp Max (°C)`  <- as.numeric(gsub(",", ".", weekdays$`Temp Max (°C)`))   # Temp Max
weekdays$`Temp Min (°C)`  <- as.numeric(gsub(",", ".", weekdays$`Temp Min (°C)`))   # Temp Min

# Calculate derived variables: mean and range of RH and temperature
weekdays$RH_Mean     <- (weekdays$`RH Max (%)` + weekdays$`RH Min (%)`) / 2         # Average RH
weekdays$RH_Range    <- weekdays$`RH Max (%)` - weekdays$`RH Min (%)`              # RH variation
weekdays$Temp_Mean   <- (weekdays$`Temp Max (°C)` + weekdays$`Temp Min (°C)`) / 2   # Average temperature
weekdays$Temp_Range  <- weekdays$`Temp Max (°C)` - weekdays$`Temp Min (°C)`        # Temp variation

# Subset the variables per location
rh_holding        <- subset(weekdays, Location == "Holding")$RH_Mean               # RH mean Holding
rh_lab            <- subset(weekdays, Location == "Laboratory")$RH_Mean            # RH mean Laboratory

temp_holding      <- subset(weekdays, Location == "Holding")$Temp_Mean             # Temp mean Holding
temp_lab          <- subset(weekdays, Location == "Laboratory")$Temp_Mean          # Temp mean Laboratory

range_rh_holding  <- subset(weekdays, Location == "Holding")$RH_Range              # RH range Holding
range_rh_lab      <- subset(weekdays, Location == "Laboratory")$RH_Range           # RH range Laboratory

range_temp_holding <- subset(weekdays, Location == "Holding")$Temp_Range           # Temp range Holding
range_temp_lab     <- subset(weekdays, Location == "Laboratory")$Temp_Range        # Temp range Laboratory

# Run the function for each environmental variable
run_tests(rh_holding, rh_lab, "RH_Mean")
run_tests(temp_holding, temp_lab, "Temp_Mean")
run_tests(range_rh_holding, range_rh_lab, "RH_Range")
run_tests(range_temp_holding, range_temp_lab, "Temp_Range")
# ==============================================================================
# End of script
# 
# If you wish you can repeat the script and subsequent steps with other
# humidity and temperature data.
print("All steps completed successfully. Check output directory for visualizations and results.")
