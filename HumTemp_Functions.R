# ==============================================================================
# Functions for Humidity and Temperature Data Visualization - Bachelor Thesis
# Author: Bart Bruijnen
# Institution: Maastricht University, FHML
# Date: 07-07-2025
#
# This R script contains custom plotting functions used in the analysis of
# environmental variables such as temperature and humidity, as part of the
# bachelor thesis:
# "The Effect of Confounding Factors on the Volatile Organic Compound (VOC)
# Composition of Human Exhaled Breath".
#
# Functions included:
# - save.pdf(): Saves a ggplot or base R plot to a PDF in the "Output" folder.
# - plot_range(): Creates a boxplot of a continuous variable by time category.
# - plot_range2(): (Deprecated) Plots min/max values across time and days.
#
# Usage:
# Source this file into your main script using source("HumTemp_Functions.R")
# Ensure required packages (ggplot2, hms) are installed.
# ==============================================================================
# ----------------------------
# Function: save.pdf
# ----------------------------
# Saves a plot created by a plotting function to a PDF file in the "Output" folder.
# - plot_function: A function that returns a ggplot2 or base plot object.
# - filename: Desired filename (without .pdf extension) to save the plot as.
save.pdf <- function(plot_function, filename) {
  # Define the output file path
  pdf_file <- file.path("Output", paste0(filename, ".pdf"))
  # Open a PDF graphics device
  pdf(pdf_file)
  # Generate and print the plot
  print(plot_function())
  # Close the graphics device
  dev.off()
  # Notify user
  message("Plot has been saved to: ", pdf_file)
}
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ----------------------------
# Function: plot_range
# ----------------------------
# Creates a boxplot for a specified numeric column across time categories.
# 
# Arguments:
# - data:        Data frame containing the data.
# - value_col:   Name (string) of the numeric column to plot (e.g., "Temp", "Humidity").
# - y_label:     Label for the Y-axis.
# - plot_title:  Title to display at the top of the plot.
plot_range <- function(data, value_col, y_label, plot_title) {
  
  # Initialize ggplot: set x-axis to Time.Category and y-axis to the selected value column
  ggplot(data, aes(x = Time.Category, y = .data[[value_col]])) +
    # Create boxplots for each Time.Category group
    # - fill: Light blue color
    # - alpha: Semi-transparent for a soft appearance
    geom_boxplot(fill = "skyblue", alpha = 0.7) +
    # Add labels to the plot (title and axis labels)
    labs(
      title = plot_title,  # Title of the entire plot
      x = "Time of Day",   # Label for the x-axis (categorical time segments)
      y = y_label          # Label for the y-axis (e.g., "Temperature (°C)")
    ) +
    # Apply a minimal theme for a clean and modern appearance
    theme_minimal()
}
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ----------------------------
# Function: plot_range2 (Unused)
# ----------------------------
# Creates a line plot showing min and max values over time by day.
# Note: Used in earlier drafts. Kept for backward compatibility.
# - data: Data frame with time, day, and value columns.
# - max_col: Column name for maximum values (numeric).
# - min_col: Column name for minimum values (numeric).
# - y_label: Label for the Y-axis.
# - plot_title: Title of the plot.
plot_range2 <- function(data, max_col, min_col, y_label, plot_title) {
  # Begin ggplot with Time.of.Day on the x-axis, group and color lines by Day
  ggplot(data, aes(x = Time.of.Day, group = Day, color = Day)) +
    
    # Add solid line for the maximum values (e.g., max temperature/humidity)
    geom_line(aes(y = .data[[max_col]]), size = 1) +
    # Add points for the maximum values
    geom_point(aes(y = .data[[max_col]])) +
    # Add dashed line for the minimum values (e.g., min temperature/humidity)
    geom_line(aes(y = .data[[min_col]]), linetype = "dashed", size = 1) +
    # Add open circle points for the minimum values
    geom_point(aes(y = .data[[min_col]]), shape = 1) +
    # Add plot title and axis labels
    labs(
      title = plot_title,     # Main title of the plot
      x = "Time of Day",      # X-axis label
      y = y_label,            # Y-axis label (e.g., "Temperature (°C)")
      color = "Day"           # Legend title for color (day grouping)
    ) +
    
    # Customize the x-axis as time with specific breaks and limits
    scale_x_time(
      limits = c(               # Set visible time range
        hms::as_hms("07:30:00"),
        hms::as_hms("16:30:00")
      ),
      breaks = hms::as_hms(     # Define specific tick marks on x-axis
        c("08:00:00", "12:00:00", "16:00:00")
      )
    ) +
    # Apply a clean, minimal theme for better aesthetics
    theme_minimal()
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ----------------------------
# Function: run_tests
# ----------------------------
# Performs statistical comparison between two numeric vectors from different groups.
# This function assesses whether two variables (e.g., Holding vs. Laboratory) differ
# significantly by testing for:
# - Normality using Shapiro-Wilk tests
# - Homogeneity of variance using an F-test
# - Difference in means using:
#     * t-test if both groups are normally distributed
#     * Wilcoxon rank-sum test (Mann–Whitney U) if not
#
# Arguments:
# - var1:      Numeric vector from group 1 (e.g., Holding)
# - var2:      Numeric vector from group 2 (e.g., Laboratory)
# - var_name:  Name of the variable being tested, for labeling output
run_tests <- function(var1, var2, var_name) {
  # Print header for this variable’s analysis
  cat("===== Tests for", var_name, "=====\n")
  
  # Perform Shapiro-Wilk test for normality on both groups
  norm1 <- shapiro.test(var1)   # Normality test for group 1
  norm2 <- shapiro.test(var2)   # Normality test for group 2
  
  # Output results of normality tests
  cat("Shapiro-Wilk Normality Test:\n\n")
  print(norm1)
  cat("\n")
  print(norm2)
  cat("\n\n")
  
  # Determine if both variables are normally distributed
  normal <- norm1$p.value > 0.05 && norm2$p.value > 0.05
  
  if (normal) {
    # If normality holds for both, proceed with F-test for equal variances
    f_test <- var.test(var1, var2)     # F-test for homogeneity of variance
    cat("F-test (equal variances):\n\n")
    print(f_test)
    cat("\n")
    
    # Determine whether to assume equal variance in t-test
    equal_var <- f_test$p.value > 0.05  # If p > 0.05, assume equal variances
    t_test <- t.test(var1, var2, var.equal = equal_var)  # Perform t-test
    cat("T-test:\n\n")
    print(t_test)   # Output results of t-test
  } else {
    # If normality does not hold, use non-parametric Wilcoxon test
    wilcox <- wilcox.test(var1, var2)   # Wilcoxon rank-sum test (Mann–Whitney U)
    cat("Wilcoxon Rank-Sum Test (Mann–Whitney U):\n\n")
    print(wilcox)  # Output results of Wilcoxon test
  }
  
  # Add line break after each complete variable analysis
  cat("\n\n")
}
# ==============================================================================
# End of HumTemp_Functions.R
#
# To use these functions, first source the script:
# source("HumTemp_Functions.R")
# Then call the functions as needed in your main script or analysis pipeline.
