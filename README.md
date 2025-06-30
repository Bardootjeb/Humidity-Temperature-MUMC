# Humidity-Temperature-MUMC

This repository contains the code and data used for the environmental sub-analysis of the bachelor thesis titled:

**“The Effect of Confounding Factors on the Volatile Organic Compound Composition of Human Exhaled Breath”**  
_Bart Bruijnen, Maastricht University, Faculty of Health, Medicine and Life Sciences (FHML), 2025_

## Research Context

Environmental factors such as **temperature** and **relative humidity (RH)** can significantly affect gas chromatography-ion mobility spectrometry (GC-IMS) readings of volatile organic compounds (VOCs) in human breath. This sub-analysis assessed:

- Diurnal and departmental variations in temperature and RH,
- Differences between the **Holding** and **Laboratory** environments,
- Correlations between environmental conditions and measurement reliability.

## Repository Structure

```
.
├── HOLDING.xlsx            # Environmental data from Holding Department
├── LAB.xlsx                # Environmental data from Laboratory
├── INCLUSIES.xlsx          # Metadata for included measurements (not used directly in script)
├── HumTemp_Main.R          # Main script for preprocessing, analysis & visualization
├── HumTemp_Functions.R     # Custom plotting and testing functions
├── README.md               # Project documentation (this file)
```

## Main Features

### Core Functionalities:
- Data loading and cleaning of Excel datasets
- Time binning (08:00, 12:00, 16:00)
- Boxplots of RH and Temperature distributions
- Repeated measures ANOVA across timepoints
- Pearson correlations
- Statistical comparisons between departments (t-tests / Wilcoxon)
- Exporting plots as PDFs

## How to Use

### 1. Set up your R Environment

Make sure you have R and RStudio installed. You may need the following R packages:

```r
packages <- c("ggplot2", "readxl", "lubridate", "hms")
```

Install any missing packages using:

```r
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
```

### 2. Clone the repository

```bash
git clone https://github.com/Bardootjeb/Humidity-Temperature-MUMC.git
cd Humidity-Temperature-MUMC
```

### 3. Run the scripts

In RStudio:

```r
source("HumTemp_Functions.R")
source("HumTemp_Main.R")
```

Ensure your working directory is set to the project root or adjust it in the script:

```r
setwd("your/local/path/Humidity-Temperature-MUMC")
```

### 4. Output

All generated plots (boxplots, line graphs, etc.) are saved as PDFs in a newly created `Output/` folder.

## Example Visualizations

The following plots are created:
- Mean RH and Temp per timepoint (boxplots)
- RH/Temp range per day and time (line plots)
- Departmental comparison of RH and Temp using statistical tests

## Background (from Thesis)

GC-IMS analysis of exhaled breath is a promising non-invasive technique for diagnosing thyroid cancer. However, RH and temperature fluctuations during breath collection may introduce analytical variance. This repository contributes to understanding:

- How RH and Temp vary within clinical environments
- Whether these changes significantly influence VOC results
- Best practices for breath sample standardization in hospital settings

For more context, refer to the full thesis: _"The Effect of Confounding Factors on the Volatile Organic Compound Composition of Human Exhaled Breath."_ Maastricht University, 2025.

## License

This project is academic in nature and not licensed for commercial reuse. Contact the author for permission or collaboration.

## Author & Contact

**Bart Bruijnen**  
BSc Biomedical Sciences  
Maastricht University – FHML  
GitHub: [@Bardootjeb](https://github.com/Bardootjeb)
