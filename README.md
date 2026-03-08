# Data Driven Decision Support 2 – Business Analytics
### Karl Franzens University of Graz

---

## Overview

This repository contains the R code used in the course **"Data Driven Decision Support 2 – Business Analytics"**. The goal of the course was to teach students how to develop simple machine learning models end-to-end, covering the full data science lifecycle: data cleaning, exploratory data analysis, and predictive modelling.

**Data source:** The dataset was scraped from the German real estate platform [Immoscout24](https://www.immoscout24.de/) in spring 2019 and spring 2020. Each observation represents an apartment listed in Germany and includes attributes such as rent, living space, number of rooms, heating type, construction year, and location. Due to potential legal considerations, the web scraping code is not included in this repository.

---

## Usage

To run the code in this repository, follow these steps:

1. **Download the repository** – Download the repository as a ZIP file using the *Code → Download ZIP* button on GitHub.
2. **Unzip the files** – Extract the contents of the ZIP file to a folder of your choice.
3. **Install required libraries** – Open R or RStudio and install all required packages before running the scripts. The main dependencies are:
   ```r
   install.packages(c("tidyverse", "caTools", "caret", "stargazer", "car"))
   ```
4. **Run the R scripts** – Open the scripts in RStudio and run them in order (01 → 02 → 03), as each script builds on the output of the previous one.

---

## Files

### `01_Exploratory_Data_Analysis.R`

> The core concepts covered in this script are explained in the accompanying presentation **`01_DataVisualization.pdf`**.

This script introduces students to visual data exploration using the raw dataset. It demonstrates how to detect potential data quality issues and understand relationships between variables before modelling.

The script covers:

1. **Distributional plots** – Histograms and box plots for `totalRent` (single and grouped by `heatingType`, `hasKitchen`), and a multi-variable box plot comparing `totalRent`, `serviceCharge`, and `baseRent`.
2. **Pie charts** – Used to visualise the distribution of categorical variables such as `heatingType`.
3. **Scatter plots** – Plots `livingSpace` against `totalRent` to identify linear trends; extends this to a three-variable scatter plot by mapping `noRooms` to point size.
4. **Pairwise scatter plot matrix** – Uses `pairs()` on a selection of numeric variables for a quick overview of all bivariate relationships.
5. **Faceted histograms** – Displays the distribution of `totalRent` separately for each heating type, using `facet_grid()`.

---

### `02_Data_Cleaning.R`

> The core concepts covered in this script are explained in the accompanying presentation **`02_DataCleaningAndManagement.pdf`**.

This script takes the raw dataset (`Dataset_Teachers.csv`) and produces a cleaned version ready for analysis. Students were instructed to filter the data to their assigned city; the provided example uses **Mittelsachsen_Kreis**.

The script covers five data cleaning steps:

1. **Redundant Data** – Selects only the relevant variables (e.g. `totalRent`, `livingSpace`, `heatingType`) and removes duplicate observations.
2. **Missing Values** – Standardises non-standard missing value labels (e.g. `"no_information"` → `NA`) and removes incomplete observations using `na.omit()`.
3. **Outliers** – Detects and removes outliers for `totalRent` using two methods:
   - *Z-score method*: removes observations more than 3 standard deviations from the mean.
   - *IQR method*: removes observations outside 1.5× the interquartile range.
4. **Inconsistencies** – Identifies and removes observations where `totalRent ≠ baseRent + serviceCharge`.
5. **Feature Engineering** – Performs two additional transformations:
   - `yearConstructed` is recoded into a categorical variable (`buildingType`) with three groups: *Altbau* (pre-1939), *Mid-Life-Crisis* (1940–1950), and *Neubau* (post-1950).
   - Rare districts in `regio3` with very few observations are consolidated into an *"Other"* category to reduce the number of dummy variables in later models.

The cleaned dataset is exported as `Cleaned_Dataset_Mittelsachsen_Kreis.csv`.

---

### `03_Regressions_and_Predictions.R`

> The core concepts covered in this script are explained in the accompanying presentation **`03_RegressionAnalysis.pdf`**.

This script covers the modelling stage of the project. It loads the cleaned dataset and walks students through training, evaluating, and critically assessing linear regression models.

The script covers:

1. **Train/test split** – Splits the cleaned data 80/20 into training and test sets using `sample.split()` with a fixed seed for reproducibility.
2. **Correlation analysis** – Uses `pairs()` and `cor()` on numeric variables to assess linearity prior to modelling.
3. **Model training** – Estimates three OLS regression models:
   - `model1`: `totalRent ~ livingSpace` (simple regression)
   - `model2`: `totalRent ~ yearConstructed` (simple regression)
   - `model3`: `totalRent ~ livingSpace + yearConstructed` (multivariate regression)
   - Results are displayed side-by-side using `stargazer`.
4. **Prediction and error metrics** – Generates predictions on the test set and calculates the **Mean Absolute Percentage Error (MAPE)** and **Sales Ratio (SR)**.
5. **k-fold cross-validation** – Uses 3-fold CV (`caret::createFolds`) to evaluate the generalisation performance of the model, reporting MAPE and SR for each fold.
6. **Assumption testing** – Tests the key assumptions of OLS regression:
   - *Linearity*: scatter plots
   - *Independence of errors*: correlation of residuals with predictors, chi-squared test
   - *Multicollinearity*: Variance Inflation Factors (VIF) via `car::vif()`
   - *Homoscedasticity*: residuals vs. fitted values plot
   - *Normality of residuals*: density plot of residuals
   - *Non-zero predictor variance*: standard deviations of predictors
7. **Robustness check** – Re-trains the same model on the raw (uncleaned) dataset and compares its MAPE and SR against the model trained on the clean data, illustrating the practical value of the data cleaning step.
8. **Influence diagnostics** – Identifies observations with a disproportionate impact on the model using **Cook's distance** and **leverage (hat values)**.
