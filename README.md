# Dummy Variable Regression of GTBank Revenue

## Overview

This project demonstrates the application of **dummy variable regression** to analyse the factors associated with GTBank revenue.

Dummy variable regression is a form of regression analysis that allows **categorical variables** to be incorporated into a regression model by representing different categories with binary indicator variables. This makes it possible to examine how membership in different categories is associated with a numerical outcome while controlling for other explanatory variables.

In this project, regression techniques are applied to **GTBank revenue data**, with the data transformed using a logarithmic transformation before the regression analysis.

The project was developed using **R** and includes the complete dataset, analysis script, and documented results.

## Objectives

The main objectives of this project are to:

* Analyse GTBank revenue using regression techniques.
* Demonstrate the use of dummy variables in regression analysis.
* Incorporate categorical information into a regression model.
* Examine the relationship between explanatory variables and GTBank revenue.
* Apply logarithmic transformation to the revenue data.
* Estimate and interpret regression coefficients.
* Evaluate the statistical significance of the fitted regression model.
* Present and interpret the results of the analysis.

## Key Concept: Dummy Variable Regression

A dummy variable is a binary variable that represents membership in a particular category.

For example, if a categorical variable has two categories, it can be represented using a variable taking values of:

```text
0 = Reference category
1 = Alternative category
```

A regression model containing a dummy variable can be expressed generally as:

```text
Y = β₀ + β₁X₁ + β₂D + ε
```

where:

* `Y` = dependent variable.
* `X₁` = continuous explanatory variable.
* `D` = dummy variable.
* `β₀` = intercept.
* `β₁` = coefficient of the continuous variable.
* `β₂` = effect associated with the dummy variable relative to the reference category.
* `ε` = random error term.

The coefficient of a dummy variable therefore provides information about how the expected value of the dependent variable differs between categories, holding the other variables constant.

## Dataset

The project uses:

`log_transformed_gtbank_data.csv`

This dataset contains the transformed GTBank revenue data used in the regression analysis.

A logarithmic transformation is useful in regression analysis when the original variable has characteristics such as substantial variation in magnitude or a skewed distribution. Transforming the dependent variable can also make relationships easier to model and interpret in some applications.

## Repository Structure

```text
dummy_variable_regression/
│
├── DUMMY VARIABLE ANALYSIS RESULTS.pdf
├── DUMMY VARIABLE REGRESSION ANALYSIS.R
├── log_transformed_gtbank_data.csv
└── README.md
```

### `log_transformed_gtbank_data.csv`

Contains the log-transformed GTBank revenue data used for the regression analysis.

### `DUMMY VARIABLE REGRESSION ANALYSIS.R`

The main R script used to perform the dummy variable regression analysis.

The script contains the statistical procedures required to prepare the data, fit the regression model, and obtain the analytical results.

### `DUMMY VARIABLE ANALYSIS RESULTS.pdf`

Contains the documented results and statistical output from the regression analysis.

## Methodology

The project follows a structured regression analysis workflow.

### 1. Data Preparation

The GTBank revenue data is imported into R and prepared for statistical analysis.

The revenue variable has been transformed logarithmically, resulting in the dataset:

`log_transformed_gtbank_data.csv`

### 2. Identification of Variables

The relevant dependent and explanatory variables are identified before fitting the regression model.

The dependent variable represents the GTBank revenue outcome, while the explanatory variables include the relevant continuous and categorical information used in the analysis.

### 3. Dummy Variable Coding

Categorical variables are represented using dummy variables so that they can be included in the regression model.

One category is treated as the **reference category**, while the remaining category or categories are represented by indicator variables.

This allows the estimated coefficients to be interpreted relative to the selected reference group.

### 4. Regression Model

A multiple linear regression framework is used to estimate the relationship between GTBank revenue and the explanatory variables.

The general model can be represented as:

```text
log(Revenue) = β₀ + β₁X₁ + β₂X₂ + ... + βₖDₖ + ε
```

where the `D` terms represent dummy variables for categorical predictors.

### 5. Model Estimation

The regression model is fitted in R using the prepared dataset.

The estimated coefficients are examined to determine the direction and magnitude of the relationships between the predictors and the transformed revenue variable.

### 6. Statistical Inference

The regression output is examined using statistical measures such as:

* Regression coefficients.
* Standard errors.
* t-statistics.
* p-values.
* R-squared.
* Adjusted R-squared.
* Overall model significance.

These measures provide information about the explanatory power and statistical significance of the fitted model.

### 7. Interpretation

The estimated coefficients are interpreted in the context of GTBank revenue.

Particular attention is given to the interpretation of dummy variable coefficients, since their meaning depends on the reference category used in the regression model.

## Analysis Workflow

The overall analytical process can be summarized as:

```text
GTBank Revenue Data
        ↓
Data Preparation
        ↓
Logarithmic Transformation
        ↓
Variable Identification
        ↓
Dummy Variable Coding
        ↓
Regression Model Specification
        ↓
Model Estimation in R
        ↓
Statistical Testing
        ↓
Model Evaluation
        ↓
Interpretation of Results
```

## Tools and Technologies

The project primarily uses **R** for statistical analysis.

### Technologies

* **R**
* **RStudio**
* **CSV**
* **PDF**

### Statistical Techniques

* Multiple linear regression.
* Dummy variable regression.
* Logarithmic transformation.
* Coefficient estimation.
* Hypothesis testing.
* Model significance testing.
* Regression model evaluation.

## How to Run the Project

### 1. Clone the Repository

```bash
git clone https://github.com/PhenomCorp12/dummy_variable_regression.git
```

### 2. Navigate to the Project Directory

```bash
cd dummy_variable_regression
```

### 3. Open the R Script

Open:

```text
DUMMY VARIABLE REGRESSION ANALYSIS.R
```

using **RStudio** or another compatible R environment.

### 4. Ensure the Dataset Is Available

Make sure:

```text
log_transformed_gtbank_data.csv
```

is available in the working directory or that the file path specified in the R script points to the correct location.

### 5. Install Required R Packages

If packages required by the script are not already installed, install them using R's package manager.

For example:

```r
install.packages("readr")
```

Additional packages should be installed according to the requirements of the analysis script.

### 6. Run the Analysis

Execute the R script sequentially to reproduce the regression analysis and generate the corresponding statistical output.

## Results

The results of the dummy variable regression analysis are documented in:

`DUMMY VARIABLE ANALYSIS RESULTS.pdf`

The analysis provides statistical information that can be used to assess:

* The estimated effects of the explanatory variables.
* The contribution of categorical variables.
* The significance of individual regression coefficients.
* The overall explanatory power of the regression model.
* The statistical significance of the fitted model.

The results should be interpreted according to the reference categories used when constructing the dummy variables.

## Interpretation of Dummy Variables

One of the main purposes of this project is to demonstrate how categorical variables can be incorporated into a regression model.

Suppose a categorical variable has two groups:

```text
Reference Group → D = 0
Comparison Group → D = 1
```

If the estimated coefficient for `D` is `β₂`, then the expected difference between the comparison group and the reference group, conditional on the other variables in the model, is represented by `β₂`.

When the dependent variable is logarithmically transformed, interpretation of the coefficient should account for the log scale. For relatively small coefficients, the percentage interpretation can be approximated by:

```text
Percentage change ≈ 100 × β₂
```

For larger coefficients, a more precise interpretation uses:

```text
Percentage change = 100 × (e^β₂ − 1)
```

This distinction is important when interpreting regression results involving a log-transformed dependent variable.

## Why Dummy Variables Are Important

Many real-world datasets contain categorical information such as:

* Gender.
* Region.
* Business category.
* Department.
* Location.
* Economic period.
* Company classification.

These variables cannot always be entered directly into a standard numerical regression model. Dummy variable coding provides a way to convert categorical information into numerical indicators that can be incorporated into the regression framework.

This makes dummy variable regression particularly useful in economics, finance, business analytics, social sciences, and other applied statistical fields.

## Applications

The techniques demonstrated in this project can be applied to:

* Financial analysis.
* Revenue modelling.
* Business forecasting.
* Economic research.
* Market analysis.
* Banking and financial services.
* Employee and organizational studies.
* Consumer behaviour analysis.
* Policy evaluation.
* Academic research.

## Limitations

The interpretation of the regression results depends on the quality and structure of the underlying GTBank revenue data.

Important considerations include:

* The number of observations available.
* The variables included in the model.
* The choice of reference categories.
* The assumptions of linear regression.
* Potential multicollinearity between explanatory variables.
* Potential heteroscedasticity.
* The effect of logarithmic transformation on interpretation.
* Possible omitted variables that may influence revenue.

Therefore, the regression results should be interpreted as statistical relationships within the analysed dataset rather than automatically implying causation.

## Future Improvements

The project can be extended by incorporating additional regression diagnostics and modelling techniques, including:

* Residual analysis.
* Tests for heteroscedasticity.
* Multicollinearity diagnostics using VIF.
* Normality assessment of residuals.
* Autocorrelation testing for time-dependent data.
* Interaction terms between dummy and continuous variables.
* Model selection techniques.
* Robust standard errors.
* Alternative regression specifications.
* Out-of-sample model validation.

These extensions would provide a more comprehensive assessment of the reliability and predictive performance of the regression model.

## Project Purpose

This repository serves as a practical demonstration of **dummy variable regression using R**, with an application to GTBank revenue data.

It is suitable for:

* Statistical learning.
* Regression analysis practice.
* Econometric analysis.
* Financial data analysis.
* R programming practice.
* Academic research.
* Understanding categorical variables in regression.

## Author

**Phenom Corporation**

Live Link:
[Google Drive](https://drive.google.com/file/d/1deNd_P9xLrNlOEaSubKNoLYhIIRYN6UF/view)

## License

This project is intended for **educational, research, and statistical analysis purposes**.
