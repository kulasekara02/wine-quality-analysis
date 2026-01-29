# Wine Quality Data Analysis

A comprehensive statistical analysis project demonstrating master's level R programming and data science techniques.

## Dataset

**Source:** [UCI Machine Learning Repository - Wine Quality Dataset](https://archive.ics.uci.edu/ml/datasets/wine+quality)

The dataset contains physicochemical properties of red wine samples and their quality ratings.

### Variables

| Variable | Description |
|----------|-------------|
| fixed.acidity | Fixed acidity (g/dm³) |
| volatile.acidity | Volatile acidity (g/dm³) |
| citric.acid | Citric acid (g/dm³) |
| residual.sugar | Residual sugar (g/dm³) |
| chlorides | Chlorides (g/dm³) |
| free.sulfur.dioxide | Free sulfur dioxide (mg/dm³) |
| total.sulfur.dioxide | Total sulfur dioxide (mg/dm³) |
| density | Density (g/cm³) |
| pH | pH level |
| sulphates | Sulphates (g/dm³) |
| alcohol | Alcohol content (% vol) |
| quality | Quality score (0-10) |

## Project Structure

```
my_r_project/
├── data/                    # Raw data files
│   └── wine_quality.csv
├── scripts/                 # Analysis scripts
│   ├── 00_run_all.R        # Master script to run all analyses
│   ├── 01_data_exploration.R
│   ├── 02_statistical_tests.R
│   ├── 03_pca_analysis.R
│   ├── 04_regression_analysis.R
│   └── 05_machine_learning.R
├── output/                  # Generated outputs
├── R/                       # Custom functions
├── docs/                    # Documentation
└── my_r_project.Rproj      # RStudio project file
```

## Analyses Performed

### 1. Exploratory Data Analysis (`01_data_exploration.R`)
- Descriptive statistics and data profiling
- Missing value analysis
- Distribution analysis (histograms, density plots)
- Outlier detection using IQR method
- Correlation analysis with heatmap visualization
- Bivariate analysis: Quality vs. predictors

### 2. Statistical Hypothesis Testing (`02_statistical_tests.R`)
- **Normality Tests:** Shapiro-Wilk, Anderson-Darling, Kolmogorov-Smirnov
- **Homogeneity of Variance:** Levene's Test
- **Non-parametric ANOVA:** Kruskal-Wallis Test
- **Post-hoc Analysis:** Dunn's Test with Bonferroni correction
- **Correlation Significance:** Pearson correlation with confidence intervals
- **Chi-Square Test:** Independence testing for categorical relationships

### 3. Principal Component Analysis (`03_pca_analysis.R`)
- Eigenvalue decomposition and scree plot
- Kaiser criterion for component selection
- Loadings analysis and interpretation
- Variable contribution to principal components
- Biplot visualization
- PC scores by quality groups with ANOVA

### 4. Multiple Regression Analysis (`04_regression_analysis.R`)
- Full model with all predictors
- Multicollinearity assessment (VIF)
- Stepwise model selection (Forward, Backward, Both)
- Regression diagnostics:
  - Residual normality (Shapiro-Wilk)
  - Homoscedasticity (Breusch-Pagan)
  - Autocorrelation (Durbin-Watson)
  - Influential observations (Cook's Distance)
- Model validation on test set
- 10-fold cross-validation
- Standardized coefficients (beta weights)

### 5. Machine Learning Classification (`05_machine_learning.R`)
- Binary classification: Good (≥6) vs. Poor (<6) quality
- Models implemented:
  - Logistic Regression
  - Random Forest
  - Support Vector Machine (Radial Kernel)
  - K-Nearest Neighbors
  - Gradient Boosting Machine
- 10-fold cross-validation with ROC-AUC optimization
- Hyperparameter tuning
- Model comparison and selection
- ROC curves and AUC analysis
- Feature importance ranking
- Confusion matrix evaluation

## How to Run

1. Open `my_r_project.Rproj` in RStudio
2. Run the master script:
   ```r
   source("scripts/00_run_all.R")
   ```

   Or run individual scripts in order:
   ```r
   source("scripts/01_data_exploration.R")
   source("scripts/02_statistical_tests.R")
   source("scripts/03_pca_analysis.R")
   source("scripts/04_regression_analysis.R")
   source("scripts/05_machine_learning.R")
   ```

## Required R Packages

```r
install.packages(c(
  "tidyverse", "corrplot", "skimr", "DataExplorer", "GGally",
  "nortest", "car", "lmtest", "rstatix", "ggpubr",
  "factoextra", "FactoMineR", "gridExtra",
  "MASS", "caret", "broom", "performance",
  "randomForest", "e1071", "pROC", "ROCR", "class", "nnet", "gbm"
))
```

## Key Findings

- **Alcohol** has the strongest positive correlation with wine quality
- **Volatile acidity** has the strongest negative correlation with quality
- PCA reveals that 5 components explain ~80% of variance
- Random Forest and Gradient Boosting achieve the best classification performance
- Multiple regression model achieves R² ≈ 0.36 for quality prediction

## Output Files

The `output/` directory contains:
- Visualization plots (PNG format)
- Statistical test results (CSV format)
- Model summaries and coefficients
- Saved model objects (RDS format)

## Author

Generated with Claude Code

## License

This project is for educational purposes. The Wine Quality dataset is from the UCI Machine Learning Repository.

## References

- Cortez, P., Cerdeira, A., Almeida, F., Matos, T., & Reis, J. (2009). Modeling wine preferences by data mining from physicochemical properties. Decision Support Systems, 47(4), 547-553.
