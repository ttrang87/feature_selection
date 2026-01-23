# ShinyFeatures

**A Shiny App for Prioritizing Features for Machine Learning Models in Cancer Patients' Outcome Analysis**

🔗 **[Try the Live App](https://shawlab-moffitt.shinyapps.io/shiny_features/)**

---

## Overview

ShinyFeatures is an interactive R Shiny application that helps researchers identify and prioritize the most informative features for machine learning models in cancer outcome analysis. The app implements four feature selection algorithms and provides comprehensive model evaluation tools.

**Developed at Moffitt Cancer Center, Tampa, FL, USA**

---

## Why Feature Selection Matters

Outcome analysis is crucial for cancer prevention and treatment decisions. While machine learning models can explore patterns in high-dimensional patient data, training models with a large number of features is:
- Difficult and time-consuming
- Prone to overfitting
- Computationally expensive

**Solution**: ShinyFeatures identifies the most informative features, improving model performance and interpretability.

---

## Algorithms

| Algorithm | What It Does | Strength | Limitation |
|-----------|-------------|----------|------------|
| **Pearson Correlation** | Picks features most related to outcome | Simple and fast | Only captures linear patterns |
| **Random Forest** | Builds decision trees and ranks features by prediction importance | Captures complex patterns | Slow with big data |
| **Markov Blanket** | Finds the smallest set of features that fully explains the outcome | Eliminates redundant features | May fail causal effect evaluation with multiple Markov boundaries |
| **LASSO** | Removes weak features automatically through regularization | Automatic feature selection | Works best for linear patterns |

---

## App Workflow

<p align="center">
  <img src="workflow.png" alt="ShinyFeatures Workflow" width="600">
</p>

### Pipeline Steps

1. **Upload dataset** - CSV/TSV format, single file or separate train/test files
2. **Select outcome and predictors** - Choose target variable and feature columns
3. **Pre-process data** - Handle missing values, imputation, feature engineering
4. **Run 4 algorithms** - Pearson Correlation, Random Forest, Markov Blanket, LASSO
5. **Result** - Overlapping features across methods + model evaluation

### Data Transformation

| Stage | Before | After |
|-------|--------|-------|
| **Input** | Raw CSV/TSV with mixed types | Cleaned numeric dataset |
| **Missing Data** | NA values present | Imputed values |
| **Categorical** | Text categories | Numeric encoding |
| **Target** | Multi-class or continuous | Binary (0/1) |

---

## Key Features

- **Data Processing**: Upload single or dual datasets, map targets to binary, customizable data preprocessing
- **Feature Selection**: Run 4 algorithms simultaneously with customizable parameters
- **Model Evaluation**: Test with Logistic Regression, Random Forest, and XGBoost
- **Visualization**: Correlation plots, importance rankings, ROC curves, SHAP values

---

## Key Findings

When tested on CCLE cancer cell line data:
- **chrYp11** and **chrYq11** identified as top features across all four algorithms
- Features selected by 1-3 algorithms included negative controls (lower reliability)

---

## Contact

**Authors**: Thuy Trang Ta, Timothy I Shaw, Yi Luo, Alyssa Obermayer

**For questions or feedback:**
- Dr. Timothy Shaw - Timothy.Shaw@moffitt.org
- Dr. Yi Luo - Yi.Luo@moffitt.org
- Thuy Trang Ta - trangta@usf.edu

**Institution**: Moffitt Cancer Center, Tampa, FL, USA
