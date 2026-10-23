# Energy Consumption Prediction

A regression analysis project focused on predicting building energy consumption using building characteristics and usage patterns.

### Project Overview
The dataset was cleaned and analyzed before developing and comparing multiple Ordinary Least Squares (OLS) regression models. The final model predicts **energy consumption (kWh)** using:

- Square Footage
- Number of Occupants
- Appliances Used
- Temperature
- Building Type
- Day Type (Weekday/Weekend)

### Key Steps
- Cleaned corrupted and missing data and removed extreme outliers.
- Performed exploratory data analysis and correlation analysis.
- Built and compared three OLS regression models.
- Used an **80/20 validation split** and **5-fold cross-validation** for model selection.
- Checked model assumptions including multicollinearity and heteroscedasticity.
- Applied the final model to a separate test set of 200 observations.

### Results
The selected model achieved:
- **Adjusted R²: 0.9999**
- **Cross-validation RMSE: 4.74 kWh**
- **Residual Standard Error: 7.64 kWh**
- All 200 test predictions remained within the observed training range.

The analysis also showed that **Square Footage** was the strongest continuous predictor, while **Day Type** had a major impact on predictive performance. Removing Day Type increased validation RMSE from **0.985 to 25.04**.

### Tools & Methods
**R · OLS Regression · Data Cleaning · Exploratory Data Analysis · Cross-Validation · Model Diagnostics · Statistical Analysis**
