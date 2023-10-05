# House price prediction
Due to the large number of factors affecting the price of housing, there was a need to simplify their valuation. Using the given data
a linear regression model was created, then it was checked whether the model meets the assumptions. Performance of the created model was evaluated using $RSME$ and $R^2$.

*Technologies used*: R, ggplot2, lmtest.

Project was based on Housing Price Prediction data from the Kaggle platform: https://www.kaggle.com/datasets/harishkumardatalab/housing-price-prediction
 
## Table of contents
* [Data preperation](#data-preperation)
* [Correlations](#correlations)
* [Model](#model)
* [Model assumptions](#model-assumptions)
* [Summary](#summary)

# Data preperation 

The first stage in this project was to load and prepare data. The data consisted of 545 observations and 13 variables. There were no missing values in the dataset.
Among the continuous variables were "price", "area", "bedrooms", "bathrooms", "stories" and "parking" ("price" is the target varaible). There were also binary variables "mainroad", "guestroom", "basement", "hotwaterheating", "airconditioning" and "prefarea". The dataset contained one categorical varaible "furniching-status" with 4 possible values, which was recoded using one-hot encoding into 4 new binary variables. As a result, the dataset had 15 variables. 

The dataset was split into training and test datasets (70%-30% split). 
After that continuous varaibles were standardized.

The price variable was additionally transformed using $ln(1+x)$ function to make the residuals distribution closer to normal.
# Correlations

Before creating the model correlations between continuous were calculated. 

 ![alt](https://github.com/mmadajski/House-price-prediction/blob/main/Images/correlations.png?raw=true)

As we can see from the correlation heatmap, there wasn't any significant correlation. No multicollinearity was detected.

# Model 

Then a linear regression model was fitted to the previously prepared data. All varaibles except for the "furnishingstatus_unfurnished" variable were statiscally significant in the model. The adjusted $R^2$ metric equals 0.703 which means that 70,3% of variation in the dependent variable can be explained by the independent variable variation. The RSME metric was equal to 0.233. 

# Model assumptions

### Normal Distribution of error terms

The p-value of the Shapiro-Wilk test is 0.051, implying that the distribution of the residuals is not significantly different from the normal distribution.
The Histogram of residuals 

![alt](https://github.com/mmadajski/House-price-prediction/blob/main/Images/Residuals_Hist.png?raw=true)

### Autocorrelation

The p-value of the Durbin-Watson test is 0.697, which indicates no autocorrelation.

### Heteroskedasticity

The p-value of the studentized Breusch-Pagan test is 0.0039, implying that the homoskedasticity not is present.
We can also see that on a predicted vs fittet values plot where no trend can be seen. 

![alt](https://github.com/mmadajski/House-price-prediction/blob/main/Images/Pred_vs_Fit.png?raw=true)

# Summary 

To facilitate the valuation of housing, a linear model was created.
The prepared model meets all the assumptions that a linear model should meet.
Although the $RSME$ and $R^2$ metrics may not be satisfactory.





