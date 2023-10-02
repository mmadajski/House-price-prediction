library(lmtest)
library(ltm)
library(dplyr)
library(Rcpp)

# Loading data
data = read.table("Housing.csv", header = TRUE, sep = ",")
View(data)
length(which(is.na(data))) # Check for missing data

# Set binary variables to 0 and 1 values instead of strings.
for (name in list("mainroad", "guestroom", "basement", "hotwaterheating", "prefarea", "airconditioning")) {
  data[,name] = ifelse(data[,name] == "yes", 1, 0) 
}

# One hot encoding "furnishingstatus" variable.
furnish_factor = factor(data[,"furnishingstatus"])

for (name in levels(furnish_factor)) {
  new_col = paste("furnishingstatus", name, sep = "_")
  data[, new_col] = ifelse(data$furnishingstatus == name, 1, 0)
}
data = data[,names(data) != "furnishingstatus"]

# Exploring data.
summary(data)
continuous_variables = c("price", "area", "bedrooms", "bathrooms", "stories", "parking")
for (column in continuous_variables) {
  hist(data[,column], main = paste("Histogram of:", column), xlab = column)
}

# Checking corelations.
# Point biserial correlations.
binary_variables = names(data)[!names(data) %in% continuous_variables]
for (bin_var in binary_variables) {
    for (con_var in continuous_variables) {
      print(paste("Corelation between", bin_var, "and", con_var, ":", biserial.cor(data[,con_var], data[,bin_var], use = c("all.obs"), level = 2)))
    }
    print("--------------")
}

# Correlations between variables are weak. Highest correlation equals to 0.452,
# on average correlations are around 0.3.

correlatinos <- round(cor(data[,continuous_variables]),2)
library(reshape2)
cors_melt <- melt(correlatinos)
library(ggplot2)
ggplot(data = cors_melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
  midpoint = 0, limit = c(-1,1), space = "Lab", 
  name="Pearson\nCorrelation")

# Correlations between continuous variables and price are quite weak at around 0.3-0.5.


# Split data into train and test sets.
set.seed(123)
train_index = sample(c(1:length(data[,"price"])), length(data[,"price"]) * 0.7)
test_index = setdiff(c(1:length(data[,"price"])), train_index)

data_train = data[train_index,]
price_train = data_train$price
data_train$price = NULL

data_test = data[test_index,]
price_test = data_test$price
data_test$price = NULL

# Standarization 
for (column in continuous_variables[continuous_variables != "price"]) {
  mean = mean(data_train[,column])
  std = sd(data_train[,column])
  
  data_train[,column] = (data_train[,column] - mean) / std
  data_test[,column] = (data_test[,column] - mean) / std
}

# Fitting model
price.log <- log1p(price_train)
model = lm(formula = price.log~., data=data_train)
summary(model)
plot(model)

# Making predictions and calculating metrics.
calculate_rmse = function(predictions, true_anwsers) {
  rmse = sqrt(mean((predictions - true_anwsers) ** 2))
  return (rmse)
}

predictions_train = predict(model, data_train)
predictions_test = predict(model, data_test)

print(paste("Rmse train:", calculate_rmse(predictions_train, log1p(price_train))))
print(paste("Rmse test:", calculate_rmse(predictions_test, log1p(price_test))))

residuals <- price.log - predictions_train
hist(residuals)
shapiro.test(residuals)
qqnorm(residuals)
qqline(c(2,2))
qqline(residuals, col = "red", lwd = 2)

dwtest(model)
bptest(model)

# As warning "prediction from a rank-deficient fit may be misleading" suggest,
# since furnishingstatus_unfurnished variable doesn't contribute in any way to model, 
# so this variable will be dropped. Bedrooms variable will be dropped as well because
# it isn't statistically important in model.

data_train$furnishingstatus_unfurnished = NULL
data_test$furnishingstatus_unfurnished = NULL
data_train$bedrooms = NULL
data_test$bedrooms = NULL

model = lm(formula = price.log~., data=data_train)
summary(model) 

predictions_train = predict(model, data_train)
predictions_test = predict(model, data_test)

print(paste("Rmse train:", calculate_rmse(predictions_train, price.log)))
print(paste("Rmse test:", calculate_rmse(predictions_test, log1p(price_test))))

residuals <- price.log - predictions_train
hist(residuals)
shapiro.test(residuals)
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)

plot(model)
dwtest(model)
bptest(model)

# In this case R-squared equals 0.7038 which means that relation between 
# price and variables explains 70.38% of variation in price.

# Shapiro-Wilk test p-value equals 0.05117 implying that residuals distribution
# is not significantly different from normal distribution.

# 
