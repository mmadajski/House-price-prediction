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

continuous_variables = c("price", "area", "bedrooms", "bathrooms", "stories", "parking")
for (column in continuous_variables) {
  hist(data[,column], main = paste("Histogram of:", column), xlab = column)
}

# Checking corelations.
install.packages("ltm")
library(ltm)

# Point biserial correlations.
binary_variables = names(data)[!names(data) %in% continuous_variables]
for (bin_var in binary_variables) {
    for (con_var in continuous_variables) {
      print(paste("Corelation between", bin_var, "and", con_var, ":", biserial.cor(data[,con_var], data[,bin_var], use = c("all.obs"), level = 2)))
    }
    print("--------------")
}

# Corelations between is weak. Highest correlation equals to 0.452,
# on average correlation are around 0.3.

correlatinos <- round(cor(data[,continuous_variables]),2)
library(reshape2)
cors_melt <- melt(correlatinos)
library(ggplot2)
ggplot(data = cors_melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
  midpoint = 0, limit = c(-1,1), space = "Lab", 
  name="Pearson\nCorrelation")

# Correlations between continuous variables and price are quite weak at around 0.3-0.5.

library(dplyr)
install.packages('Rcpp')
library(Rcpp)
train_index = sample(c(1:length(data[,"price"])), length(data[,"price"]) * 0.7)
test_index = setdiff(c(1:length(data[,"price"])), train_index)

data_train = data[train_index,]
price_train = data_train$price
data_train$price = NULL

data_test = data[test_index,]
price_test = data_test$price
data_test$price = NULL

for (column in continuous_variables[continuous_variables != "price"]) {
  mini = min(data_train[,column])
  maxi = max(data_train[,column])
  
  data_train[,column] = (data_train[,column] - mini) / (maxi - mini)
  data_test[,column] = (data_test[,column] - mini) / (maxi - mini)
}
View(data_train)

model = lm(formula = price_train~., data=data_train)
summary(model)
model$coefficients

# Making predictions and calculating metrics.
calculate_rmse = function(predictions, true_anwsers) {
  rmse = sqrt(mean((predictions - true_anwsers) ** 2))
  return (rmse)
}

predictions_train = predict(model, data_train)
predictions_test = predict(model, data_test)

print(paste("Rmse train:", calculate_rmse(predictions_train, price_train)))
print(paste("Rmse test:", calculate_rmse(predictions_test, price_test)))

# As warning "prediction from a rank-deficient fit may be misleading" suggest,
# since furnishingstatus_unfurnished variable doesn't contribute in any way to model, 
# so this variable will be dropped.

data_train$furnishingstatus_unfurnished = NULL
data_test$furnishingstatus_unfurnished = NULL

model = lm(formula = price_train~., data=data_train)

predictions_train = predict(model, data_train)
predictions_test = predict(model, data_test)

print(paste("Rmse train:", calculate_rmse(predictions_train, price_train)))
print(paste("Rmse test:", calculate_rmse(predictions_test, price_test)))

summary(model)

# In this case R-squared equals 0.6637 which means that relation between 
# price and training variables explains 66.37% of variation in data.
# R-square is to small to build good model.
# High values of rsme in train and test data (1083651 and 992861 respectively) confirms that. 
