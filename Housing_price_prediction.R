library(lmtest)
library(ltm)
library(dplyr)
library(Rcpp)
library(reshape2)
library(ggplot2)


# Loading data
data = read.table("Housing.csv", header = TRUE, sep = ",")
dim(data)
names(data)
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

# Split data into train and test sets.
set.seed(123)
train_index = sample(c(1:length(data[,"price"])), length(data[,"price"]) * 0.7)
test_index = setdiff(c(1:length(data[,"price"])), train_index)

data_train = data[train_index,]
price_train = data_train$price

data_test = data[test_index,]
price_test = data_test$price
data_test$price = NULL

# Correlations
correlatinos <- round(cor(data_train[,continuous_variables]),2)
cors_melt <- melt(correlatinos)

ggplot(data = cors_melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation values") +
  labs(x = "", y = "", title = "Correlations Heatmap")

data_train$price = NULL

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

print(paste("Rmse train:", calculate_rmse(predictions_train, price.log)))
print(paste("Rmse test:", calculate_rmse(predictions_test, log1p(price_test))))

residuals <- price.log - predictions_train
hist(residuals)
shapiro.test(residuals)

qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)

dwtest(model)
bptest(model)

# Removing "furnishingstatus_unfurnished" variable
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

# Residuals histogram
residuals_mean <- mean(residuals)
ggplot(plot.data, aes(x=residuals)) + 
  geom_histogram(aes(y = ..density..), color="black", fill = "blue", bins = 40) +
  geom_vline(xintercept = residuals_mean, color = "red", size = 1) +
  geom_density(color = "green", size = 1.3) + 
  labs(x = "Residuals", y = "Density", title = "Histogram of residuals")

# Predicted vs Residuals plot
plot.data <- data.frame(residuals, predictions_train)
ggplot(plot.data, aes(x=predictions_train, y=residuals)) + geom_point(color="blue") +
  geom_hline(yintercept = 0, color="black") + labs(x = "Predicted values", y = "Residuals", title = "Predicted vs Residuals")+
  geom_smooth(color = "red")
