### Lab 5 SVM ###

library(ggplot2)
library(ggfortify)
library(e1071)
library(class)
library(caret)

wine_data <- read.csv("/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/wine/wine.data", header = FALSE)
names(wine_data) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine_data)


# Fit a linear mmodel 
wine_lm <- lm(Type ~ ., data = wine_data)
summary(wine_lm)

# Extract significant variables based on p-value < 0.05
sig_vars <- summary(wine_lm)$coefficients
sig_vars <- sig_vars[sig_vars[, "Pr(>|t|)"] < 0.05, , drop = FALSE]

# Print significant variables and extract 5 most significant variables
print("Significant variables based on p-values:")
print(sig_vars[order(sig_vars[, "Pr(>|t|)"]), ])

wine_data$Type <- as.factor(wine_data$Type)

# Subset the dataset with only the top 5 variables and Type
wine_data_subset <- wine_data[,c(1,5,8,11,13,14)]

# Split into train/test datasets
set.seed(42)
train.indexes <- sample(1:nrow(wine_data_subset), size = 0.7 * nrow(wine_data_subset))

train <- wine_data_subset[train.indexes,]
test <- wine_data_subset[-train.indexes,]

# Preprocess the training data 
train_X <- train[, -1]  
train_Y <- train$Type
test_X <- test[, -1]   
test_Y <- test$Type

preproc <- preProcess(train_X, method = c("center", "scale"))
train_X_scaled <- predict(preproc, train_X)
test_X_scaled <- predict(preproc, test_X)

# Combine scaled predictors with the target variable
train_scaled <- data.frame(Type = train_Y, train_X_scaled)
test_scaled <- data.frame(Type = test_Y, test_X_scaled)

# Train SVM with a linear kernel
tune_linear <- tune.svm(
  Type ~ ., data = train_scaled, kernel = "linear",
  cost = 2^(-2:10),
  tunecontrol = tune.control(cross = 5)
)
tune_linear

# Best linear model
best_linear <- tune_linear$best.model
best_linear

# Train SVM with a radial basis function (RBF) kernel
set.seed(123)
tune_rbf <- tune.svm(
  Type ~ ., data = train_scaled, kernel = "radial",
  cost = 2^(-2:10), gamma = 2^(-2:10),
  tunecontrol = tune.control(cross = 5)
)
tune_rbf

# Best RBF model
best_rbf <- tune_rbf$best.model
best_rbf

# Evaluate both models on the scaled test dataset
linear_predictions <- predict(best_linear, test_scaled)
linear_accuracy <- mean(linear_predictions == test_scaled$Type)

rbf_predictions <- predict(best_rbf, test_scaled)
rbf_accuracy <- mean(rbf_predictions == test_scaled$Type)

cat("\nLinear Kernel SVM Accuracy:", linear_accuracy, "\n")
cat("RBF Kernel SVM Accuracy:", rbf_accuracy, "\n")

## Choose another classification method (kNN, NaiveBayes, etc.) and train a classifier
## based on the same features.

set.seed(123)
tune_knn <- train(
  Type ~ ., data = train_scaled, method = "knn",
  tuneGrid = expand.grid(k = 1:20),
  trControl = trainControl(method = "cv", number = 10)
)
best_k <- tune_knn$bestTune$k
cat("Optimal k for kNN:", best_k, "\n")

# Predict using the kNN model
knn_predictions <- knn(
  train = train_X_scaled,
  test = test_X_scaled,
  cl = train_Y,
  k = best_k
)

knn_accuracy <- mean(knn_predictions == test_Y)
cat("kNN Accuracy:", knn_accuracy, "\n")

# Plot kNN accuracy vs. k
k_values <- 1:20
cv_accuracies <- tune_knn$results$Accuracy

plot(
  k_values, cv_accuracies,
  type = "b",
  xlab = "k",
  ylab = "Cross-Validation Accuracy",
  main = "k Selection for kNN"
)

# Highlight the best k
points(best_k, max(cv_accuracies), col = "red", pch = 19)
text(best_k, max(cv_accuracies), labels = paste("Best k =", best_k), pos = 3, col = "red")

## Compare the performance of the models

# Function to compute Precision, Recall, and F1
compute_metrics_table <- function(predictions, true_labels) {
  contingency_table <- table(Predicted = predictions, Actual = true_labels)
  # Metrics calculations
  n <- sum(contingency_table) 
  diag <- diag(contingency_table) 
  rowsums <- apply(contingency_table, 1, sum) 
  colsums <- apply(contingency_table, 2, sum) 
  
  precision <- diag / colsums 
  recall <- diag / rowsums 
  f1 <- 2 * precision * recall / (precision + recall) 
  
  # Create a data frame with the metrics
  metrics <- data.frame(
    Class = names(precision),
    Recall = recall,
    Precision = precision,
    F1 = f1
  )
  
  return(list(ContingencyTable = contingency_table, Metrics = metrics))
}

# Compute metrics for each model
linear_metrics <- compute_metrics_table(linear_predictions, test_Y)
rbf_metrics <- compute_metrics_table(rbf_predictions, test_Y)
knn_metrics <- compute_metrics_table(knn_predictions, test_Y)

### Print Results ###
cat("\nLinear SVM Contingency Table:\n")
print(linear_metrics$ContingencyTable)
#write.csv(linear_metrics$ContingencyTable, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 5/contingency_table_linear_SVM.csv")
cat("\nLinear SVM Metrics:\n")
print(linear_metrics$Metrics)
#write.csv(linear_metrics$Metrics, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 5/linear_SVM_model_performance.csv")

cat("\nRBF SVM Contingency Table:\n")
print(rbf_metrics$ContingencyTable)
#write.csv(rbf_metrics$ContingencyTable, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 5/contingency_table_radial_SVM.csv")
cat("\nRBF SVM Metrics:\n")
print(rbf_metrics$Metrics)
#write.csv(rbf_metrics$Metrics, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 5/radial_SVM_model_performance.csv")

cat("\nkNN Contingency Table:\n")
print(knn_metrics$ContingencyTable)
#write.csv(knn_metrics$ContingencyTable, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 5/contingency_table_KNN.csv")
cat("\nkNN Metrics:\n")
print(knn_metrics$Metrics)
#write.csv(knn_metrics$Metrics, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 5/KNN_model_performance.csv")

## 2. Using the NY House dataset, train a SVM regression model to predict price based on 
## Square Footage and plot predicted vs price.

# Read in the csv
house_data <- read.csv("/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 5/NY-House-Dataset.csv")

# Clean up the high outliers by removing everything above the 99th percentile
upper_bound <- quantile(house_data$PRICE, 0.99, na.rm = TRUE)
house_data_clean <- house_data[house_data$PRICE <= upper_bound, ]

# Remove rows with missing values
house_data_clean <- na.omit(house_data_clean)

# Apply log transformation and save as new variables
house_data_clean$LOG_PRICE <- log(house_data_clean$PRICE)
house_data_clean$LOG_SQUARE_FOOTAGE <- log(house_data_clean$PROPERTYSQFT)

# Separate data into training and testing data
set.seed(123)
train_indices <- sample(1:nrow(house_data_clean), 0.7 * nrow(house_data_clean))
train_data <- house_data_clean[train_indices, ]
test_data <- house_data_clean[-train_indices, ]

# Tune the SVM model to find the ideal parameters
tuned_svm <- tune.svm(
  LOG_PRICE ~ LOG_SQUARE_FOOTAGE,
  data = train_data,
  kernel = "radial",
  cost = 10^(-1:2),  
  gamma = 10^(-3:0)  
)

print(tuned_svm)

# Train the optimized SVM model
optimized_svm_model <- tuned_svm$best.model

# Predict on the test set
test_data$Predicted_LOG_PRICE <- predict(optimized_svm_model, newdata = test_data)

# Plot predicted vs actual prices in log space
ggplot(test_data, aes(x = LOG_PRICE, y = Predicted_LOG_PRICE)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  ggtitle("Predicted vs Actual Log Prices (Optimized SVM)") +
  xlab("Actual Log Price") +
  ylab("Predicted Log Price") +
  theme_minimal()

# Calculate R-squared
rss <- sum((test_data$Predicted_LOG_PRICE - test_data$LOG_PRICE)^2) 
tss <- sum((test_data$LOG_PRICE - mean(test_data$LOG_PRICE))^2) 
r_squared <- 1 - (rss / tss)
cat("\nR-squared on log-transformed scale:", r_squared, "\n")

## Train a Linear Regression model
linear_model <- lm(LOG_PRICE ~ LOG_SQUARE_FOOTAGE, data = train_data)

# Predict on the test set
test_data$Predicted_LOG_PRICE_LM <- predict(linear_model, newdata = test_data)

# Plot predicted vs actual log prices
ggplot(test_data, aes(x = LOG_PRICE, y = Predicted_LOG_PRICE_LM)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  ggtitle("Predicted vs Actual Log Prices (Linear Model)") +
  xlab("Actual Log Price") +
  ylab("Predicted Log Price") +
  theme_minimal()

# Calculate R-squared for Linear Model
rss_lm <- sum((test_data$Predicted_LOG_PRICE_LM - test_data$LOG_PRICE)^2) # Residual sum of squares
tss_lm <- sum((test_data$LOG_PRICE - mean(test_data$LOG_PRICE))^2) # Total sum of squares
r_squared_lm <- 1 - (rss_lm / tss_lm)

cat("R-squared for Linear Model on log-transformed scale:", r_squared_lm, "\n")











  