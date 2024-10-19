## 1. Variable Distributions ##

library(ggplot2)

# Read in data
Epi2024_results <- read.csv("~/GitHub/Data_Analytics_2024/Lab 3/epi2024results_DA_F24_lab03.csv")
# Copy variables
Epi2024_data <- read.csv("~/GitHub/Data_Analytics_2024/Lab 3/epi2024results_DA_F24_lab03.csv")

# Check structure
str(Epi2024_data)

# Check for NA values and filter out NA values for both regions
latin_caribbean <- subset(Epi2024_data, region == "Latin America & Caribbean" & !is.na(SPI))
africa <- subset(Epi2024_data, region == "Sub-Saharan Africa" & !is.na(SPI))
latin_caribbean_2 <- subset(Epi2024_data, region == "Latin America & Caribbean" & !is.na(EPI))
africa_2 <- subset(Epi2024_data, region == "Sub-Saharan Africa" & !is.na(EPI))

# 1.1) Plot histograms for SPI with density lines overlayed for both regions
summary(latin_caribbean$SPI)
hist(latin_caribbean$SPI, 
     main = "Histogram for Latin America & Caribbean - SPI",
     seq(0., 100., 3),
     xlab = "SPI", 
     col = "blue",
     prob=TRUE)  
lines(density(latin_caribbean$SPI), col = "red", lwd = 2)

summary(africa$SPI)
hist(africa$SPI, 
     main = "Histogram for Sub-Saharan Africa - SPI",
     seq(0., 100., 3),
     xlab = "SPI", 
     col = "green",
     prob=TRUE)  
lines(density(africa$SPI), col = "red", lwd = 2)

# 1.2) Plot QQ plots for both variables compared to known probability distributions 

# QQ Plot for Latin America & Caribbean - SPI vs normal distribution
qqnorm(latin_caribbean$SPI, main = "QQ Plot for Latin America & Caribbean - SPI")
qqline(latin_caribbean$SPI, col = "red")

# QQ Plot for Sub-Saharan Africa - SPI  vs normal distribution
qqnorm(africa$SPI, main = "QQ Plot for Sub-Saharan Africa - SPI")
qqline(africa$SPI, col = "red")

# QQ Plot for Latin America & Caribbean - EPI vs normal distribution
qqnorm(latin_caribbean_2$EPI, main = "QQ Plot for Latin America & Caribbean - EPI")
qqline(latin_caribbean_2$EPI, col = "red")

# QQ Plot for Sub-Saharan Africa - EPI  vs normal distribution
qqnorm(africa_2$EPI, main = "QQ Plot for Sub-Saharan Africa - EPI")
qqline(africa_2$EPI, col = "red")


## Linear Models ##


# 2.1. Choose a subset of 5 variables (excluding EPI) and using the formula EPI~VAR1+VAR2+VAR3+
#VAR4+VAR5, fit a linear model and identify which variable most significantly influences EPI. Plot that variable
#with another and overlay the fitted line.


# Remove Na values from variables 
Epi_filtered <- na.omit(Epi2024_data[, c("EPI", "SPI", "ECO", "BDH", "MKP", "MHP")])

# Fit the linear model
epi_lm <- lm(EPI ~ SPI + ECO + BDH + MKP + MHP, Epi_filtered)
plot(EPI ~ SPI + ECO + BDH + MKP + MHP)

# Display summary to see which variable is most significant
summary(epi_lm)

# Plot the most significant variable that influences EPI
plot(Epi_filtered$ECO, Epi_filtered$EPI, main = "EPI vs ECO", xlab = "ECO", ylab = "EPI")

# Add the fitted line for EPI ~ SPI
abline(lm(EPI ~ ECO, Epi_filtered), col = "blue")

## 2.2. Fitting the Linear Model for a Specific Region ##

# Subsetting the data for a specific region - Latin America & Caribbean

region_subset <- subset(Epi_filtered, region == "Latin America & Caribbean")

# Fit the linear model for this subset
latin_caribbean_model <- lm(EPI ~ SPI + ECO + BDH + MKP + MHP, region_subset)

# Display summary for the region model
summary(latin_caribbean_model)

plot(region_subset$ECO, region_subset$EPI, main = "EPI vs ECO for Latin America & Caribbean", xlab = "ECO", ylab = "EPI")

# Add the fitted line for EPI ~ SPI
abline(lm(EPI ~ ECO, region_subset), col = "blue")

# Get the summary of the model to see the R-squared value
summary(epi_lm)$r.squared
summary(latin_caribbean_model)$r.squared


# Based on the r squared values, the model for the entire dataset has an r-squared value of
# 0.8291269 and the model for the Latin America and Caribbean region has an r-squared value of
# 0.8405756.The r-squared value tells one how much of the overall variance in EPI is explained by the  
# model. The regional model is a better fit. This is because it deals with capturing the specific 
# patterns of the Latin America and Caribbean region vs averaging out the differences across multiple regions 
# like the other model for the entire dataset.


## Classification (KNN) ##

library(e1071)
library(class)

# 3.1 Choose subset of 5 variables and filter the subset by keeping 3 out of the 8 regions

subset_variables <- c('SPI', 'ECO', 'BDH', 'MKP', 'MHP')
regions_1 <- c('Southern Asia', 'Eastern Europe', 'Sub-Saharan Africa')
subset_1 <- subset(Epi2024_results, region %in% regions_1, select = c('SPI', 'ECO', 'BDH', 'MKP', 'MHP', 'region'))

# Remove rows with missing values
subset_1_filtered <- na.omit(subset_1)

## training set indexes
set.seed(42)  
train.indexes <- sample(1:nrow(subset_1_filtered), size = 0.7 * nrow(subset_1_filtered))

# Create training and test sets
region1.train <- subset_1_filtered[train.indexes, ]
region1.test <- subset_1_filtered[-train.indexes, ]

# Prepare data for kNN
trainX <- region1.train[, -ncol(region1.train)]  # Features (excluding the region column)
trainY <- region1.train$region                   # Target (the region column)

testX <- region1.test[, -ncol(region1.test)]
testY <- region1.test$region                     

# Train kNN model (with k = 5)
knn_pred <- knn(train = trainX, test = testX, cl = trainY, k = 5)

# Evaluate the model with a confusion matrix
conf_matrix <- table(Actual = knn_pred, Predicted = testY, dnn = list('Predicted','Actual'))
print(conf_matrix)
write.csv(conf_matrix, "/Users/dirtydave/GitHub/Data_Analytics_2024/KNN_contingency_matrix_1.csv")

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

# Accuracy = 0.583333

# 3.2. Repeat the previous model with the same variables for another set of 3 other regions and evaluate.

regions_2 <- c('Greater Middle East', 'Latin America & Caribbean', 'Global West')
subset_2 <- subset(Epi2024_results, region %in% regions_2, select = c('SPI', 'ECO', 'BDH', 'MKP', 'MHP', 'region'))

# Remove rows with missing values
subset_2_filtered <- na.omit(subset_2)

## training set indexes
set.seed(42)  
train.indexes_2 <- sample(1:nrow(subset_2_filtered), size = 0.7 * nrow(subset_2_filtered))

# Create training and test sets
region2.train <- subset_2_filtered[train.indexes_2, ]
region2.test <- subset_2_filtered[-train.indexes_2, ]

# Prepare data for kNN
trainX_2 <- region2.train[, -ncol(region2.train)]  # Features (excluding the region column)
trainY_2 <- region2.train$region                   # Target (the region column)

testX_2 <- region2.test[, -ncol(region2.test)]
testY_2 <- region2.test$region                     

# Train kNN model (with k = 5)
knn_pred_2 <- knn(train = trainX_2, test = testX_2, cl = trainY_2, k = 5)

# Evaluate the model with a confusion matrix
conf_matrix_2 <- table(Actual = knn_pred_2, Predicted = testY_2, dnn = list('Predicted','Actual'))
print(conf_matrix_2)
write.csv(conf_matrix_2, "/Users/dirtydave/GitHub/Data_Analytics_2024/KNN_contingency_matrix_2.csv")

# Calculate accuracy
accuracy_2 <- sum(diag(conf_matrix_2)) / sum(conf_matrix_2)
print(paste("Accuracy:", accuracy_2))

# Accuracy = 0.526315789473684

# By comparing the accuracy of the contingency matrices for both models, it can be seen that the
# model for the regions Southern Asia, Eastern Europe, and Sub-Saharan Africa is the better model. It
# has an accuracy of 0.583333 compared to the accuracy of the second model for Greater Middle East, 
# Latin America & Caribbean, and Global West which is 0.526315789473684. I think the first model is the
# better one because of the regions that were chosen. The distinction of the regios plays a huge part
# in which model will be better. The regions in the first model are in the same hemisphere which can lead to 
# them having certain similarities.

## Clustering ##

kmeans_variables <- c('SPI', 'ECO', 'BDH', 'MKP', 'MHP')




