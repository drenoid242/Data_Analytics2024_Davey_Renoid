##########################################
### Principal Component Analysis (PCA) ###
##########################################

library(ggplot2)
library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with wine dataset
wine <- read.csv("/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/wine/wine.data", header = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)
wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]
write.csv(wine, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/wine_data.csv")
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

# Perform PCA (excluding the Type column)
wine.pc <- prcomp(wine[, -1], center = TRUE, scale. = TRUE)
attributes(wine.pc)
summary(wine.pc)

# Plot for the 1st and 2nd PC
autoplot(wine.pc, data = wine, colour = "Type",
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("PCA of Wine Dataset: 1st and 2nd Principal Components") +
  theme_minimal()

# Extract and rank loadings for the 1st Principal Component (PC1)
loadings <- abs(wine.pc$rotation[, 1])

# Create a data frame with variables and their contributions to the 1st PC
contributions <- data.frame(
  Variable = rownames(wine.pc$rotation),
  Contribution_to_PC1 = loadings
)

# Sort by the contribution to the 1st PC in descending order
top_contributors <- contributions[order(-contributions$Contribution_to_PC1), ]

# Display the top variables contributing to the 1st PC
print(head(top_contributors))

# Save the ranked contributions to a CSV file
write.csv(top_contributors, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/top_contributors_PC1.csv", row.names = FALSE)

### The variables that contribute the most to the 1st PC are Flavanoids, Total phenols and 
### Od280/od315 of diluted wines

## Train a classifier model to predict wine types using the 13 attributes
set.seed(42)

train_indices <- sample(1:nrow(wine), 0.7 * nrow(wine))
train_data <- wine[train_indices, ]
test_data <- wine[-train_indices, ]

# Train a Support Vector Machine (SVM) model
svm_model <- svm(Type ~ ., data = train_data, kernel = "linear")

# Make predictions on the test set
predictions <- predict(svm_model, test_data)

# Evaluate the model using a confusion matrix
contingency_table <- table(Predicted = predictions, Actual = test_data$Type)

# Print the contingency table
print(contingency_table)

n = sum(contingency_table) # number of instances
nc = nrow(contingency_table) # number of classes
diag = diag(contingency_table) # number of correctly classified instances per class 
rowsums = apply(contingency_table, 1, sum) # number of instances per class
colsums = apply(contingency_table, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

metrics <- data.frame(recall, precision, f1)

write.csv(metrics, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/classification_metrics_all.csv")

# Save the contingency table to a CSV file
contingency_table_matrix <- as.matrix(contingency_table)
#write.csv(contingency_table_matrix, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/contingency_table_all_attributes.csv")

# Calculate accuracy
accuracy <- sum(diag(contingency_table)) / sum(contingency_table)
cat("Accuracy:", accuracy)

## Train a classifier model to predict wine type using the data projected into the first 3 PCs

wine_classified <- as.data.frame(wine.pc$x[, 1:3])
wine_classified$Type <- wine$Type

sample_2 <- sample(1:nrow(wine_classified), 0.7 * nrow(wine_classified))
train_2 <- wine_classified[sample_2, ]
test_2 <- wine_classified[-sample_2, ]

svm_model_2 <- svm(Type ~ ., data = train_2, kernel = "linear")

# Make predictions on the test set
predictions_2 <- predict(svm_model_2, test_2)

# Evaluate the model using a contingency table
conf_matrix_2 <- table(Predicted = predictions_2, Actual = test_2$Type)

# Print the contingency table and calculate accuracy
print(conf_matrix_2)

n = sum(conf_matrix_2) # number of instances
nc = nrow(conf_matrix_2) # number of classes
diag = diag(conf_matrix_2) # number of correctly classified instances per class 
rowsums = apply(conf_matrix_2, 1, sum) # number of instances per class
colsums = apply(conf_matrix_2, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

metrics <- data.frame(recall, precision, f1)

write.csv(metrics, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/classification_metrics_3_PC.csv")

accuracy_2 <- sum(diag(conf_matrix_2)) / sum(conf_matrix_2)
cat("Accuracy:", accuracy_2)

# Save the contingency table to a CSV file
contingency_table_matrix <- as.matrix(conf_matrix_2)
#write.csv(contingency_table_matrix, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/contingency_table_3_PC.csv")

## Drop the variables least contributing to the 1st PC and rerun PCA ##

# Sort contributions in ascending order to find the least contributing variables
contributions <- contributions[order(contributions$Contribution_to_PC1), ]
print(contributions)

# Drop the least contributing variables (e.g., bottom 3 contributors)
least_contributors <- contributions$Variable[1:3]
print(least_contributors)

# Remove least contributing variables from the dataset
wine_reduced <- wine[, !(colnames(wine) %in% least_contributors)]

# Rerun PCA on the reduced dataset (excluding 'Type')
wine.pc_reduced <- prcomp(wine_reduced[, -1], center = TRUE, scale. = TRUE)

# Visualize the PCA with reduced variables
autoplot(wine.pc_reduced, data = wine_reduced, colour = "Type",
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("PCA of Wine Dataset: Reduced Variables") +
  theme_minimal()

# Summary of the new PCA
summary(wine.pc_reduced)

## Train a classifier using the first 3 PCs of the reduced data ##
pca_data_reduced <- data.frame(wine.pc_reduced$x[, 1:3], Type = wine$Type)
train_indices_reduced <- sample(1:nrow(pca_data_reduced), 0.7 * nrow(pca_data_reduced))
train_data_reduced <- pca_data_reduced[train_indices, ]
test_data_reduced <- pca_data_reduced[-train_indices, ]

# Train SVM model
svm_model_pca_reduced <- svm(Type ~ ., data = train_data_reduced, kernel = "linear")
predictions_pca_reduced <- predict(svm_model_pca_reduced, test_data_reduced)

# Evaluate the model
contingency_table_pca_reduced <- table(Predicted = predictions_pca_reduced, Actual = test_data_reduced$Type)
print(contingency_table_pca_reduced)

# Save the contingency table to a CSV file
contingency_table_matrix_reduced <- as.matrix(contingency_table_pca_reduced)
#write.csv(contingency_table_matrix_reduced, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/contingency_table_reduced.csv")

n = sum(contingency_table_matrix_reduced) # number of instances
nc = nrow(contingency_table_matrix_reduced) # number of classes
diag = diag(contingency_table_matrix_reduced) # number of correctly classified instances per class 
rowsums = apply(contingency_table_matrix_reduced, 1, sum) # number of instances per class
colsums = apply(contingency_table_matrix_reduced, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

metrics <- data.frame(recall, precision, f1)

write.csv(metrics, "/Users/dirtydave/GitHub/Data_Analytics_2024/Lab 4/classification_metrics_reduced.csv")

# Calculate accuracy
accuracy_pca_reduced <- sum(diag(contingency_table_pca_reduced)) / sum(contingency_table_pca_reduced)
cat("Accuracy:", accuracy_pca_reduced)
