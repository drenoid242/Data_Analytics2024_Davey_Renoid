## Lab 2 Part 2 ##
### Exercise 2 - KNN ###

library("e1071")
library('class')

iris.df <- iris

rows <- nrow(iris.df)
sample.iris <- sample(rows,rows*.7)

## create train & test sets based on sampled indexes
iris.train <- iris[sample.iris,]
iris.test <- iris[-sample.iris,]

sqrt(105)
k = 11

# Train model and predict for sepal.length & sepal.width
KNNpred <- knn(train = iris.train[,1:2], test = iris.test[,1:2], cl = iris.train$Species, k = k)

# Create contingency table/ confusion matrix
contingency.table <- table(Actual = KNNpred, Predicted = iris.test$Species, dnn = list('predicted','actual'))
print(contingency.table)
write.csv(contingency.table, "/Users/dirtydave/Documents/Data_Analytics/Lab 2/Part 2/KNN_contingency_table_1.csv")
contingency.matrix = as.matrix(contingency.table)
sum (diag(contingency.matrix))/length(iris.test$Species)

accuracy <- c()
ks <- c(5,11,25,55,75,95)

for (k in ks) {
  KNNpred <- knn(train = iris.train[,1:2], test = iris.test[,1:2], cl = iris.train$Species, k = k) 
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual'))) 
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species))
}
plot(ks,accuracy,type = "b", xlim = c(0, 100), ylim = c(0.5,0.9))

# The contingency table that was generated for the model of sepal.length and 
# sepal.width is 75% accurate. The model predicted species for setosa based on 
# the parameters. It incorrectly predicted 7 versicolor species as virginica and 4 virginica as versicolor.
# It can be seen from the accuracy plot that the best k value for this model
# would be 13-15. It would give an accuracy of 80%


### Second subset of features ###

# Train model and predict for sepal.length & petal.length

k = 25
KNNpred <- knn(train = iris.train[,c(1,3)], test = iris.test[,c(1,3)], cl = iris.train$Species, k = k)

# Create contingency table/ confusion matrix
contingency.table <- table(Actual = KNNpred, Predicted = iris.test$Species, dnn = list('predicted','actual'))
print(contingency.table)
write.csv(contingency.table, "/Users/dirtydave/Documents/Data_Analytics/Lab 2/Part 2/KNN_contingency_table_2.csv")
contingency.matrix = as.matrix(contingency.table)
sum (diag(contingency.matrix))/length(iris.test$Species)

accuracy <- c()
ks <- c(5,11,25,55,75,95)

for (k in ks) {
  KNNpred <- knn(train = iris.train[,c(1,3)], test = iris.test[,c(1,3)], cl = iris.train$Species, k = k) 
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual'))) 
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species))
}
plot(ks,accuracy,type = "b", xlim = c(0, 100), ylim = c(0.5,1))

# The contingency table that was generated for the model of sepal.length and 
# petal.length is 95% accurate. The model predicted species for setosa based on 
# the parameters. It incorrectly predicted 1 versicolor species as virginica and 1 virginica as versicolor.
# It can be seen from the accuracy plot that the best k value for this model
# would be 25. It would give an accuracy of 95%
