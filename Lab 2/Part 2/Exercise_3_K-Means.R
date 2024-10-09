## Lab 2 Part 2 ##
### Exercise 3 ###

library("e1071")
library("ggplot2")

#Plot iris petal length vs. petal width, color by species
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) + geom_point()

# set seed for random number generator 
set.seed(123)

# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) + geom_point()

wss <- c()
ks <- c(5,25,55,75,95)

for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k) 
  wss <- c(wss,iris.km$tot.withinss)
}

plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa" 
labeled.clusters[labeled.clusters==2] <- "versivolor" 
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5], dnn=list('predicted','actual'))

# The cluster plot for petal.length and petal.width worked great for the setosa
# species. It was able to group setosa together. There were a couple of outliers for 
# the clusters associated versicolor and virginica. They aren't grouped as well
# setosa and have some overlap. It can be seen from the elbow plot that the ideal
# value for k is in the range of 70-75 for the optimal amount of clusters.

### K Means for Iris using Petal Length and Sepal Length ###

#Plot iris petal length vs. Sepal Length, color by species
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) + geom_point()

# set seed for random number generator 
set.seed(123)

# run k-means
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, colour = assigned.clusters)) + geom_point()

wss <- c()
ks <- c(5,10,20,40,50,60,70,80,90)

for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k) 
  wss <- c(wss,iris.km$tot.withinss)
}

plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa" 
labeled.clusters[labeled.clusters==2] <- "versivolor" 
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5], dnn=list('predicted','actual'))

# The cluster plot for sepal.length and petal.length worked great for the setosa
# species as well. It was able to group setosa together. Versicolor and virginica 
# were clustered together great as well. They aren't as distinctively grouped as
# setosa is but the clustering came out fine. It can be seen from the elbow plot that the ideal
# value for k is 60.

### K-Means with Abalone dataset ###

abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
abalone$sex <- NULL
#tf <- is.na(abalone)
#write.csv(tf, "/Users/dirtydave/Documents/Data_Analytics/Lab 2/Part 2/tf.csv")

# Plot dataset with colored age groups
ggplot(abalone, aes(x = diameter, y = whole_weight, colour = age.group)) + geom_point()

set.seed(123)

#Train and run k means
abalone.km <- kmeans(abalone[,-8:-9], centers = 3)
assigned.clusters <- as.factor(abalone.km$cluster)

ggplot(abalone, aes(x = diameter, y = whole_weight, colour = assigned.clusters)) + geom_point()

wss <- c()
ks <- c(5,15,25,30)

for (k in ks) {
  
  abalone.km <- kmeans(abalone[,-8:-9], centers = k)
  
  wss <- c(wss,abalone.km$tot.withinss)
  
}

plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "Old" 
labeled.clusters[labeled.clusters==2] <- "Adult" 
labeled.clusters[labeled.clusters==3] <- "Young"
table(labeled.clusters, abalone[,9], dnn=list('predicted','actual'))

# The cluster plot for diameter and whole weight, worked great for clustering the
# age group predictions. There is some overlap between all of the clusters for
# the age groups. It can be seen from the elbow plot that the ideal value for k 
# is 25 for the optimal amount of clusters.


