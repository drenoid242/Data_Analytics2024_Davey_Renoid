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


### K-Means with Abalone dataset ###

library("e1071")
library("ggplot2")

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
