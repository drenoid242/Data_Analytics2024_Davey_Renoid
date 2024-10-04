## Lab 2 Part 2 ##
## Exercise 1 ##

library("e1071")

abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 
summary(abalone)

abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

# drop the sex column (categorical variable) 
abalone$sex <- NULL

##Predict age group based on length and diameter

classifier2 <- naiveBayes(abalone[,c(1,2)], abalone[,c(9)]) ## use 3 variable on their own to predict the age group
contingency_table_1 <- table(predict(classifier2, abalone[,c(1,2)]), abalone[,c(9)], dnn=list("predicted","actual"))
print(contingency_table_1)
classifier2$tables$length
write.csv(contingency_table_1, "/Users/dirtydave/Documents/Data_Analytics/Lab 2/Part 2/contingency_table_1.csv")

plot(function(x) dnorm(x, 0.4209915, 0.11137474), ylim=c(0, 5), 0, 1, col="red", main="Length distribution for the 3 different age groups")
curve(dnorm(x, 0.5707182, 0.08740980), add=TRUE, col="blue")
curve(dnorm(x, 0.5868542, 0.08100644), add=TRUE, col = "green")

##Predict age group based on diameter and height

classifier3 <- naiveBayes(abalone[,c(2,3)], abalone[,c(9)]) 
contingency_table_2 <- table(predict(classifier3, abalone[,c(2,3)]), abalone[,c(9)], dnn=list("predicted","actual"))
print(contingency_table_2)
write.csv(contingency_table_2, "/Users/dirtydave/Documents/Data_Analytics/Lab 2/Part 2/contingency_table_2.csv")
classifier3$tables$diameter

plot(function(x) dnorm(x, 0.3212758, 0.09029187), ylim=c(0, 7), 0, 1, col="red", main="Diameter distribution for the 3 different age groups")
curve(dnorm(x, 0.4458591, 0.07153798), add=TRUE, col="blue")
curve(dnorm(x, 0.4632083, 0.06699741), add=TRUE, col = "green")

##Predict age group based on height and whole weight

classifier4 <- naiveBayes(abalone[,c(3,4)], abalone[,c(9)]) ## use 3 variable on their own to predict the age group
contingency_table_3 <- table(predict(classifier4, abalone[,c(3,4)]), abalone[,c(9)], dnn=list("predicted","actual"))
print(contingency_table_3)
write.csv(contingency_table_3, "/Users/dirtydave/Documents/Data_Analytics/Lab 2/Part 2/contingency_table_3.csv")
classifier4$tables$height

plot(function(x) dnorm(x, 0.1065956, 0.04183039), ylim=c(0, 15), 0, 0.6, col="red", main="Height distribution for the 3 different age groups")
curve(dnorm(x, 0.1516906, 0.02984784), add=TRUE, col="blue")
curve(dnorm(x, 0.1648125, 0.02935998), add=TRUE, col = "green")

