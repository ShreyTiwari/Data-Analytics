###Question 2###

#Clearing data
rm(list = ls())

#Importing the datasets.
tips_tailored <- read.csv("~/Sem5/DA/R Programming/Assignment 4/Question2/tips_tailored.csv")
tips <- read.csv("~/Sem 5/DA/R Programming/Assignment 4/Question2/tips.csv")

#Reassigning the datasets to variables.
test <- tips_tailored
actual <- tips

#Importing Library
library(caret)

##Part A##
#Confusion Matrix [GroundTruth and Predicted.Results]
matrix <- confusionMatrix(factor(test$Predicted.Results, levels = c("Win", "Lose")), factor(test$Result, levels = c("Win", "Lose")), positive = "Win", dnn = c("Predicted", "GroundTruth"))
print(matrix)

#Calucations:
# Accuracy = (TP + TN) / (TP + TN + FP + FN) = (3274 + 28670) / (3279 + 28670 + 4404 + 1895)
# Accuracy = 0.8353

# Precision = TP / (TP + FP) = 3279 / (3279 + 1895)
# Precision = 0.6337

# Recall = TP / (TP + FN) = 3279 / (3279 + 4404)
# Recall = 0.4267

# Misclassification rate = 1 - Accuracy = 1 - 0.8353 = 0.1647

# F1 score = 2 * (Precision * Recall) / (Precision + Recall) = 2 * (0.6337 * 0.4267) / (0.6337 + 0.4267)
# F1 score = 0.5099

# Fbeta score (beta = 2) = 0.4566
# Fbeta score (beta = 0.5) = 0.5777

##Part B##
#Adding column 'Predictions1' with all values equal to false
test$Predicted1 <- c("Lose")
levels(test$Predicted1) <- c("Lose","Win")

#Confusion Matrix [ GroundTruth and Predicted1 ]
matrix1 <- confusionMatrix(factor(test$Predicted1, levels = c("Win", "Lose")), factor(test$Result, levels = c("Win", "Lose")), positive = "Win", dnn = c("Predicted", "GroundTruth"))
print(matrix1)

#Calculations:(Accuracy, Precision, Recall, Misclassification Rate)
# Accuracy = (TP + TN) / (TP + TN + FP + FN) = (0 + 30565) / (0 + 30565 + 0 + 7863)
# Accuracy = 0.7991

# Recall = TP / (TP + FN) = 0 / (0 + 7863)
# Recall = 0

# Precision = TP / (TP + FP) = 0 / (0 + 0)
# Precision = Not defined

# Misclassification rate = 1 - Accuracy = 1 - 0.7991
# Misclassification rate = 0.2008

# This indicates that even though the accuracy of the second model is decent, it is an unacceptable model as the values of recall and precision are very bad. 


##Part C##

# Why is accuracy not enough for the evaluation of a classification model?
# When we use accuracy, we assign equal cost to false positives and false negatives.
# When the data set is imbalanced - say it has 99% of instances in one class and only 1 %
# in the other - there is a great way to lower the cost. Predicting that every instance belongs 
# to the majority class will get you an accuracy of 99%.
# The problem starts when the actual costs that we assign to every error are not equal.
# If we deal with a rare but fatal disease, the cost of failing to diagnose the disease of a sick
# person is much higher than the cost of sending a healthy person to more tests.

#Accuracy:
# It is number of bets predicted correctly to the total number of bets.
# It the first metric that can be used to give an idea about the model in general.
# If the accuracy of the model is low to begin with, then there is no need of futher investigation.

#Precision:
# In context of the dataset and the model, we can describe precision as:
# Out of the bets that were predicted as going to be won, how many were actually won.

#Recall:
# In context of the dataset and the model, we can describe recall as:
# Out of the bets that were actually won, how many were classified as won.


#Misclassification Rate:
# In context of the dataset and the model, we can describe misclassication rate as:
# The as the number of bets that were predicted/classified wrongly to the total number of bets.

#F-Beta Score:
# The F-beta score is the weighted harmonic mean of precision and recall, reaching its optimal value at 1 and
# its worst value at 0. Beta represents the importance of precision. It means that as the beta value increases,
# you value precision more.

# Thus we see that the metrics that we choose to evaluate our machine learning model are very important.
# In general there is no single right metric that can alone best describe any model. To understand 
# different aspects of the model we use different metrics.