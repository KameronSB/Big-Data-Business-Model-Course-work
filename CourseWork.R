#Importig the data from CSV file


#library(readxl)
#creditcard <- read_excel("creditcard2.csv")

#To import the data I right clicked on the creditcard2.csv in RStudio file
#window and clicked import dataset


summary(creditcard2)

creditcard2$Time
summary(creditcard2$Time)
#Why is this not showing on GITHUB!!!!!!!!!!

#Making it easier to access the data set 
credit <- creditcard2
credit
summary(credit)


##checking the balance (split) of the target variable
classData <- credit$Class
posClass <- subset(classData, classData == 1)
negClass <- subset(classData, classData == 0)

#working out the length (size of the dataset)
length(posClass)
length(negClass)
len <- c(length(posClass), length(negClass))
len

#Plotting bar chart fro target variable
barplot(len, col = "#1b98e0", names.arg = c("1: confirmed credit card fraud", "0: not credit card fraud"))


##Checking all other variables for missing data
sum(is.na(credit)) 

#Plotting time data analysis between fraud and non fraud cases
FraudClass <- subset(credit, credit$Class == 1)
NonFraudClass <- subset(credit, credit$Class == 0)

plot(density(FraudClass$Time))
plot(density(NonFraudClass$Time))

library(ggplot2)

#transforming seconds to hours 
credit$Time = credit$Time/3600
summary(credit$Time)
FraudClass <- subset(credit, credit$Class == 1)
NonFraudClass <- subset(credit, credit$Class == 0)

#replotting density time graphs
plot(density(FraudClass$Time))
plot(density(NonFraudClass$Time))

#Plotting with histrogram and density
ggplot(FraudClass, aes(x=Time)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666")

ggplot(NonFraudClass, aes(x=Time)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666")

##Plotting and analysis of ammount of money transferred
ggplot(FraudClass, aes(x=Amount)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=500, fill="#FF6666")

ggplot(NonFraudClass, aes(x=Amount)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=500, fill="#FF6666")

summary(FraudClass$Amount)
summary(NonFraudClass$Amount)

#Further analysis with amount and time
library(ggplot2)
ggplot(FraudClass, aes(x = Time, y = Amount)) +
  geom_point()
ggplot(NonFraudClass, aes(x = Time, y = Amount)) +
  geom_point()

##Matrix of correlation
install.packages("corrplot")                      # Install corrplot package
library("corrplot")                               # Load corrplot
corrplot(cor(credit))    # Apply corrplot function

install.packages("ggcorrplot")                      # Install ggcorrplot package
library("ggcorrplot")                               # Load ggcorrplot
ggcorrplot(cor(credit))                     # Apply ggcorrplot function



##Splitting data set into testing and training data set for use in developing a model
set.seed(101) # Set Seed so that same sample can be reproduced in future also
ind <- sample(2, nrow(credit), replace=TRUE, prob=c(0.8,0.2))
trainingSet <- credit[ind==1,]
testSet <- credit[ind==2,]

#Scaling the partioned data
trainingSet[, 1:30] <- scale(trainingSet[ ,1:30])
testSet[, 1:30] <- scale(testSet[, 1:30])

##Logistic regression
library(caTools)

#Fitting logistic regression model to training data
classifer = glm(formula = Class~.,
                family = binomial,
                data = trainingSet)

#Predicting the test set results
pred <- predict(classifer, type = 'response', newdata = testSet[-31])
pred
yPred <- ifelse(pred > 0.5, 1, 0)
yPred

##Analysing the performance of the model
#Making confusion matrix
cm = table(testSet$Class, yPred)
cm

cm <- confusionMatrix(data = testSet$Class, reference = yPred)
example <- confusionMatrix(data=predicted_value, reference = expected_value)
expected_value <- factor(yPred)
predicted_value <- factor(testSet$Class)
#Display results 
example


#Visualizing the results of test set
ctable <- as.table(matrix(c(55617, 6, 43, 62), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


##Naive Bayes
install.packages('e1071')
library('e1071')

#Fitting NB model to the training data
classifer <- naiveBayes(x = trainingSet[-31],
                        y = trainingSet$Class)

#Predicting testing set result
yPred <- predict(classifer, newdata = testSet[-31])
yPred

##Analysing the performance of the model
#Making confusion matrix
#Insatll required packages
install.packages('caret')

#Import required library
library(caret)
#Creating confusion matrix
cm = table(testSet$Class, yPred)
cm

example <- confusionMatrix(data=predicted_value, reference = expected_value)
expected_value <- factor(yPred)
predicted_value <- factor(testSet$Class)
#Display results 
example


#Visualizing the results of test set
ctale <- as.table(matrix(c(55617, 1306, 18, 86), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

##K-NN

library(class)

#fitting training data to the model and prediction
yPred <- knn(train = trainingSet[, -31],
             test = testSet[, -31],
             cl= trainingSet[, 31],
             k = 5)


#Confusion matrix
example <- confusionMatrix(data=predicted_value, reference = expected_value)
expected_value <- factor(yPred)
predicted_value <- factor(testSet$Class)
example


##Support vector machine


library('e1071')

#fitting model to training set
classifer <- svm(formula= Class~.,
                 data=trainingSet,
                 type='C-classification',
                 kernal='linear')

#predicting test set results
yPred <- predict(classifer, newdata = testSet[-31])
yPred


#Confusion matrix
expected_value <- factor(yPred)
predicted_value <- factor(testSet$Class)
example <- confusionMatrix(data=predicted_value, reference = expected_value)
example


##Random forest
install.packages('randomForest')
library(randomForest)

#Fitting model to training set
classifer <- randomForest(x = trainingSet[-31],
                          y = trainingSet$Class,
                          ntree=10)

#predicting test set results
yPred <- predict(classifer, newdata = testSet[-31])
yPred

#Confusion matrix
expected_value <- factor(yPred)
predicted_value <- factor(testSet$Class)
example <- confusionMatrix(data=predicted_value, reference = expected_value)
example
