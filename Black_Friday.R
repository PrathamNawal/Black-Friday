#Load the datum
library(readr)
train <- read_csv("~/Downloads/train.csv" 
                  +     col_types = cols(Age = col_factor(levels = c()) 
                                         +         City_Category = col_factor(levels = c()) 
                                         +         Gender = col_factor(levels = c()) 
                                         +         Marital_Status = col_factor(levels = c()) 
                                         +         Stay_In_Current_City_Years = col_factor(levels = c())))

#structure of the train datum
str(train)

#head of the datum
head(train)

#tail of the datum
tail(train)

#Visualization
#load the libraries for visualization
library("ggplot2")
library("scales")
library("corrplot")
library("psych")
library("gplot")
library("vcd")
#Univariate Analysis
#Gender
table(train$Gender)
round(prop.table(table(train$Gender)),digits = 2)
class(train$Gender)
levels(train$Gender)[levels(train$Gender) == "M"] <- 0
levels(train$Gender)[levels(train$Gender) == "F"] <- 1

#Age
class(train$Age)
table(train$Age)
round(prop.table(table(train$Age)),digits = 2)

#recoding age groups
levels(train$Age)[levels(train$Age) == "0-17"] <- 0
levels(train$Age)[levels(train$Age) == "18-25"] <- 1
levels(train$Age)[levels(train$Age) == "26-35"] <- 2
levels(train$Age)[levels(train$Age) == "36-45"] <- 3
levels(train$Age)[levels(train$Age) == "46-50"] <- 4
levels(train$Age)[levels(train$Age) == "51-55"] <- 5
levels(train$Age)[levels(train$Age) == "55+"] <- 6

#Occupation
table(train$Occupation)
class(train$Occupation)
round(prop.table(table(train$Occupation)),digits = 2)
hist(train$Occupation,main = "Occupation",xlab = "No of years")

#City Category
table(train$City_Category)
round(prop.table(table(train$City_Category)),digits = 2)
levels(train$City_Category)[levels(train$City_Category) == "A"] <- 0
levels(train$City_Category)[levels(train$City_Category) == "B"] <- 1
levels(train$City_Category)[levels(train$City_Category) == "C"] <- 2 

#Stay in the city
table(train$Stay_In_Current_City_Years)
round(prop.table(table(train$Stay_In_Current_City_Years)),digits = 2)
class(train$Stay_In_Current_City_Years)
levels(train$Stay_In_Current_City_Years)[levels(train$Stay_In_Current_City_Years) == "4+"] <- 4

#Marital Status
table(train$Marital_Status)
round(prop.table(table(train$Marital_Status)),digits = 2)


#Total Missing Values
sum(is.na(train))

#Category wise missing values
apply(train,2,function(x)sum(is.na(x)))

#Removing Product_Category_3 because it has missing values more than 40% of the observations
train = train[,-11]

#Imputation of missing values
install.packages('DMwR')
library(DMwR)
train = centralImputation(train)


#Alternative Method
centralValue(train$Product_Category_3)
centralValue(train$Product_Category_2)
train$Product_Category_2[is.na(train$Product_Category_2)] = 9
train$Product_Category_3[is.na(train$Product_Category_3)] = 14

#Sampling the data
#Random sampling
set.seed(101)
sample <- sample.int(n = nrow(train), size = floor(.80*nrow(train)), replace = F)
trainSet <- train[sample, ]
testSet <- train[-sample,]

#Modelling
#Regression

#Linear Regression
linearModel = lm(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`+`Product_Category_3`,data = train)
print(linearModel)
summary(linearModel)


#Since p-value is less than 0.05, so our model is statistically significant
#We can use this model for further prediction of the test data

#Prediction of the test data
predictlm = predict(linearModel,test)

#Prediction Accuracy
preds_lm = data.frame(cbind(actuals = test$Purchase,predicteds = predictlm))
correlation_accuracy_lm = cor(preds_lm)
n = sum(correlation_accuracy_lm)
diag = diag(correlation_accuracy_lm)
accuracy_lm = sum(diag)/n
#Root Mean square error method
error_lm = predictlm - testSet$Purchase
RMSE_lm <- sqrt(mean((error_lm)^2))
#Modelling -2
#Decision Tree
library(rpart)
model_DT = rpart(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`+`Product_Category_3`,data = train)
print(model_DT)
summary(model_DT)

#Prediction of the test data
predictDT = predict(model_DT,test)

#Error Metric and Accuracy of the Model
library(caret)
table(predictDT,testSet$Purchase)

preds_DT = data.frame(cbind(actuals = testSet$Purchase,predicteds = predictDT))
correlation_accuracy_DT = cor(preds_DT)
n = sum(correlation_accuracy_DT)
diag = diag(correlation_accuracy_DT)
accuracy_DT = sum(diag)/n

#Root mean square error method
RMSE_DT <- sqrt(mean((testSet$Purchase-predictDT)^2))

#Not working currently
#Modelling - 3
#Naive Bayes
library(e1071)
model_NB = naiveBayes(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`,data = trainSet)
print(model_NB)
summary(model_NB)

#Prediction of the test datum
predict_NB = predict(model_NB,testSet)


#Root mean square error method
RMSE_NB <- sqrt(mean((testSet$Purchase-predict_NB)^2))

#Modelling - 4
#kNN Algorithm
install.packages('kknn')
library(kknn)

model_knn = kknn(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`,trainSet,testSet,k=10,distance = 2)
Conf_matrix = table(pred_knn, trainSet$Purchase)
model_kknn <- kknn(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`, trainSet, test = testSet,k=10, kernel="rectangular", distance=2)
pred_knn$fitted.values

predictedKNN = predict(pred_knn,testSet)

accuracy = sum(diag(Conf_matrix))/2000
RMSE_knn <- sqrt(mean((testSet$Purchase - pred_knn$fitted.values)^2))

#Modelling -5
library(randomForest)

model_rf = randomForest(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`,trainSet,ntree = 100)

#Test Data
library(readr)
test <- read_csv("~/Downloads/test.csv", 
                   +     col_types = cols(Age = col_factor(levels = c()), 
                                          +         City_Category = col_factor(levels = c()), 
                                          +         Gender = col_factor(levels = c()), 
                                          +         Marital_Status = col_factor(levels = c()), 
                                          +         Stay_In_Current_City_Years = col_factor(levels = c())))


#structure of the train datum
str(test)

#head of the datum
head(test)

#tail of the datum
tail(test)


#Univariate Analysis
#Gender
table(test$Gender)
round(prop.table(table(test$Gender)),digits = 2)
class(test$Gender)
levels(test$Gender)[levels(test$Gender)== "M"] <- 0
levels(test$Gender)[levels(test$Gender) == "F"] <- 1

#Age
class(test$Age)
table(test$Age)
round(prop.table(table(test$Age)),digits = 2)

#recoding age groups
levels(test$Age)[levels(test$Age) == "0-17"] <- 0
levels(test$Age)[levels(test$Age) == "18-25"] <- 1
levels(test$Age)[levels(test$Age) == "26-35"] <- 2
levels(test$Age)[levels(test$Age) == "36-45"] <- 3
levels(test$Age)[levels(test$Age) == "46-50"] <- 4
levels(test$Age)[levels(test$Age) == "51-55"] <- 5
levels(test$Age)[levels(test$Age) == "55+"] <- 6

#Occupation
table(train$Occupation)
class(train$Occupation)
round(prop.table(table(train$Occupation)),digits = 2)
hist(train$Occupation,main = "Occupation",xlab = "No of years")

#City Category
table(test$City_Category)
round(prop.table(table(test$City_Category)),digits = 2)
levels(test$City_Category)[levels(test$City_Category) == "A"] <- 0
levels(test$City_Category)[levels(test$City_Category) == "B"] <- 1
levels(test$City_Category)[levels(test$City_Category) == "C"] <- 2

#Stay in the city
table(test$Stay_In_Current_City_Years)
round(prop.table(table(test$Stay_In_Current_City_Years)),digits = 2)
class(test$Stay_In_Current_City_Years)
levels(test$Stay_In_Current_City_Years)[levels(test$Stay_In_Current_City_Years) == "4+"] <- 4

#Marital Status
table(train$Marital_Status)
round(prop.table(table(train$Marital_Status)),digits = 2)
levels(test$Gender)[levels(test$Gender) == "Male"] <- 0
levels(test$Gender)[levels(test$Gender) == "Female"] <- 1
#Total Missing Values
sum(is.na(test))

#Category wise missing values
apply(test,2,function(x)sum(is.na(x)))

#Removing Product_Category_3 because it has missing values more than 40% of the observations
test = test[,-11]

#Imputation of missing values
#Using Central Value Imputation
library(DMwR)
centralValue(test$Product_Category_2)
test$Product_Category_2[is.na(test$Product_Category_2)] = 9

#Alternative Method
library(DMwR)
centralValue(test$Product_Category_2)
centralValue(test$Product_Category_3)
test$Product_Category_2[is.na(test$Product_Category_2)] = 9
test$Product_Category_3[is.na(test$Product_Category_3)] = 14

#Modelling 1 - Linear Model 
linearModel = lm(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`+`Product_Category_3`,data = train)
print(linearModel)
summary(linearModel)

#Prediction on the test data
predictlm = predict(linearModel,test)

#Modelling -2
#Decision Tree
library(rpart)
model_DT = rpart(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`+`Product_Category_3`,data = train)
print(model_DT)
summary(model_DT)

#Prediction of the test data
predictDT = predict(model_DT,test)

#saving the model as submission
sample_DT1 = data.frame(test$User_ID,test$Product_ID,list(predictDT))
names(sample_DT1) = c("User_ID","Product_ID","Purchase")
write.csv(sample_DT1,file = "sample_DT1",row.names = FALSE)

#Modelling - 3
#kNN Algorithm
install.packages('kknn')
library(kknn)

model_knn = kknn(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`,train,test,k=10,distance = 2)
Conf_matrix = table(pred_knn, trainSet$Purchase)
model_kknn <- kknn(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`, trainSet, test = testSet,k=10, kernel="rectangular", distance=2)
pred_knn$fitted.values

predictedKNN = predict(pred_knn,test)
#not working properly

#Modelling- 4
#Gradient Boosting Method
install.packages('caret')
install_github('trainControl')
library(caret)
fitControl <- trainControl( method = "repeatedcv", number = 4, repeats = 4)

#Ensembling
#Method-1
#Average
ensemble_1 =   mapply("+",list(predictlm),list(predictDT))
sample_avg = data.frame(test$User_ID,test$Product_ID,ensemble_1)                                                                     
names(sample_avg) = c("User_ID","Product_ID","Purchase")
View(sample_avg)
write.csv(sample_avg,file = "sample_avg.csv",row.names = FALSE)
#Poor Results


