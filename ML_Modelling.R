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
model_NB = naiveBayes(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`,data = train[,])
print(model_NB)
summary(model_NB)

#Prediction of the test datum
predict_NB = predict(model_NB,test[233599,])


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

model_rf = randomForest(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`,train[1000,],ntree = 100)