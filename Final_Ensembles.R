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

#Modelling -5 
#Elastic Net Regression
install.packages('Ecdat')
library(Ecdat)
install.packages('caret')
library(caret)
install.packages('glmnet')
library(glmnet)

x <- model.matrix(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`+`Product_Category_3`,train)
y <- train$Purchase
m <- cv.glmnet(x,y)

#Modelling -6
#GBM
install.packages('gbm')
library(gbm)
set.seed(1)
boost.Purchase = gbm(`Purchase` ~ `Gender` + `Age` + `Occupation` + `City_Category` + `Stay_In_Current_City_Years`+`Marital_Status`+`Product_Category_1`+`Product_Category_2`,train,distribution = "gaussian",n.trees = 500,interaction.depth = 4)
summary(boost.Purchase)

yhat.boost = predict(boost.Purchase,test,n.trees =100)

#Creating sample from predictions
sample_gbm = data.frame(test$User_ID,test$Product_ID,list(yhat.boost))
names(sample_gbm) <- c("User_ID","Product_ID","Purchase")
View(sample_gbm)

#Saving as csv file
write.csv(sample_gbm,file = "sample_gbm.csv",row.names = FALSE)

