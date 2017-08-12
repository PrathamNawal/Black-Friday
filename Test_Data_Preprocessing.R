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