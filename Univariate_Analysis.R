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