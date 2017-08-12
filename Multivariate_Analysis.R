#Chi-Square Test for Independence
View(train)
#Gender V/s Age
myTable <- xtabs(~Gender+Age,data = train)
library(vcd)
chisq.test(myTable)

#Age V/s City Category
myTable <- xtabs(~Age+City_Category,data = train)
chisq.test(myTable)

#City_Category V/s Marital Status
myTable <- xtabs(~City_Category+Marital_Status,data = train)
chisq.test(myTable)

#Association Statitics between varaibles
myTable <- xtabs(~Gender+Age,data = train)
library(vcd)
assocstats(myTable)

#Age V/s City Category
myTable <- xtabs(~Age+City_Category,data = train)
assocstats(myTable)

#City_Category V/s Marital Status
myTable <- xtabs(~City_Category+Marital_Status,data = train)
assocstats(myTable)

#Correlation Check
install.packages('psych')
library(psych)
corr.test(train,use = "complete")