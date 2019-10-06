rm(list = ls()) # this line says remove (that's the rm) everything currently in the working environment (that's list = ls())

install.packages('readstata13') 
library(readstata13)
library(ggplot2)
library(foreign)

#### Set the home working directory and read the wvs datasets into R ####
setwd("C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/S 040 - Intermediate Statistics for Educational Research/Homework Assignment 1")
wvsdata <- read.dta13('wvs.dta')

#### Distribution of Variables ####
wvsdata$abortion <- wvsdata$justified_abortion >=6
wvsdata$abortion <- factor(wvsdata$abortion,levels = c(FALSE, TRUE),labels = c("Rarely Justified","Frequently Justified") )
#Check for values which are greater than or equal to 6 in abortion column of wvs dataset 
#and assigned TRUE if condition is satisfied and FALSE otherwise. 
#Values are changed from TRUE to Frequently justified and FALSE to Rarely justified
table(wvsdata$abortion)
wvsdata$religion <- wvsdata$important_religion <=2
wvsdata$religion <- factor (wvsdata$religion, levels = c(TRUE, FALSE), labels = c("Religion important", "Religion not important"))
table(wvsdata$religion)
#If values of important religion are lesser than or equal to 2 then its changed to Religion important
#If values are otherwise then they are changed to Religion not important

ggplot(subset(wvsdata, !is.na(wvsdata$abortion)), aes(x = abortion)) + geom_bar()
ggplot(subset(wvsdata, !is.na(wvsdata$religion)), aes(x = religion)) + geom_bar()
#Bar chart of both newly created abortion and religion variables are represented by calling ggplot.
#Values of NA in both the variables are omitted through the !is.na command and 
#X axis is defined through aes command 

#### Summary of the observed relationship between the variables ####
table(wvsdata$religion, wvsdata$abortion )
prop.table(table(wvsdata$religion, wvsdata$abortion ), margin = 1)
round(prop.table(table(wvsdata$religion, wvsdata$abortion ), margin = 1),4) * 100
#Contingency table was created by calling table command to associate religon and abortion variables.
#Values are changed to proportion through prop.table and round command multiplied by 100 is to convert the proportion to percentages

results = chisq.test(table(wvsdata$religion,wvsdata$abortion), correct = FALSE)
results$statistic
results$p.value
results$observed
round(results$expected)
#Null hypothesis testing was done and the test results were stored in results variable
#result$statistic shows the chi squared value and results$p.value showes the p value from the results

####Summarize the relationship between the variable separately for Indian and American respondents ####
wvsdatasubset <- subset(wvsdata, wvsdata$country %in% c("India","United States"))
table(wvsdatasubset$country)
#wvsdatasubset stores the values having India and US as the country of origin

#### India ####
wvsIndia <- subset(wvsdatasubset, wvsdatasubset$country %in% c("India"))
table(wvsIndia$religion, wvsIndia$abortion )
#Contingency table analysis was done between religion and abortion variables having values with country of origin as India
round(prop.table(table(wvsIndia$religion, wvsIndia$abortion ), margin = 1),4)*100
TestStatIndia = chisq.test(table(wvsIndia$religion, wvsIndia$abortion), correct = FALSE)
TestStatIndia$statistic
TestStatIndia$p.value
TestStatIndia$observed
TestStatIndia$expected
#Null hypothesis testing was done and the test results were stored in TestStatIndia variable
#TestStatIndia$statistic shows the chi squared value and TestStatIndia$p.value showes the p value from the results

#### USA ####
wvsUS <- subset(wvsdatasubset, wvsdatasubset$country %in% c("United States"))
table(wvsUS$religion, wvsUS$abortion )
#Contingency table analysis was done between religion and abortion variables having values with country of origin as US
round(prop.table(table(wvsUS$religion, wvsUS$abortion ), margin = 1),4)*100
TestStatAmerica = chisq.test(table(wvsUS$religion, wvsUS$abortion), correct = FALSE)
TestStatAmerica$statistic
TestStatAmerica$p.value
TestStatAmerica$expected
TestStatAmerica$observed
#Null hypothesis testing was done and the test results were stored in TestStatAmerica variable
#TestStatAmerica$statistic shows the chi squared value and TestStatAmerica$p.value showes the p value from the results

#### Use the full information in the religion variable ####
#instead of using the religion variable which we split as important and not important, we are using 
#important_religion variable which has values ranging from 1 to 4 to determine the importance of religion
table(wvsdata$important_religion, wvsdata$abortion)
round(prop.table(table(wvsdata$important_religion, wvsdata$abortion), margin = 1),4)*100
TestStatistic <- chisq.test(table(wvsdata$important_religion,wvsdata$abortion), correct = FALSE)
TestStatistic$statistic
TestStatistic$p.value
TestStatistic$expected
TestStatistic$observed

#### Challenge Question 2 ####

wvsJordan <- subset(wvsdata, wvsdata$country %in% c("Jordan"))
wvsQatar <- subset(wvsdata, wvsdata$country %in% c("Qatar"))
wvsPhilippines <- subset(wvsdata, wvsdata$country %in% c("Philippines"))

table(wvsJordan$important_religion,wvsJordan$justified_abortion)
round(prop.table(table(wvsJordan$important_religion, wvsJordan$justified_abortion), margin = 1),4)*100
TestStatisticJordan <- chisq.test(table(wvsJordan$important_religion,wvsJordan$justified_abortion), correct = FALSE)

table(wvsQatar$religion,wvsQatar$abortion)
round(prop.table(table(wvsQatar$religion, wvsQatar$abortion), margin = 1),4)*100
TestStatisticQatar <- chisq.test(table(wvsQatar$important_religion,wvsQatar$justified_abortion), correct = FALSE)

table(wvsPhilippines$religion,wvsPhilippines$abortion)
round(prop.table(table(wvsPhilippines$religion, wvsPhilippines$abortion), margin = 1),4)*100
TestStatisticPhilippines <- chisq.test(table(wvsPhilippines$important_religion,wvsPhilippines$justified_abortion), correct = FALSE)
