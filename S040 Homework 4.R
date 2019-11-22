#---
#title: "R Notebook"
#output: html_notebook
#---
install.packages('readstata13') 
library(readstata13)
library(ggplot2)
library(foreign)
library(texreg)
library(gridExtra)
library(GGally)

#What is the relationship between educational spending and the Human Development Index, adjusting for a country’s per-capita Gross National Income (GNI) and child dependency ratio (the number of people under the age of 14 as a proportion of the people aged 15 to 64)?

#Reading the dataset from dropbox into R
hdr <- read.dta('https://www.dropbox.com/s/2xqoqrtykjq6cer/hdr.dta?dl=1')

### Question 2 - Describe Predictors and Outcomes ####
#Get the mean, median and interquartile range of GNI variable
summary(hdr$gni)
#Get the standard deviation of GNI variable
sd(hdr$gni, na.rm = TRUE)
#Visualize the GNI variable as histogram
ggplot(hdr, aes(x = hdr$gni)) + geom_histogram()


#Log transform the Educational Spending and Gross National Income predictors with the base 2 
hdr$l2educ <- log(hdr$education_spending, base = 2)
summary(hdr$l2educ)
sd(hdr$l2educ, na.rm = TRUE)
ggplot(hdr, aes(x = hdr$l2educ)) + geom_histogram()

hdr$l2gni <- log(hdr$gni, base = 2)
summary(hdr$l2gni)
sd(hdr$l2gni, na.rm = TRUE)
ggplot(hdr, aes(x = log(gni, base = 2))) + geom_histogram()

summary(hdr$dependency_young)
sd(hdr$dependency_young, na.rm = TRUE)
ggplot(hdr, aes(x = hdr$dependency_young)) + geom_histogram()

summary(hdr$hdi)
sd(hdr$hdi, na.rm = TRUE)
ggplot(hdr, aes(x = hdr$hdi)) + geom_histogram()

#Scatterplot of all the values between Human Development Index and Education Spending
ggplot(hdr, aes(y = hdr$hdi, x = hdr$education_spending)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + geom_smooth(se = FALSE, col = 'purple') 

#Question 3 - Identify Correlation among Predictors and Outcomes by generating correlation matrix ####
ggpairs(hdr[c('hdi', 'l2educ', 'l2gni', 'dependency_young')])

#Question 4 - Linear Regression & Assumption Violation ####
#Regress Human Development Index on Educational Spending in Countries and identify the indicators for this regression
ggplot(hdr, aes(x = hdi)) + geom_histogram()
simplemod <- lm(hdi ~ l2educ, hdr, na.action = "na.exclude")
summary(simplemod)
#Identify the confidence intervals, Build the prediction models and identify the standard residuals 
confint(simplemod)
hdr$pred <- predict(simplemod)
hdr$resid <- rstandard(simplemod)
summary(hdr$resid)
#Visualize the predicted values from the model in a histogram
ggplot(hdr, aes(x = hdr$pred)) + geom_histogram()
#Visualize the residuals from the model in a histogram
ggplot(hdr, aes(x = hdr$resid)) + geom_histogram()
#Visualize the scatter plot between residuals and predicted values 
ggplot(hdr, aes(x = hdr$resid, y = hdr$pred)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + geom_smooth(se = FALSE, col = 'purple') 

#### Question 5 - Multiple regression 
#Generate a model where Human Development Index is regressed on Educational spending controlling for Gross National Income and the Dependency Young
multiplemod <- lm(hdi ~ l2educ+ l2gni+dependency_young, hdr, na.action = "na.exclude")
summary(multiplemod)
#Create a Taxonomy table comparing both linear and multiple regression
htmlreg(list(simplemod, multiplemod), file = "C:/Users/LenovoT480s/Desktop/regoutput.rtf",        digits = 4)

#### Question 6 - Evaluate assumptions such as linearity, normality, homoscedasctity  by creating scatterplots for Standardized residuals and Predictors
hdr$multiplepred <- predict(multiplemod)
hdr$multipleresid <- residuals(multiplemod)
ggplot(hdr, aes(x = hdr$multiplepred)) + geom_histogram()
ggplot(hdr, aes(x = hdr$multipleresid)) + geom_histogram()
ggplot(hdr, aes(x = hdr$multiplepred, y = hdr$multipleresid)) + geom_point() + geom_smooth(se = FALSE, col = 'green') + geom_smooth(method = lm, se = FALSE)

#Scatterplots for standardized residuals and log transform of Educational outocmes
ggplot(hdr, aes(x = hdr$l2educ, y = hdr$multipleresid)) + geom_point() + geom_smooth(se = FALSE, col = 'green') + geom_smooth(method = lm, se = FALSE)

#Scatterplots for standardized residuals and log transform of Gross National Income
ggplot(hdr, aes(x = hdr$l2gni, y = hdr$multipleresid)) + geom_point() + geom_smooth(se = FALSE, col = 'green') + geom_smooth(method = lm, se = FALSE)

#Scatterplots for standardized residuals and Child as a percentage of adults
ggplot(hdr, aes(x = hdr$dependency_young, y = hdr$multipleresid)) + geom_point() + geom_smooth(se = FALSE, col = 'green') + geom_smooth(method = lm, se = FALSE)

#Scatterplots 
#### Question 9 - Creating prototypicafor predicted values and standardized residuals
ggplot(hdr, aes(x = hdr$multiplepred, y = hdr$multipleresid)) + geom_point() + geom_smooth(se = FALSE, col = 'green') + geom_smooth(method = lm, se = FALSE) +
geom_smooth(se = FALSE, col = 'yellow', lty = 2) 
coefs =  coef(multiplemod)
line_dat <- data.frame(slopes = c(coefs['l2educ']), 
                       intercepts = c(coefs['(Intercept)'] + coefs['dependency_young'] * 46.67 + coefs['l2gni'] * 9,
                                      coefs['(Intercept)'] + coefs['dependency_young'] * 46.67 + coefs['l2gni'] * 12.5,
                                      coefs['(Intercept)'] + coefs['dependency_young'] * 46.67 + coefs['l2gni'] * 16),
                       type = c('Low GNI', 'Moderate GNI', 'High GNI'))
ggplot(hdr, aes(x = l2educ, y = hdi)) + geom_point() + 
  geom_abline(data = line_dat, mapping = aes(slope = slopes, intercept = intercepts, color = type))


