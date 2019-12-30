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

hdr <- read.dta('https://www.dropbox.com/s/2xqoqrtykjq6cer/hdr.dta?dl=1')
#What is the relationship between educational spending and human development, adjusting for Gross National Income, rates of child dependency, and violence rates (i.e., identification of a country as high-violence)?

#Assigning values 1 for homicide rate>5 and 0 for homicide rate<5  
hdr$high_violence <- 0 + (hdr$homicide_rate > 5)
summary(hdr$high_violence)
ggplot(hdr, aes(x = hdr$high_violence)) + geom_histogram()

#Tabulate the number of countries that are non violent and violent
table(hdr$high_violence)
tapply(hdr$hdi, hdr$high_violence, summary)
tapply(hdr$l2educ, hdr$high_violence, summary)
t.test(hdi~high_violence,data = hdr)

#Log transform education spending of countries 
hdr$l2educ <- log(hdr$education_spending, base = 2)
summary(hdr$l2educ)
ggplot(hdr, aes(x = hdr$l2educ)) + geom_histogram()
sd(hdr$l2educ, na.rm = TRUE)
hdr$l2gni <- log(hdr$gni, base = 2)
summary(hdr$l2gni)
ggplot(hdr, aes(x = hdr$l2gni)) + geom_histogram()


#### Question 2 ####
multiplemod <- lm(hdi ~ l2educ+ l2gni+dependency_young + high_violence, hdr, na.action = "na.exclude")
#Build a model which regressess HDI on Educational spending which interacts violence rates in a country against its GNI controlling for Dependency and GNI
interactionmod <- lm(hdi ~ l2educ+ l2gni+dependency_young + high_violence+l2gni * high_violence, hdr, na.action = "na.exclude")
summary(interactionmod)

#Generates Confidence interval for the interaction model
confint(interactionmod)
#Writes the model in the mentioned file
htmlreg(list(multiplemod, interactionmod), file = "C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/S 040 - Intermediate Statistics for Educational Research/interaction.rtf",        digits = 4)

#### Question 3 ####
#Extracts the coefficients from the model and creates a dataframe that creates slope and intercepts
coefs = coef(interactionmod)
line_dat_simple1 <- data.frame(violence = c('Not Violent', 'Violent'),
                              slopes = coefs['l2gni'] + coefs['l2gni:high_violence'] * c(0,1),
                              intercepts = c(coefs['(Intercept)'] + coefs['l2educ'] * 2.23  + coefs['dependency_young'] * 46.67 +  coefs['high_violence'] * c( 0,1))) 
line_dat_simple1

#Visualizes the plot using the slope and intercepts
ggplot(hdr, aes(x = l2gni , y = hdi)) + geom_point() +geom_abline(data = line_dat_simple1, mapping = aes(color = violence, intercept = intercepts, slope = slopes)) + labs(x = 'l2 Gross National Income', y = 'Human Development Index')+xlim(range(hdr$l2gni, na.rm = TRUE)) + ylim(range(hdr$hdi,  na.rm = TRUE))
#What should be the x argument in the interaction variable

#### Question 4 ####
summary(hdr$gni)
hdr$gnisquare = I(hdr$gni^2)
summary(hdr$gnisquare)
#Builds a quadratic model where HDI is regressed with GNI and GNI squared while controlling for other predictors such as DEPENDENCY, Educational Spending and Violence
quadraticmod <- lm(hdi ~ gni + gnisquare + l2educ+ high_violence +dependency_young , hdr, na.action = "na.exclude")
summary(quadraticmod)
htmlreg(list(multiplemod, quadraticmod), file = "C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/S 040 - Intermediate Statistics for Educational Research/quadratic.rtf",        digits = 4)

screenreg(list(quadraticmod, interactionmod, multiplemod), include.fstatistic = TRUE) 

#### Question 5 ####
coefs = coef(quadraticmod)

#Using the coefficiences, creates slope and intercepts as part of the dataframe
line_dat_simple2 <- data.frame(slopes = c(coefs['gni']),
                               intercepts = c(coefs['(Intercept)'] + coefs['l2educ'] * 2.23 + coefs['dependency_young'] * 46.67 + coefs['gnisquare']*hdr$gnisquare))


#Dataframe gets visualized using GGPLOT as relationship between HDI and GNI controlling for other variables 
ggplot(hdr, aes(x = hdr$gni , y = hdr$hdi)) + geom_point(alpha = .2) + geom_smooth( data = line_dat_simple2, mapping = aes(intercept = intercepts, slope = slopes)) + xlab(hdr$gni) + ylab(hdr$hdi)  + labs(x = 'Gross National Income', y = 'Human Development Index')


#Adfer's plot
ggplot(hdr, aes(x = gni , y = hdi)) + geom_point(alpha = .2) + geom_smooth(method = 'lm', se = FALSE, formula = y ~ x + I(x*x)) + xlab(hdr$gni) + ylab(hdr$hdi)  + labs(x = 'Gross National Income', y = 'Human Development Index')

#ggplot(hdr, aes(x = gni , y = hdi))+ geom_point(alpha = .2) + data.frame(stat_function(fun = fun_plot)) + labs(x = 'Gross National Income', y = 'Human Development Index') + xlim(range(hdr$gni, na.rm = TRUE)) + ylim(range(hdr$hdi,  na.rm = TRUE))

#+geom_smooth(method = 'lm', se = FALSE, formula = y ~ x + I(x*x))
#How to change values in exponent
#ggplot(hdr, aes(x = gni , y = hdi)) + geom_point() +geom_abline(data = line_dat_simple2, mapping = aes( intercept = intercepts, slope = slopes)) + labs(x = 'l2 Gross National Income', y = 'Human Development Index')+xlim(range(hdr$gni, na.rm = TRUE)) + ylim(range(hdr$hdi,  na.rm = TRUE))
