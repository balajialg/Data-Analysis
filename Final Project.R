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

rm(list = ls())

# Input the Indian UDISE dataset into R (Indian Education Dataset)
UDISE <- read.csv('C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/S 040 - Intermediate Statistics for Educational Research/Final Project/Distt Report Card 2015-16_Version_App_3.1.0.csv')

#### STEP 1 - Visualize the Univariate Statistics
#Total number of schools across districts in India
summary(UDISE$Total_Number_of_Schools)
ggplot(UDISE, aes(x = Total_Number_of_Schools)) + geom_histogram()  + xlab('Total number of schools across districts in India ')

#Number of schools having girls toilet in a District 
summary(UDISE$SGTOILTOT)
ggplot(UDISE, aes(x = SGTOILTOT)) + geom_histogram()  + xlab('Number of schools  with Girls toilet across districts in India')

#Main Predictor - Proportion of schools with girls toilet across districts in India
UDISE$SchoolsWithToilet <- (UDISE$SGTOILTOT/UDISE$Total_Number_of_Schools) 
summary(UDISE$SchoolsWithToilet)
  ggplot(UDISE, aes(x = SchoolsWithToilet)) + geom_histogram()  + xlab('Proportion of schools with Girls toilet across districts in India')

#Control Predictor - Total number of government schools which are rural in India
summary(UDISE$SCHTOTGR)
ggplot(UDISE, aes(x = SCHTOTGR)) + geom_histogram() + xlab('Total number of govt rural schools across Districts ')

#Drop out rate of students in upper primary grades across districts in India
summary(UDISE$DRUP)
ggplot(UDISE, aes(x = DRUP)) + geom_histogram() + xlab('Drop out percentage in upper primary ')

#Drop out rate of girl students in upper primary grades across districts in India
summary(UDISE$DRGUP)
ggplot(UDISE, aes(x = DRGUP)) + geom_histogram() + xlab('Drop out percentage of girls in upper primary ')

#Visualize the polychotomous predictor - States across India
ggplot(UDISE, aes(x = STATE.NAME, y = SchoolsWithToilet)) + geom_point()  + xlab('States across India')
ggplot(UDISE, aes(x = STATE.NAME, y = SCHTOTGR)) + geom_point()  + xlab('States across India')
ggplot(UDISE, aes(x = STATE.NAME, y = DRGUP)) + geom_point()  + xlab('States across India')

#### STEP 2 - Visualize the bivariate statistics
#Correlation across predictors and outcomes
ggpairs(UDISE[c('DRGUP', 'SchoolsWithToilet', 'SCHTOTGR')])
#Correlation between Key Predictor and Outcome
cor.test(UDISE$DRGUP, UDISE$SchoolsWithToilet) 
#Correlation between control predictor and Outcome
cor.test(UDISE$DRGUP, UDISE$SCHTOTGR) 
#Correlation between Predictor and control predictor
cor.test(UDISE$SchoolsWithToilet, UDISE$SCHTOTGR) 

#Keeping the reference value of the Polychotomous predictor state as DELHI
UDISE$STATE.NAME <- relevel(UDISE$STATE.NAME, ref = "DELHI")

#### STEP 3 - Regression
#Regressing Drop out rate of girls in upper primary on proportion of Schools with girls toilet across districts in India
mod1 <- lm(DRGUP ~ SchoolsWithToilet, UDISE)
summary(mod1)
screenreg(list(mod1), include.fstatistic = TRUE) 
confint(mod1)


#Regressing Drop out rate of girls in upper primary grades on proportion of Schools with girls toilet in districts controlling for schools which are government and rural across districts in India
mod2 <- lm(DRGUP ~ SchoolsWithToilet + SCHTOTGR, UDISE)
summary(mod2)
confint(mod2)
screenreg(list(mod1, mod2), include.fstatistic = TRUE) 

#Regressing Drop out rate of girls in upper primary grades on proportion of Schools with girls toilet across districts in India controlling for schools which are government and rural across districts and also states in India
mod3 <- lm(DRGUP ~SchoolsWithToilet + SCHTOTGR +STATE.NAME, UDISE,na.action = "na.exclude")
summary(mod3)
confint(mod3)
screenreg(list(mod1, mod2, mod3), include.fstatistic = TRUE) 

#To the model 3, Add a statistical interaction between states and main predictor proportion of schools which have girls toilet across districts in India
mod4 <- lm(DRGUP ~SchoolsWithToilet+ SCHTOTGR +STATE.NAME + STATE.NAME * SchoolsWithToilet, UDISE,na.action = "na.exclude")
summary(mod4)
#Using screenreg listing the taxonomy table for all the models built 
screenreg(list(mod1, mod2, mod3,mod4), include.fstatistic = TRUE) 
htmlreg(list(mod1,mod2, mod3, mod4), file = "C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/S 040 - Intermediate Statistics for Educational Research/finmod.rtf",        digits = 4)


#### STEP 4 - ANOVA test to identify the appropriate model
#Using ANOVA test to identify the models which are statistically significant and can be part of final interpretation
anova(mod2,mod3, mod4)

#### STEP 5 - Assumption Checking
#Assumption checking for normality, homescedasticity and linearity for the selected model
UDISE$pred <- predict(mod3)
UDISE$resid <- rstandard(mod3)
summary(UDISE$resid)
ggplot(UDISE, aes(x = UDISE$pred)) + geom_histogram() 
ggplot(UDISE, aes(x = UDISE$resid)) + geom_histogram() + labs(x = 'Histogram of Residuals')

ggplot(UDISE, aes(x = UDISE$resid)) + geom_histogram(aes(y = ..density..), bins = 20) + geom_density() +  stat_function(fun = dnorm, col = 'darkorchid') +   xlab('Standardized residuals from the final model')

ggplot(UDISE, aes(x = UDISE$pred, y =UDISE$resid )) + geom_point() 
ggplot(UDISE, aes(x = UDISE$pred, y = UDISE$resid)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + geom_smooth(se = FALSE, col = 'purple') + labs(x = 'Predicted values from the model', y = 'Residuals') #Evaluate whether its predictor 

#### STEP 6 - Generate prototypical lines
#Generating Prototypical lines by substituting the needed values in the selected model
coefs = coef(mod3)
#Create dataframe with the substituted values for slope and intercept
line_dat_simple <- data.frame(states =  c('DELHI', 'GUJARAT', 'NAGALAND'), slopes = coefs['SchoolsWithToilet'],intercepts = coefs['(Intercept)'] + coefs['SCHTOTGR'] * 1468 + c(coefs['STATE.NAMEDELHI'],coefs['STATE.NAMEGUJARAT'],coefs['STATE.NAMENAGALAND'] ))

#Create the plot between Drop out rate of girls in upper primary and proportion of schools with toilet with prototypical values
ggplot(UDISE, aes(x = SchoolsWithToilet , y = DRGUP)) + geom_point() +geom_abline(data = line_dat_simple, mapping = aes( intercept = intercepts, slope = slopes, col = states)) + labs(x = 'Proportion of Schools with girls toilet', y = 'Drop out rate of girls in Upper Primary grades')+xlim(range(UDISE$SchoolsWithToilet, na.rm = TRUE)) + ylim(range(UDISE$DRGUP,  na.rm = TRUE))




########IGNORE ###############

#Explore Female Literacy Rate variable
Female_Literacy_Rate <- UDISE$Female.Literacy.Rate
summary(Female_Literacy_Rate)
ggplot(UDISE, aes(x = Female_Literacy_Rate)) + geom_histogram() + xlab('Overall Female Literacy rate in India')

#Total number of students in primary grades
Enrolment_PR <- UDISE$Age.Group.6.to.10
summary(Enrolment_PR)
ggplot(UDISE, aes(x = Enrolment_PR)) + geom_histogram() + xlab('Total number of students in primary ')

# Schools with Primary and Upper Primary
#UDISE$SGTOILPUP <- UDISE$SGTOIL1 + UDISE$SGTOIL2
ggplot(UDISE, aes(x = Primary.with.Upper.Primary_Schools_Total, y = SGTOIL2)) + geom_point()
cor.test(UDISE$Primary.with.Upper.Primary_Schools_Total, UDISE$SGTOIL2)
cor.test(UDISE$Primary_Schools_Total, UDISE$SGTOIL1)

cor.test(UDISE$DR6to8,UDISE$Primary.with.Upper.Primary_Schools_Total )

cor.test(UDISE$DR6to8, UDISE$PPTR30)
#Govt Schools 
ggplot(UDISE, aes(x = Total_Govt_Schools, y = SGTOILTOT)) + geom_point()
cor.test(UDISE$Total_Govt_Schools, UDISE$SGTOILTOT)

#Girl Toilets vs Gender Gap
mod1 <- lm( Gender_Gap ~ SGTOILTOT, UDISE )
summary(mod1)
#Drop out rate of girls in upper primary vs parents to teacher's ratio of 1:30
modPTR <- lm(DRGUP ~ UPTR35, UDISE)
summary(modPTR)


#Drop out rate of girls in all govt rural schools due to Girls Toilet
mod4 <- lm(DRGUP ~SGTOILTOT + STATE.NAME + Age.Group.11.to.13 + Total_Number_of_Schools, UDISE)
summary(mod4)

#Dropout rate of girls in upper primary on the government schools, Professional qualification of female teachers
mod5 <- lm(DRGUP ~ Total_Govt_Schools + PGRFTCH, UDISE)
summary(mod5)

#Dropout rate of girls in upper primary on the government schools, Professional qualification of male and female teachers
mod6 <- lm(DRGUP ~ PGRFTCH + SCHTOTGR + PGRMTCH  , UDISE)
summary(mod6)

mod7 <- lm(DRGUP ~SGTOILTOT + UIDAY35, UDISE)
summary(mod7)

#Dropout rate of girls in upper primary on the girls toilet, state, total schools and number of instructional days
mod8 <- lm(DRGUP ~SGTOILTOT + STATE.NAME + Total_Number_of_Schools + UIDAY35, UDISE)
summary(mod8)

#Dropout rate of girls in upper primary on the girls toilet, state, total schools and number of instructional days, rural schools
mod9 <- lm(DRGUP ~SGTOILTOT + UIDAY35+ SCHTOTGR, UDISE)
summary(mod9)

#Drop out rate of stuents in upper primary vs Girls Toilet
mod1 <- lm(DRUP ~ SGTOILTOT, UDISE )
summary(mod1)

#Total number of students in Age group 11 to 13
Enrolment_UPR <- UDISE$Age.Group.11.to.13
summary(Enrolment_UPR)
ggplot(UDISE, aes(x = Enrolment_UPR)) + geom_histogram() + xlab('Total number of students in Upper primary ')



#Drop out rate of girls in upper primary vs Girls Toilet
mod2 <- lm(DRGUP ~ SGTOILTOT, UDISE)

mod3 <- lm(DRGUP ~ SGTOILTOT + SCHTOTGR, UDISE)
summary(mod3)
screenreg(list(mod2, mod3), include.fstatistic = TRUE) 


mod4 <- lm(DRGUP ~SGTOILTOT + SCHTOTGR +STATE.NAME, UDISE,na.action = "na.exclude")
summary(mod4)
screenreg(list(mod2, mod3, mod4), include.fstatistic = TRUE) 

mod5 <- lm(DRGUP ~SGTOILTOT + SCHTOTGR +STATE.NAME + STATE.NAME * SGTOILTOT, UDISE,na.action = "na.exclude")
summary(mod5)
confint(mod5)
screenreg(list(mod2, mod3, mod4, mod5), include.fstatistic = TRUE) 
anova(mod3,mod4,mod5)
#Assumption Checking
UDISE$pred <- predict(mod4)
UDISE$resid <- rstandard(mod4)
summary(UDISE$resid)
ggplot(UDISE, aes(x = UDISE$pred)) + geom_histogram() 
ggplot(UDISE, aes(x = UDISE$resid)) + geom_histogram() + labs(x = 'Histogram of Residuals from interaction model')
ggplot(UDISE, aes(x = UDISE$pred, y =UDISE$resid )) + geom_point() 
ggplot(UDISE, aes(x = UDISE$pred, y = UDISE$resid)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + geom_smooth(se = FALSE, col = 'purple') + labs(x = 'Predicted values from the model', y = 'Residuals') #Evaluate whether its predictor vs residuals or vice versa
