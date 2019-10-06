install.packages('readstata13') 
library(readstata13)
library(ggplot2)
library(foreign)

#### Set the home working directory and read the wvs datasets into R ####
setwd("C:/Users/LenovoT480s/Desktop/HGSE TIE 2019 - 2020/Courses/S 040 - Intermediate Statistics for Educational Research/Homework Assignments/Assignment 3")
madesedata <- read.dta13('https://www.dropbox.com/s/6jes28kc3yuo455/madese.dta?dl=1')

summary(madesedata)

mean(madesedata$teach_white_pct, na.rm = TRUE) #Measures the central tendency
median(madesedata$teach_white_pct, na.rm = TRUE) #Measures the central tendency
sd(madesedata$teach_white_pct, na.rm = TRUE) #Measures the spread
summary(madesedata$teach_white_pct) #Measures the spread 
ggplot(madesedata, aes(x =teach_white_pct )) + geom_histogram() + xlab('Percentage of white teachers in schools')

mean(madesedata$stud_white_pct, na.rm = TRUE)
median(madesedata$stud_white_pct, na.rm = TRUE)
sd(madesedata$stud_white_pct, na.rm = TRUE)
summary(madesedata$stud_white_pct)
ggplot(madesedata, aes(x =stud_white_pct )) + geom_histogram() + xlab('Percentage of white students in schools')


ggplot(madesedata, aes(x=teach_white_pct, y=stud_white_pct)) + geom_point() + xlab('Percentage of white teachers in schools') + ylab('Percentage of white students in schools')



