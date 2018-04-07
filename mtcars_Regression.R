#                         ________________________

##                          REGRESSION MODELS IN R
##                         ________________________

## Introduction
## ============

## Workshop description
## ~~~~~~~~~~~~~~~~~~~~

##   - This workshop is a part of Regression on mtcars dataset
##   - Appropriate for those with basic knowledge of R
##   - This is not a statistics course!
##   - Learning objectives:
##     - Learn the R formula interface
##     - Specify factor contrasts to test specific hypotheses
##     - Perform model comparisons
##     - Run and interpret variety of regression models in R

## Materials and Setup                                        

## Use mtcars dataset
## Set working directory
## ~~~~~~~~~~~~~~~~~~~~~

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

##Cleaning the working envronment
rm(list = ls())
## set the working directory
setwd("D:/Santosh/Data Science/IMS PROschool/Batch_5/Lectures/Slides/Eight Session")
## Load the dataset
mtcars <- read.csv("mtcars.csv")

#Install the required package
#install.packages("corrplot")
#install.packages("lmtest")
#install.packages('MASS')
#install.packages("ggplot2")
#install.packages('GGally')
#install.packages("car")
install.packages('EBImage')
library(corrplot)
library(lmtest)
library(ggplot2)
library(GGally)
library(MASS)
library(car)
library(EBImage)


##Check the structure of dataset and missing value analysis
head(mtcars)
tail(mtcars)
str(mtcars)
summary(mtcars)
sum(is.na(mtcars))

## Exploratory data analyses
cor(mtcars)
boxplot(mtcars$mpg ~ mtcars$cyl)
boxplot(mtcars$mpg ~ mtcars$vs)
mtcars$gear <- as.numeric(mtcars$gear)
qplot(mpg, wt, data = mtcars)
qplot(gear,mpg, data=mtcars, geom=c("boxplot", "jitter"),
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon")

M<-cor(mtcars)
head(round(M,2))
corrplot(M, method="circle")
corrplot(M, method="number")
pairs(~ mpg + disp + drat + wt, data = mtcars, main = "Simple Scatterplot Matrix")
ggpairs(mtcars,axisLabels='show')
ggplot(mtcars, aes(y=mpg, x=factor(am, labels = c("automatic", "manual")), fill=factor(am)))+
  geom_violin(colour="black", size=1)+
  xlab("transmission") + ylab("MPG")

#We can form a clear hypothesis from this visualization: it appears that automatic cars have a lower miles per gallon,
#and therefore a lower fuel efficiency, than manual cars do.
#Lets confirm by doing t test

test <- t.test(mpg ~ am, data= mtcars, var.equal = FALSE, paired=FALSE ,conf.level = .95)
result <- data.frame( "t-statistic"  = test$statistic, 
                      "df" = test$parameter,
                      "p-value"  = test$p.value,
                      "lower CL" = test$conf.int[1],
                      "upper CL" = test$conf.int[2],
                      "automatic mean" = test$estimate[1],
                      "manual mean" = test$estimate[2],
                      row.names = "")
result
View(result)

#                         ________________________

##                              Model Building
##                         ________________________


# Simple linear regression model
mtcars$amfactor <- factor(mtcars$am, labels = c("automatic", "manual"))
model_1 <- lm(mpg ~ amfactor, data = mtcars)
summary(model_1)

##All the estimates provided here are in comparison with automatic transmission.
##The intercept of 17.14 is simply the mean MPG of automatic transmission.
##The slope of 7.24 is the change in the mean between manual transmission and automatic transmission.

#Multiple linear regression model

model_2 <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)
summary(model_2)

# Detecting Multicollinearity(car package)

##A major problem with multivariate regression is collinearity.
##If two or more predictor variables are highly correlated, and they are both entered into a regression model,
##it increases the true standard error and you get a very unstable estimates of the slope.

vif(model_2)

##Values for the VIF that are greater than 10 are considered large.
##We should also pay attention to VIf values between 5 and 10
##At these point we might consider leaving only one of these variables in the model

#Stepwise selection method(MASS package)

step <- stepAIC(model_2, direction="both", trace=FALSE)
summary(step)

#How to perform Stepwise Regression
model_3 <- lm(mpg ~ factor(am), data = mtcars)
model_4 <- lm(mpg ~ factor(am)+wt, data = mtcars)
model_5 <- lm(mpg ~ factor(am)+wt+qsec, data = mtcars)
model_6 <- lm(mpg ~ factor(am)+wt+qsec+hp, data = mtcars)
model_7<- lm(mpg ~ factor(am)+wt+qsec+hp+drat, data = mtcars)

summary(model_3)
summary(model_4)
summary(model_5)
summary(model_6)
summary(model_7)

# Final Model
model_8 <- lm(mpg ~ wt+qsec+factor(am), data = mtcars)
summary(model_8)

#You can observe that all the variables now are statistically significant.
#This model explains 84% of the variance in miles per gallon (mpg).
#Now when we read the coefficient for am, we say that, on average, manual transmission cars have 2.94 MPGs more than automatic transmission cars.
#However this effect was much higher than when we did not adjust for weight and qsec.


#                         ________________________

##                  Model Diagonostics(Most Important Steps)
##                         ________________________


#1. Multicollinearity in the model(car package)

vif(model_8)

#2. Autocorrelation(lmtest package)

dwtest(model_8)


##The DW values range between 0 to 4.
##The DW Statistic between 1.5 to 2 indicates no autocorrelation between errors.
##Since the DW Statistic for our model is 1.671096
##we can assume there exists no autocorrelation in our errors. 

#3. Residuals versus the fitted values

plot(model_8)

##By plotting residuals versus the fitted values,
##we're looking for any sort of pattern. Same thing with the fitted values versus the standardized,
##where it's plotting a function of the standardized residuals. Plots below show that no specific pattern exist in the residuals.
##Also you can check for the heteroskedascity


#4. Normality of residuals

qqPlot(model_8, main="Normal Q-Q plot")

#5. Influential Observations(Residual Vs Leverage)



