
################################################################
#                   Biostatistical Methods I                   #
#           Lecture 18: Multiple Linear Regression             #
#               Model Building/Variable Selection              #
#                   Last updated: 11.15.2018                   #
################################################################



rm(list = ls())

install.packages('dplyr')
install.packages('HH')                 # For VIF function
install.packages("leaps")              # Stepwise and test-based criteria

library(dplyr)
library(HH)
library("leaps")


# Read data Surgical.csv

data_surg <- surg<-read.csv("~/Desktop/Surgical.csv")
names(data_surg)

# Attach function lets you access the variables directly
attach(data_surg)


# Explorations: scatter plot matrix and pairwise correlations
pairs(data_surg)

# Helps identify collinearity 
cor(data_surg)

# Boxplots for each variable
# par(mar=c(3,3,1,1)) # if margins are too large
par(mfrow=c(2,3))
boxplot(Lnsurvival, main='Lnsurvival')
boxplot(Age, main='Age')
boxplot(Bloodclot,main='Bloodclot' )
boxplot(Progindex, main='Progindex')
boxplot(Enzyme, main='Enzyme')
boxplot(Liver, main='Liver')

# Make a data frame with ln(Survival) as the only outcome.
surg<-data.frame(data_surg[,-9])

# Fit a regression using all predictors
mult.fit <- lm(Lnsurvival ~ Bloodclot + Progindex + Enzyme + Liver + Age + Gender + Alcmod + Alcheav, data=surg)
summary (mult.fit)

# Same thing
mult.fit <- lm(Lnsurvival ~ ., data=surg)
summary(mult.fit)

#############################################################################
# Backward elimination: take out non-significant variables 'one at a time'  #
# starting with the highest p-value                                         #
#############################################################################

# No Liver
step1<-update(mult.fit, . ~ . -Liver)
summary(step1)

# No Alcmod
step2<-update(step1, . ~ . -Alcmod)
summary(step2)

# No Age
step3<-update(step2, . ~ . -Age)
summary(step3)

# No Gender
step4<-update(step3, . ~ . -Gender)
summary(step4)

# What are the effects on R2 and adj-R2? Any effect on the coefficients?


#########################################################################################
# Forward elimination: Reversed backward elimination starting with the lowest p-value   #                                                            
#########################################################################################

library(faraway)
library(broom)

### Step 1:  Fit simple linear regressions for all variables,look for the variable with lowest p-value
fit1 <- lm(Lnsurvival ~ Bloodclot, data=surg)
tidy(fit1)
fit2 <- lm(Lnsurvival ~ Progindex, data=surg)
tidy(fit2)
fit3 <- lm(Lnsurvival ~ Enzyme, data=surg)
tidy(fit3)
fit4 <- lm(Lnsurvival ~ Liver, data=surg)
tidy(fit4)
fit5 <- lm(Lnsurvival ~ Age, data=surg)
tidy(fit5)
fit6 <- lm(Lnsurvival ~ Gender, data=surg)
tidy(fit6)
fit7 <- lm(Lnsurvival ~ Alcmod, data=surg)
tidy(fit7)
fit8 <- lm(Lnsurvival ~ Alcheav, data=surg)
tidy(fit8)

# Enter first the one with the lowest p-value: Enzyme
forward1<-lm(Lnsurvival~Enzyme, data=surg)
tidy(forward1)

### Step 2: Enter the one with the lowest p-value in the rest 
fit1 <- update(forward1, . ~ . +Bloodclot)
tidy(fit1)
fit2 <- update(forward1, . ~ . +Progindex)
tidy(fit2)
fit3 <- update(forward1, . ~ . +Liver)
tidy(fit3)
fit4 <- update(forward1, . ~ . +Age)
tidy(fit4)
fit5 <- update(forward1, . ~ . +Gender)
tidy(fit5)
fit6 <- update(forward1, . ~ . +Alcmod)
tidy(fit6)
fit7 <- update(forward1, . ~ . +Alcheav)
tidy(fit7)

# Enter the one with the lowest p-value: Progindex
forward2 <- update(forward1, . ~ . + Progindex)
tidy(forward2)

### Step 3: Enter the one with the lowest p-value in the rest 
fit1 <- update(forward2, . ~ . +Bloodclot)
tidy(fit1)
fit2 <- update(forward2, . ~ . +Liver)
tidy(fit2)
fit3 <- update(forward2, . ~ . +Age)
tidy(fit3)
fit4 <- update(forward2, . ~ . +Gender)
tidy(fit4)
fit5 <- update(forward2, . ~ . +Alcmod)
tidy(fit5)
fit6 <- update(forward2, . ~ . +Alcheav)
tidy(fit6)
# Enter the one with the lowest p-value: Alcheav
forward3 <- update(forward2, . ~ . + Alcheav)
tidy(forward3)

### Step 4: Enter the one with the lowest p-value in the rest 
fit1 <- update(forward3, . ~ . +Bloodclot)
tidy(fit1)
fit2 <- update(forward3, . ~ . +Liver)
tidy(fit2)
fit3 <- update(forward3, . ~ . +Age)
tidy(fit3)
fit4 <- update(forward3, . ~ . +Gender)
tidy(fit4)
fit5 <- update(forward3, . ~ . +Alcmod)
tidy(fit5)
# Enter the one with the lowest p-value: Bloodclot
forward4 <- update(forward3, . ~ . + Bloodclot)
tidy(forward4)


### Step 5: Enter the one with the lowest p-value in the rest 
fit1 <- update(forward4, . ~ . +Liver)
tidy(fit1)
fit2 <- update(forward4, . ~ . +Age)
tidy(fit2)
fit3 <- update(forward4, . ~ . +Gender)
tidy(fit3)
fit4 <- update(forward4, . ~ . +Alcmod)
tidy(fit4)
# P-value of all new added variables are larger than 0.05, which means that they 
# are not significant predictor, and we stop here.

# The model we obtained is Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot
mult.fit <- lm(Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot)
summary(mult.fit)


############################################################################
#  Stepwise regreession                                                    #   
# 'Step' function uses AIC criterion for var selection and the default     #
#  option is 'backward'.                                                   #
#  Step is a simplified version of stepAIC()                               #
############################################################################

mult.fit <- lm(Lnsurvival ~ ., data=surg)
step(mult.fit, direction='backward')


############################################################################
#                         Test-based procedures                           #
############################################################################

# Leaps function provides all-subsets analysis

# Printing the 2 best models of each size, using the Cp criterion:
leaps(x = surg[,1:8], y = surg[,9], nbest=2, method="Cp")


# Printing the 2 best models of each size, using the adjusted R^2 criterion:
leaps(x = surg[,1:8], y = surg[,9], nbest=2, method="adjr2")

# Summary of models for each size (one model per size)
b<-regsubsets(Lnsurvival ~ ., data=surg)
   (rs<-summary(b))

# Plots of Cp and Adj-R2 as functions of parameters
par(mar=c(4,4,1,1))
par(mfrow=c(1,2))

plot(2:9, rs$cp, xlab="No of parameters", ylab="Cp Statistic")
abline(0,1)

plot(2:9, rs$adjr2, xlab="No of parameters", ylab="Adj R2")


# AIC of the 6-predictor model:

multi.fit6 <- lm(Lnsurvival ~ Age + Gender + Bloodclot + Alcheav + Progindex + Enzyme, data=data_surg)
AIC(multi.fit6)

# BIC
AIC(multi.fit6, k = log(length(Lnsurvival)))

# AIC of the 4-predictor model:
multi.fit4 <- lm(Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot)
AIC(multi.fit4)

# How do the 6- and 4-predictors models compare in terms of AIC, R-adj, Cp?

#############################################################################
#   A more compact way to look at the test-based results                    #
#############################################################################

best <- function(model, ...) 
{
  subsets <- regsubsets(formula(model), model.frame(model), ...)
  subsets <- with(summary(subsets),
                  cbind(p = as.numeric(rownames(which)), which, rss, rsq, adjr2, cp, bic))
  
  return(subsets)
}  


# Select the 'best' model of all subsets for 4-predictor model
round(best(multi.fit4, nbest = 1), 4)


# Checking the model assumptions for the model: Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot
par(mfrow=c(2,2))
plot(multi.fit4)


