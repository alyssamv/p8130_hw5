
################################################################
#                   Biostatistical Methods I                   #
#           Lecture 17: Multiple Linear Regression             #
#                     Model Diagnostics                        #
#                 Last updated: 11.08.2018                     #
################################################################

rm(list = ls())

install.packages('dplyr')
install.packages('HH')                 # For VIF function
library(dplyr)
library(HH)


# Read data Surgical.csv
data_surg<-read.csv("O:\\Teaching\\Methods1\\P8130_Fall18\\Lectures\\Lecture17\\Surgical.csv")
names(data_surg)
attach(data_surg)

# Residuals vs fitted values plot
par(mfrow = c(1, 2))

fit1 <- lm(Survival ~ Bloodclot + Progindex + Enzyme + Liver, data=data_surg)
plot(fitted(fit1), resid(fit1), xlab = "Predicted/Fitted value", ylab = "Residual")
title("(a) Residual Plot for Y (Survival) ")
abline(0, 0)

# MLR with LnSurvival - natural log transformation of "Survival" outcome
fit2 <- lm(Lnsurvival ~ Bloodclot + Progindex + Enzyme + Liver,  data=data_surg)
plot(fitted(fit2), resid(fit2), xlab = "Predicted/Fitted value", ylab = "Residual")
title("(b) Residual Plot for lnY (LnSurvival)")
abline(0, 0)


#Residuals vs one covariate: use data Hospital.csv

data_hosp<-read.csv("O:\\Teaching\\Methods1\\P8130_Fall18\\Lectures\\Lecture15\\Hospital.csv")

fit3<-lm(LOS~NURSE,data=data_hos)
plot(data_hos$NURSE, fit3$residuals)
abline(h=0, lwd=2, col=2)

#Residuals vs one covariate
fit4=lm(log(LOS)~NURSE,data=data_hos)
plot(data_hos$NURSE, fit4$residuals)
abline(h=0, lwd=2, col=2)


# Quantile - Quantile plot (QQ-plot)

par(mfrow = c(1, 2))

qqnorm(resid(fit1), xlab = "Expected Value", ylab = "Residual", main = "")
qqline(resid(fit1))
title("(a) QQ Plot for Y (Survival)")

qqnorm(resid(fit2), xlab = "Expected Value", ylab = "Residual", main = "")
qqline(resid(fit2))
title("(d) QQ Plot lnY (LnSurvival)")


# Obtain all (4) diagnostic plots: EASIER and fast check of the MLR diagnostics
# Plot the regression object
par(mfrow=c(2,2))
plot(fit2)


#################################################################################
#                       Box-Cox transformation                                  #
#################################################################################


library(MASS)
fit1 <- lm(Survival ~ Bloodclot, data=data_surg)
boxcox(fit1)  # default grid of lambdas is -2 to 2 by 0.1

# Could change grid of lambda values
boxcox(fit1, lambda = seq(-3, 3, by=0.25) ) 


# Box Cox for multiple regression
mult.fit1 <- lm(Survival ~ Bloodclot + Progindex + Enzyme + Liver + Age + Gender + Alcmod + Alcheav, data=data_surg) 
summary(mult.fit1)

boxcox(mult.fit1) 
plot(mult.fit1)


mult.fit2 <- lm(Lnsurvival ~ Bloodclot + Progindex + Enzyme + Liver + Age + Gender + Alcmod + Alcheav, data=data_surg) 
summary(mult.fit2)
boxcox(mult.fit2) 
plot(mult.fit2)

#####################################################################################
#           Outliers, leverage and influential points                               #
#####################################################################################

# rstandard function gives the INTERNALLY studentized residuals 

stu_res<-rstandard(mult.fit1)
outliers_y<-stu_res[abs(stu_res)>2.5]

# Measures of influence:
# Gives DFFITS, Cook's Distance, Hat diagonal elements, and others.

influence.measures(mult.fit1)

# Look at the Cook's distance lines and notice obs 5 and 28 as potential Y outliesrs / influential points

par(mfrow=c(2,2))
plot(mult.fit1)

# Examine results with and without observations 5 and 28 that have very high survivals (>2000)
summary(mult.fit1)
plot(data_surg$Bloodclot, data_surg$Survival)
surg_only5_28<-data_surg[c(5,28),]


# Remove observations 5 and 28
surg_no5_28<-data_surg[c(-5,-28),]
plot(surg_no5_28$Bloodclot, surg_no5_28$Survival)
mult.fit_no5_28<- lm(Survival ~ Bloodclot + Progindex + Enzyme + Liver + Age + Gender + Alcmod + Alcheav, data=surg_no5_28) 

summary(mult.fit_no5_28)

influence.measures(mult.fit_no5_28)

par(mfrow=c(2,2))
plot(mult.fit_no5_28)


#####################################################################################
#                             Multicollinearity                                     #
#####################################################################################
 
# Generate data
set.seed(1)

data.multi = data.frame(age = round(rnorm(100, 30, 10),2)) %>%
  mutate(height = 25 + 2*age + rnorm(100, 0, 15),
         age_copy=age, age_new=age + rnorm(100, 0, 2))

# Correlation matrix for all variables
round(cor(data.multi),3)

# Scatter plot matrix for all variables
pairs(data.multi)


# Fit a simple linear regression with original age
reg1<-lm(height~age, data=data.multi)
summary(reg1)

# Fit a linear regression with original age and age_copy
reg2<-lm(height~age+age_copy, data=data.multi)
summary(reg2)

# Fit a linear regression with original age and age_new
reg3<-lm(height~age+age_new, data=data.multi)
summary(reg3)

# Fit a linear regression with original age, age_copy and age_new
reg4<-lm(height~age+age_copy+age_new, data=data.multi)
summary(reg4)

# Calculate the variance inflation factor (VIF)
vif(reg4)


# Data Surgical
# Checking for multicollinearity: VIF
vif(mult.fit_no5_28)


