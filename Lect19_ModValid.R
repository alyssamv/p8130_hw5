
################################################################
#                   Biostatistical Methods I                   #
#           Lecture 19: Multiple Linear Regression             #
#                      Model Validation                        #
#                   Last updated: 11.17.2018                   #
################################################################


rm(list = ls())

install.packages(c('boot', 'caret','MPV'))              


# Read data Surgical.csv and keep only lnSurvival as outcome
data_surg<-read.csv("O:\\Teaching\\Methods1\\P8130_Fall18\\Lectures\\Lecture17\\Surgical.csv")
surg_lnSurv<-data.frame(data_surg[,-9])

library(caret)

# Use 5-fold validation and create the training sets

data_train<-trainControl(method="cv", number=5)

# Fit the 4-variables model that we discussed in previous lectures
model_caret<-train(Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot,
                   data=surg_lnSurv,
                   trControl=data_train,
                   method='lm',
                   na.action=na.pass)
  
# Model predictions using 4 parts of the data fro training 
model_caret

#RMSE=0.2130177,Rsquared=0.8342916 - good % variantion acconted for

# Model coefficients
model_caret$finalModel

# Examine model prediction for each fold
model_caret$resample

# Look at standard deviation around the Rsquared value by examining the R-squared from each fold.
sd(model_caret$resample$Rsquared)



# Let's look at the model using all data, no CV
full_model<-lm(Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot, data=surg_lnSurv)
summary(full_model)

# Full data had an Rsquared=0.8299; what was the CV result for Rsquared?
# What about MSE values?


# Exercise for you: try 'caret' and split the data 50-50


#####################################################
#                        LOOCV                      #
#####################################################

# Use glm() instead of lm() because of cv.glm() function
glm.fit<-glm(Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot, data=surg_lnSurv)


library(boot)                          # For cv.glm()
cv.err<-cv.glm(surg_lnSurv,glm.fit)

# The two delta values should be similar: we use the first one
# The second value is bias corrected
cv.err$delta   

# Next steps are to fit models with other predictor combinations and compare the CVs

########################################################################
# Bootstrap to assess the variability of model estimates: b0 and b1    #
########################################################################

boot.fn<-function(data, index){
	return(coef(lm(Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot, data=data_surg, subset=index)))
}

# Our usual regression, no bootstrap yet
boot.fn(surg_lnSurv,1:54)


# Compute the estimates by sampling with replacement
# Sample chooses 54 observations from 54, with replacement
# Might have duplicates
set.seed(1)

# One draw
boot.fn(data_surg, sample(54,54, replace=T))

# Use function boot() to repeat the sampling 10000 times.
# Repeat 10000 times to get the estimates, SEs ad bias

boot::boot(data_surg, boot.fn, 10000)

# How does it compare to the original (non-bootstrap) estimates?


########################################################################
#                   Model validation: criteria                         #
########################################################################

library(MPV)                           # For PRESS criterion

newsummary <- function(model)
{
    list('coefs'    = round(t(summary(model)$coef[, 1:2]), 4),
         'criteria' = cbind('SSE'   = anova(model)["Residuals", "Sum Sq"],
                            'PRESS' = PRESS(model),
                            'MSE'   = anova(model)["Residuals", "Mean Sq"],
                            'Rsq'   = summary(model)$adj.r.squared))
}

newsummary(lm(Lnsurvival ~ Bloodclot + Alcheav + Progindex + Enzyme, surg_lnSurv))



####################################################################
#               Data Generation and Cross Validation              #
####################################################################


## Generate 100 training sets each of size 50 from a polynomial regression model
# Fit a sequence of cubic spline models with degrees of freedom from 1 to 20.

set.seed(3)

gen_data <- function(n, beta, sigma_eps) {
  eps <- rnorm(n, 0, sigma_eps)
  x <- sort(runif(n, 0, 100))
  X <- cbind(1, poly(x, degree = (length(beta) - 1), raw = TRUE))
  y <- as.numeric(X %*% beta + eps)
  
  return(data.frame(x = x, y = y))
}

# Fit the models
require(splines)

n_rep <- 100
df <- 1:20
beta <- c(5, 0.3, 0.001, -3e-05)           
n_test <- 100
n_train<-50
sigma_eps <- 0.5

xy <- res <- list()

# Test data
xy_test <- gen_data(n_test, beta, sigma_eps)

# Train data
for (i in 1:n_rep) {
  xy[[i]] <- gen_data(n_train, beta, sigma_eps)
  x <- xy[[i]][, "x"]
  y <- xy[[i]][, "y"]
  res[[i]] <- apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
}

# Plot true and training data with overlaid models

#dev.off()

x <- xy[[1]]$x
X <- cbind(1, poly(x, degree = (length(beta) - 1), raw = TRUE))
y <- xy[[1]]$y
plot(y ~ x, col = "gray", lwd = 2)
lines(x, X %*% beta, lwd = 3, col = 1)                        #True model
lines(x, fitted(res[[1]][[1]]), lwd = 3, col = 2)
lines(x, fitted(res[[1]][[3]]), lwd = 3, col = 3)
lines(x, fitted(res[[1]][[20]]), lwd = 3, col = 4)
legend(x = "bottomright", legend = c("True function", "Linear fit (df = 1)", "Good model (df = 3)", 
                                     "Overfitted model (df = 20)"), lwd = rep(3, 4), col = c(1,2,3,4), cex = 0.85)




# k-fold cross-validation using simulated data. Generate 100 observations and choose k=10.
# The plot shows the training, test errors, CV and standard errors of the individual prediction error for each of the k=10 parts.

n_train <- 100
xy <- gen_data(n_train, beta, sigma_eps)
x <- xy$x
y <- xy$y

fitted_models <- apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
mse <- sapply(fitted_models, function(obj) deviance(obj)/nobs(obj))

n_test <- 100
xy_test <- gen_data(n_test, beta, sigma_eps)
pred <- mapply(function(obj, degf) predict(obj, data.frame(x = xy_test$x)), 
               fitted_models, df)
te <- sapply(as.list(data.frame(pred)), function(y_hat) mean((xy_test$y - y_hat)^2))

n_folds <- 10
folds_i <- sample(rep(1:n_folds, length.out = n_train))
cv_tmp <- matrix(NA, nrow = n_folds, ncol = length(df))
for (k in 1:n_folds) {
  test_i <- which(folds_i == k)
  train_xy <- xy[-test_i, ]
  test_xy <- xy[test_i, ]
  x <- train_xy$x
  y <- train_xy$y
  fitted_models <- apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
  x <- test_xy$x
  y <- test_xy$y
  pred <- mapply(function(obj, degf) predict(obj, data.frame(ns(x, df = degf))), 
                 fitted_models, df)
  cv_tmp[k, ] <- sapply(as.list(data.frame(pred)), function(y_hat) mean((y - 
                                                                           y_hat)^2))
}
cv <- colMeans(cv_tmp)

require(Hmisc)

plot(df, mse, type = "l", lwd = 2, col = gray(0.4), ylab = "Prediction error", 
     xlab = "Degrees of freedom (log scaled)", main = paste0(n_folds, 
                                                                     "-fold Cross-Validation"), log = "x")

# For a better vizualization of the errors, you can change the ylim=c(0.01,1)

lines(df, te, lwd = 2, col = "darkred", lty = 2)
cv_sd <- apply(cv_tmp, 2, sd)/sqrt(n_folds)
errbar(df, cv, cv + cv_sd, cv - cv_sd, add = TRUE, col = "steelblue2", pch = 19, 
       lwd = 0.5)
lines(df, cv, lwd = 2, col = "steelblue2")
points(df, cv, col = "steelblue2", pch = 19)
legend(x = "topright", legend = c("Training error", "Test error", "Cross-validation error"), 
       lty = c(1, 2, 1), lwd = rep(2, 3), col = c(gray(0.4), "darkred", "steelblue2"), cex = 0.85)
