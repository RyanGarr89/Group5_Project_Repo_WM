library(readr)
library(boot)

Default <- read_csv("Default.csv")

Default$default <- factor(Default$default) # default -> factor

head(Default)
summary(Default)

## Problem 1 (A) Fit a Logistic Regression Model

# Fitting a logistic regression model
default_logit <- glm(default ~ income + balance, data = Default, family = binomial)

summary(default_logit)

# Interpretation: In this part, we fit a logistic regression model to predict whether an individual defaults on their credit card using income and balance as predictors. From the model output, both predictors are statistically significant, with balance having a much stronger effect than income. This indicates that higher credit card balances are strongly associated with an increased probability of default, while income has a smaller but still statistically significant relationship with default risk as both p-values are <0.001

## Problem 1 (B) Validation Set Approach

# Part 1: Split Data Into Training & Validation Sets
set.seed(1)

train <- sample(1:nrow(Default), nrow(Default) / 2)

train_set <- Default[train, ]
validation_set <- Default[-train, ]

# Interpretation: We randomly split the dataset into two equal parts. One half of the observations is used as a training set to fit the model, and the other half is held out as a validation set to evaluate the model’s performance on unseen data.

# Part 2: Fit the logistic regression model on the training data
default_logit_train <- glm(default ~ income + balance, data = train_set, family = binomial)
summary(default_logit_train)

# Interpretation: Using only the training observations, we fit a logistic regression model with income and balance as predictors. Both variables remain statistically significant, which is consistent with the results from part (A). Once again, balance appears to be the dominant predictor of default.

# Part 3: Predict default status on the validation set
validation_probs <- predict(default_logit_train, newdata = validation_set, type = "response")

# Classify as default if probability > 0.5
validation_pred <- ifelse(validation_probs > 0.5, "Yes", "No")

# Interpretation: We then use the fitted model to compute the predicted probability of default for each individual in the validation set. Individuals with a predicted probability greater than 0.5 are classified as defaulters, while the remaining individuals are classified as non-defaulters.

# Part 4: Calculate the validation set error
confusion_matrix_validation <- table(Predicted = validation_pred, Actual = validation_set$default)

confusion_matrix_validation

validation_error <- mean(validation_pred != validation_set$default)
validation_error

# Interpretation: The confusion matrix shows that the model correctly classifies the vast majority of observations in the validation set, with relatively few misclassifications. The validation set error is approximately 2.54%, meaning that about 97.5% of the validation observations are classified correctly. This low error rate suggests that the logistic regression model generalizes well to new data.
```
Overall Interpretation:
Using the validation set approach, we randomly split the data into a training set and a validation set. We then fit a logistic regression model using income and balance on the training observations only. The fitted model was used to compute posterior probabilities of default for each individual in the validation set, and individuals with a predicted probability greater than 0.5 were classified as defaulters.

The confusion matrix shows that the model correctly classifies the vast majority of validation observations, with relatively few misclassifications. The validation set error is approximately 2.54%, meaning that about 97.5% of the validation observations are classified correctly. This suggests that the logistic regression model provides an accurate estimate of default risk and generalizes well to unseen data.

## Problem 1(C) Use LOOCV to split observations
set.seed(1)

default_glm <- glm(default ~ income + balance, data=Default, family = binomial)

# This function will do steps 3 and 4 that we saw in part B. It classifies Yes if p_hat(predicted probabilities) is greater than 0.5 and it returns the 0-1 loss(misclassification indicator), so that the average of these losses across the LOOCV splits is the LOOCV misclassification error.
cost_misclass_05 <- function(y,p_hat) {
  pred_class <-ifelse(p_hat >0.5, "Yes", "No")
  mean(pred_class != ifelse(y=="Yes", "Yes", "No"))
}
# Cv.glm is used because it automatically sets K equal to the number of rows in the data which is the LOOCV.
cv_loocv <- cv.glm(data = Default, glmfit = default_glm, cost=cost_misclass_05)

# Delta 1 is the direct average misclassification rate. It is computed using the 0.5 cutoff and is comparable to the validation-set error.
loocv_err <- cv_loocv$delta[1]
loocv_err
# The average misclassification rate using LOOCV is approximately 1.46%, meaning that about 98.54% of the observations are classified accurately. This provides strong evidence of stable out-of-sample performance.The LOOCV misclassification rate is lower than the validation error which reflects the reduced bias of LOOCV. Since both estimates are relatively close we can conclude that the model generalizes well and its performance estimate is stable.



library(boot)
set.seed(1)

Default <- read.csv("Insert your file path")
Default$default <- as.factor(Default$default)

# 2a Defines boot.fn
boot.fn <- function(data, index) {
  coef(
    glm(default ~ income + balance,
        data = data,
        family = binomial,
        subset = index)
  )[c("income", "balance")]
}

# 2b Bootstrap with 1000 resamples
set.seed(1)
boot.out <- boot::boot(Default, boot.fn, R = 1000)

boot.out

# Print Bootstrap standard errors for income and balance
apply(boot.out$t, 2, sd)
