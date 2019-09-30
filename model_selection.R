## MAE5904

library(ISLR)
data("Hitters")
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# Removing missing values
Hitters <- na.omit(Hitters)


# Best subset -------------------------------------------------------------

library(leaps)
regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)

reg.summary <- summary(regfit.full)
names(reg.summary)

# Adjusted R squared
plot(reg.summary$adjr2, type = "l")


# foward and backward stepwise --------------------------------------------

regfwd.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfwd.full)

regbwd.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regbwd.full)

coef(regfit.full,5)
coef(regfwd.full,5)
coef(regbwd.full,5)


# LASSO -------------------------------------------------------------------

library(glmnet)

x = model.matrix(Salary ~ ., Hitters)
