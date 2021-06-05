# simple Linear Regression model

# Correlation Coefficient
cor(Waist, AT)
# Covariance
cov(Waist, AT)

reg <- lm(AT ~ Waist, data = wc.at) # Y ~ X
summary(reg)
confint(reg, level = 0.95)
pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

#ggplot for adding Regression line for data
library(ggplot2)
ggplot(data = wc.at, aes(Waist, AT) ) +
     geom_point(color = 'blue') + stat_smooth(method = lm, formula = y ~ x) +
        geom_line(color = 'red', data = wc.at, aes(x = Waist, y = pred$fit))

# Alternate way
ggplot(data = wc.at, aes(x = Waist, y = AT)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = wc.at, aes(x = Waist, y = pred$fit)) #line is w.r.t pred

# Evaluation the model for fitness 
cor(pred$fit, wc.at$AT)
reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse1 <- sqrt(mean((wc.at$AT-pred$fit)^2))

# Transformation Techniques
# input = log(x); output = y

plot(log(Waist), AT)
cor(log(Waist), AT)
reg_log <- lm(AT ~ log(Waist), data = wc.at)
summary(reg_log)
confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")
pred <- as.data.frame(pred)
rmse <- sqrt(mean(reg_log$residuals^2))

# Regression line for data
ggplot(data = wc.at, aes(log(Waist), AT) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ log(x))
# Alternate way
ggplot(data = wc.at, aes(x = log(Waist), y = AT)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = wc.at, aes(x = log(Waist), y = pred$fit))


# Log transformation applied on 'y'
# input = x; output = log(y)
plot(Waist, log(AT))
cor(Waist, log(AT))
reg_log1 <- lm(log(AT) ~ Waist, data = wc.at)
summary(reg_log1)
predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
#reg_log1$residuals
#sqrt(mean(reg_log1$residuals^2)) #here log(at) is not converted
pred <- exp(predlog)  # Antilog = Exponential function #convert fit value
pred <- as.data.frame(pred)
res_log1 = AT - pred$fit
rmse <- sqrt(mean(res_log1^2))
# Regression line for data
ggplot(data = wc.at, aes(x=Waist,y=log(AT)) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ x)
# Alternate way
ggplot(data = wc.at, aes(x = Waist, y = log(AT))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = wc.at, aes(x = Waist, y = predlog$fit))

# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)
reg2 <- lm(log(AT) ~ Waist + I(Waist*Waist), data = wc.at)
summary(reg2)
predlog <- predict(reg2, interval = "predict")
predlog<- as.data.frame(predlog)
pred <- exp(predlog)
pred <- as.data.frame(pred)
cor(pred$fit, wc.at$AT)
res2 = AT - pred$fit
rmse <- sqrt(mean(res2^2))
# Regression line for data
ggplot(data = wc.at, aes(Waist + I(Waist*Waist), log(AT)) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ x+I(x^2))
# Alternate way
ggplot(data = wc.at, aes(x = Waist + I(Waist*Waist), y = log(AT))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = wc.at, aes(x = Waist + I(Waist^2), y = predlog$fit))

#multiple Linear Regression model

pairs(Cars)   #Scatter plot for all pairs of variables
plot(Cars)
#### Scatter plot matrix with Correlations inserted in graph
#install.packages("GGally")
library(GGally)
ggpairs(Cars)
### Partial Correlation matrix
#install.packages("corpcor")
library(corpcor)
cor(Cars)# correlation matrix
cor2pcor(cor(Cars))
# The Linear Model of interest
model.car <- lm(MPG ~ VOL + HP + SP + WT, data = Cars) # lm(Y ~ X)
summary(model.car)
model.carV <- lm(MPG ~ VOL)
summary(model.carV)
model.carW <- lm(MPG ~ WT)
summary(model.carW)
model.carVW <- lm(MPG ~ VOL + WT)
summary(model.carVW)

# Diagnostic Plots
#install.packages("car")
library(car)
plot(model.car)# Residual Plots, QQ-Plot, Std. Residuals vs Fitted, Cook's distance
qqPlot(model.car, id.n = 5) # QQ plots of studentized residuals, helps identify outlier
# Deletion Diagnostics for identifying influential obseravations
influenceIndexPlot(model.car, id.n = 3) # Index Plots of the influence measures
influencePlot(model.car, id.n = 3) # A user friendly representation of the above
# Regression after deleting the 77th observation
model.car1 <- lm(MPG ~ VOL + HP + SP + WT, data = Cars[-77, ])
summary(model.car1)
### Variance Inflation Factors
vif(model.car)  # VIF is > 10 => collinearity
# Regression model to check R^2 on Independent variales
VIFWT <- lm(WT ~ VOL + HP + SP)
VIFVOL <- lm(VOL ~ WT + HP + SP)
summary(VIFWT)
summary(VIFVOL)
# VIF of SP
1/(1-0.94)

#### Added Variable Plots ######
avPlots(model.car, id.n = 2, id.cex = 0.8, col = "red")

pred <- predict(model, newdata = test)
actual <- test$MPG
error <- actual - pred
test.rmse <- sqrt(mean(error**2))
test.rmse

#Logestic Regression

# The output of sigmoid function lies in between 0-1
model <- glm(ATTORNEY ~ ., data = claimants, family = "binomial")
summary(model)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model)) #converting to odd ratio ans appling exp to coefficents
# Prediction to check model validation
prob <- predict(model, claimants, type = "response")
# or use plogis for prediction of probabilities
prob <- plogis(predict(model, claimants))
# Confusion matrix and considering the threshold value as 0.5 
confusion <- table(prob > 0.5, claimants$ATTORNEY)

# Model Accuracy 
Acc <- sum(diag(confusion)/sum(confusion))
# Convert the probabilities to binary output form using cutoff
pred_values <- ifelse(prob > 0.5, 1, 0)

library(caret)
# Confusion Matrix
confusionMatrix(factor(claimants$ATTORNEY, levels = c(0, 1)), factor(pred_values, levels = c(0, 1)))

# Decide on optimal prediction probability cutoff for the model
library(InformationValue)
optCutOff <- optimalCutoff(claimants1$ATTORNEY, prob_full)
optCutOff
# Check multicollinearity in the model
library(car)
vif(fullmodel)

# Misclassification Error - the percentage mismatch of predcited vs actuals
# Lower the misclassification error, better the model.
library(misclassGLM)
misClassError(claimants1$ATTORNEY, prob_full, threshold = optCutOff)

# ROC curve
# Greater the area under the ROC curve, better the predictive ability of the model
library(plotROC)
plotROC(claimants1$ATTORNEY, prob_full)

# Confusion Matrix
predvalues <- ifelse(prob_full > optCutOff, 1, 0)
results <- confusionMatrix(predvalues, claimants1$ATTORNEY)
sensitivity(predvalues, claimants1$ATTORNEY)
confusionMatrix(actuals = claimants1$ATTORNEY, predictedScores = predvalues)


# Multinomial Logit Model

require('mlogit') #require is same as library
require('nnet')
commute <- multinom(choice ~ cost.car + cost.carpool + cost.bus + cost.rail + time.car + time.carpool + time.bus + time.rail, data = train)
summary(commute)
# If in case the baseline should be changed 
train$choice  <- relevel(train$choice, ref= "carpool")

##### Significance of Regression Coefficients###
z <- summary(commute)$coefficients / summary(commute)$standard.errors
p_value <- (1 - pnorm(abs(z), 0, 1)) * 2 #pvalue<0.5 significance of model
summary(commute)$coefficients
# odds ratio 
exp(coef(commute))
# check for fitted values on training data
prob <- fitted(commute) #fitted is similar to predict
# Predicted on test data
pred_test <- predict(commute, newdata =  test, type = "probs") # type="probs" is to calculate probabilities
# Find the accuracy of the model
class(pred_test)
pred_test <- data.frame(pred_test)
View(pred_test)
pred_test["prediction"] <- NULL
# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))
}
predtest_name <- apply(pred_test, 1, get_names) #here 1 indicates rows
pred_test$prediction <- predtest_name
# Confusion matrix
table(predtest_name, test$choice)

# barplot(table(predtest_name, test$choice), beside = T, col =c("red", "lightgreen", "blue", "orange"), legend = c("bus", "car", "carpool", "rail"), main = "Predicted(X-axis) - Legends(Actual)", ylab ="count")
barplot(table(predtest_name, test$choice), beside = T, col =c("red", "lightgreen", "blue", "orange"), main = "Predicted(X-axis) - Legends(Actual)", ylab ="count")
# Accuracy on test data
mean(predtest_name == test$choice)


# Ordinal Logistic Regression
library(carData)
library(MASS)
summary(WVS)
table(WVS$poverty)
# Ordinal Logistic Regression
# Proportional Odds Logistic Regression - polr
model <- polr(poverty ~ religion + degree + country + age + gender, data = WVS, Hess = TRUE)
summary(model)
# Significance check of coefficients
summary_table <- coef(summary(model))
p_val <- pnorm(abs(summary_table[, "t value"]), lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(p_val,3))
summary_table
# Prediction on new data
new_data <- data.frame("religion"= "yes","degree"="no","country"="Norway","age"=30,"gender"="male")
prob <- predict(model,new_data, type = "p")


