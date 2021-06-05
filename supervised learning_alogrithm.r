
#naive bayes
library(e1071)
## building naiveBayes classifier.
salary_classifier <- naiveBayes(x_train, y_train)
## building naiveBayes classifier with laplace value
salary_classifier_lap <- naiveBayes(x_train, y_train,laplace = 3)
##  Evaluating model performance with out laplace
salary_test_pred <- predict(salary_classifier, x_test)
## test accuracy
test_acc <- mean(salary_test_pred == y_test)
test_acc #0.81
#train accuracy
salary_train_pred <- predict(salary_classifier,x_train)
train_acc <- mean(salary_train_pred==y_train)
train_acc #0.82

#K Nearest Neighbor
library(class)
test_pred <- knn(train = x_train,test =x_test, cl=df_train$Type, k=7)
accu <- mean(test_pred==df_test$Type)
conf_mat <- table(x=test_pred,y=df_test$Type)

#decision tree
library(C50)
model <- C5.0(x_train[, -9], x_train$Class.variable)
plot(model) 
#prediction on test data
test_pred <- predict(model,x_test[,-9])
test_acc <- mean(test_pred==x_test$Class.variable)
test_acc #0.74
#prediction on train data
train_pred <- predict(model,x_train[,-9])
train_acc <- mean(train_pred==x_train$Class.variable)
train_acc #0.84 #model is over fitting.
#cross table
table(test_pred,x_test$Class.variable)
table(train_pred,x_train$Class.variable)

#Random Forest Classifier and bagging
# install.packages("randomForest")
library(randomForest)
rf <- randomForest(x_train$Class.variable ~ ., data = x_train)
#prediction on test data
test_pred_rf <- predict(rf,x_test)
test_acc_rf <- mean(test_pred_rf==x_test$Class.variable)
test_acc_rf #0.77
#prediction on train data
train_pred_rf <- predict(rf,x_train)
train_acc_rf <- mean(train_pred_rf==x_train$Class.variable)
train_acc_rf #1 #model is biased.
#cross table
table(test_pred_rf,x_test$Class.variable)
table(train_pred_rf,x_train$Class.variable)

#Adaboosting
library(adabag)
adaboost <- boosting(Start_Tech_Oscar ~ ., data = movies_train, boos = TRUE)
# Test data
adaboost_test <- predict(adaboost, movies_test)
table(adaboost_test$class, movies_test$Start_Tech_Oscar)
mean(adaboost_test$class == movies_test$Start_Tech_Oscar)
# Train data
adaboost_train <- predict(adaboost, movies_train)
table(adaboost_train$class, movies_train$Start_Tech_Oscar)
mean(adaboost_train$class == movies_train$Start_Tech_Oscar)

#GradientBoosting
library(gbm)
boosting <- gbm(movies_train$Collection ~ ., data = movies_train, distribution = 'gaussian',
                n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
# distribution = Gaussian for regression and Bernoulli for classification
# Prediction for test data result
boost_test <- predict(boosting, movies_test, n.trees = 5000)
rmse_boosting <- sqrt(mean(movies_test$Collection - boost_test)^2)
# Prediction for train data result
boost_train <- predict(boosting, movies_train, n.trees = 5000)
rmse_train <- sqrt(mean(movies_train$Collection - boost_train)^2)

#XGBoosting
library(xgboost)
# DMatrix on train
Xmatrix_train <- xgb.DMatrix(data = train_x, label = train_y)
# DMatrix on test 
Xmatrix_test <- xgb.DMatrix(data = test_x, label = test_y)
# Max number of boosting iterations - nround
xg_boosting <- xgboost(data = Xmatrix_train, nround = 50,objective = "multi:softmax", eta = 0.3, 
                       num_class = 2, max_depth = 100)
# Prediction for test data
xgbpred_test <- predict(xg_boosting, Xmatrix_test)
table(test_y, xgbpred_test)
mean(test_y == xgbpred_test)
# Prediction for train data
xgbpred_train <- predict(xg_boosting, Xmatrix_train)
table(train_y, xgbpred_train)
mean(train_y == xgbpred_train)

#Voting
library(randomForest)
# Random Forest Analysis
cc_RF <- randomForest(good_bad ~ ., data = cc_Train, keep.inbag = TRUE, ntree = 500)
# Overall class prediction (hard voting) # it gives maximum votes in which 500 DT sampling of each row had
cc_RF_Test_Margin <- predict(cc_RF, newdata = cc_TestX, type = "class")
# Prediction(soft)#if we want to check individually 500 DT output of samples.
cc_RF_Test_Predict <- predict(cc_RF, newdata = cc_TestX, type = "class", predict.all = TRUE)
sum(cc_RF_Test_Margin == cc_RF_Test_Predict$aggregate)
# Majority Voting
dim(cc_RF_Test_Predict$individual)
# View(cc_RF_Test_Predict$individual) # Prediction at each tree
Row_Count_Max <- function(x) names(which.max(table(x)))
Voting_Predict <- apply(cc_RF_Test_Predict$individual, 1, Row_Count_Max)
all(Voting_Predict == cc_RF_Test_Predict$aggregate)
all(Voting_Predict == cc_RF_Test_Margin)
mean(Voting_Predict == cc_TestY)
mean(cc_RF_Test_Predict$aggregate==cc_TestY)



