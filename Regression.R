####################### Regression Codes
# This script contains all the commonly used ML regression algorithms syntax

library(e1071)
library(caret)
library(randomForest) 
library(glmnet)
library(xgboost)
library(gbm)

################ Linear Regression

LR_Function = function(X_train , data_model) {
  lm( X_train ~ ., data =data_model)
}

Linear_Regression = LR_Function(  training[,target],training[, names(training) != target])
summary(Linear_Regression)


################ Linear Regression (Lasso) including hyperparameter tuning

library(glmnet)

lambdas <- 10^seq(2, -3, by = -.1) ### Setting the parameters


# Setting alpha = 1 implements lasso regression
Lasso_Function = function(X_train , y_train) {
  suppressWarnings(cv.glmnet(X_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE))
}

lasso = Lasso_Function(training[,target],training[, names(training) != target])

# Best parameters
lambda_best <- lasso$lambda.min 
lambda_best


################ Linear Regression (Ridge) including hyperparameter tuning

library(glmnet)

lambdas <- 10^seq(2, -3, by = -.1) ### Setting the parameters

# Setting alpha = 0 implements ridge regression
Ridge_Function = function(X_train , y_train) {
  suppressWarnings(cv.glmnet(X_train, y_train, alpha = 0, lambda = lambdas, standardize = TRUE))
}

ridge = Ridge_Function(training[,target],training[, names(training) != target])

# Best parameters
ridge_best <- ridge$lambda.min 
ridge_best


################ Linear Regression (Elastic Net) including hyperparameter tuning

#Elastic net is a regularized regression method that linearly combines the L1 and L2 penalties of the lasso and ridge methods
#The solution is to combine the penalties of ridge regression and lasso to get the best of both worlds.
#??(alpha) is the mixing parameter between ridge (??=0) and lasso (??=1).
#??(lambda) parameter is the regularization penalty. Setting ?? to 0 is the same as using the OLS, while the larger its value, the stronger is the coefficients' size penalized.


library(glmnet)

ElasticGrid <- expand.grid(alpha = seq(0,1,by = 0.01), lambda = seq(0,40,by = 0.1) )
my_control <-trainControl(method="cv", number=5, allowParallel = TRUE) 

EN_Function = function(X_train , y_train) {
  suppressWarnings(train(x=X_train, y=y_train, method='glmnet',
                         trControl= my_control, tuneGrid=ElasticGrid , preProcess = c("center", "scale")))
}

Elastic_Net = EN_Function(training[, names(training) != target],training[,target])

Elastic_Net$bestTune


################ Regression Tree including hyperparameter tuning

library(rpart)

### Building hyper parameter grid

hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

### Model Building

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = target ~ .,
    data    = training,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

### to get the best models

get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)


################ Random Forest

#mtry - Number of variables available for splitting at each tree node.

RFGrid <- expand.grid(.mtry=c(1:50))

RF_Function = function(X_train , y_train) {
  suppressWarnings(train(x=X_train, y=y_train,
                         method="rf",tuneGrid=RFGrid, importance = TRUE,trControl=my_control,ntree=500)
  )
}

RF = RF_Function(training[, names(training) != target],training[,target])

RF$bestTune

print(RF)
varImp(RF)
plot(RF)


################ SVM including hyperparameter tuning

my_control <-trainControl(method="cv", number=5, allowParallel = TRUE) 

polyGrid <- expand.grid(degree = c(1,2),
                        scale =  seq(0.01,1,by = 0.1),
                        C = seq(0.01,1,by = 0.1))


SVM_Function = function(X_train , y_train) {
  suppressWarnings(train(X_train,y_train, method = "svmPoly", tuneGrid=polyGrid ,
                         preProcess = c("center", "scale"),
                         trControl = my_control)
  )
}

# tune_svm <- tune(svm, train.x=x, train.y=y, 
#                  kernel="radial", ranges=list(cost=10^(-2:2), gamma=c(.25,.5,1,2)))

SVM = SVM_Function(training[, names(training) != target],training[,target])
plot(SVM)
SVM$bestTune

################ GBM

## Parameters in GBM
# n.trees - Boosting Iterations
# interaction.depth - Max Tree Depth  
# n.minobsinnode - Minimum number of observations in the terminal nodes of the trees. This is the actual number of observations, not the total weight.
# shrinkage - srinkage Also known as the learning rate or step-size reduction

GBMGrid <- expand.grid(interaction.depth = seq(1, 5, by = 1),
                       n.trees = seq(100, 500, by = 100),
                       n.minobsinnode = seq(1, 10, by = 2),
                       shrinkage = seq(.01, 1, by = .1)) 


GBM_Function = function(X_train , y_train) {
  suppressWarnings(train(x=X_train, y=y_train,
                         method = "gbm",
                         trControl = my_control,        
                         tuneGrid=GBMGrid,verbose=FALSE)
  )
}

GBM = GBM_Function(training[, names(training) != target],training[,target])

GBM$bestTune


################ XG Boost ##############################

my_control <-trainControl(method="cv", number=5)

### Hyperparameter grid
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = seq(4,15),
  gamma = c(0.0, 0.2, 1),
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)


### Hyperparameter tuining
xgb_caret <- train(x=training[, names(training) != target], y=training[,target], method='xgbTree', trControl= my_control, tuneGrid=xgb_grid)
 xgb_caret$bestTune

### Train on entire dataset
label_train <- training[,target]
dtrain <- xgb.DMatrix(data=as.matrix(training[, names(training) != target]), label=label_train)
dtrain_cal <- xgb.DMatrix(data=as.matrix(training[, names(training) != target]))
dtest <- xgb.DMatrix(data= as.matrix(testing[, names(testing) != target]))


xgb_model <- xgb.train(data = dtrain,
                       params = xgb_caret$bestTune,
                       watchlist = list(train = dtrain),
                       nrounds = 5000,
                       verbose = 1,
                       print_every_n = 100,
                       seed =1
)


### Feature importance
names <- dimnames(training[, names(training) != target])[[2]]
importance_matrix <- xgb.importance(names,model=xgb_model)

importance_matrix %>% top_n(Gain,n=20) %>%
  ggplot(aes(reorder(Feature,Gain),Gain)) + geom_bar(stat= "identity") +
  xlab("Features") + theme_minimal() + coord_flip() +
  theme(axis.text.x = element_blank()) + ylab("Importance")+
  ggtitle("Feature importance using xgboost")

Pred <- predict(xgb_model, dtest)

### Testing RMSE
errval <- testing$total_daily_calls - Pred
print(paste('XGBoost Test MSE = ', round(mean(abs(errval)) )))
print(paste('XGBoost Test RMSE = ',  round(sqrt(mean(errval^2)))))
print(paste('XGBoost Test MAPE = ', round((mean(abs(testing$total_daily_calls - Pred)/testing$total_daily_calls))*100,2)))


################## Collating all model results into 1 table and reusing the above functions for future prediction

# Create empty df to store model results

nc <- 7 
nr <- 6 
model_df <- data.frame(matrix(NA, ncol = nc, nrow = nr)) 
names(model_df) <- c("model", "Train MAE","Train RMSE","Train MAPE", "Test MAE","Test RMSE","Test MAPE")

#Function that caculates accuracy metrics and appends results to data frame

model_result_func <- function(i,training,testing,target,model,model_name,model_results) {
  #Training RMSE
  suppressWarnings(Pred <- predict(model, training[, names(training) != target]))
  errval <- training[,target] - Pred 
  print(paste(model_name, 'Train MSE = ', round(mean(abs(errval)) )))
  print(paste(model_name, 'Train RMSE = ',  round(sqrt(mean(errval^2)))))
  print(paste(model_name, 'Train MAPE = ', round((mean(abs(training[,target] - Pred)/training[,target]))*100,2)))
  model_results[i,1] <- model_name
  model_results[i,2] <- round(mean(abs(errval)) )
  model_results[i,3] <- round(sqrt(mean(errval^2)))
  model_results[i,4] <- round((mean(abs(training[,target] - Pred)/training[,target]))*100,2)
  
  #Testing RMSE
  suppressWarnings(Pred <- predict(model,  testing[, names(testing) != target]))
  errval <- testing[,target] - Pred 
  print(paste(model_name, 'Test MSE = ', round(mean(abs(errval)) )))
  print(paste(model_name, 'Test RMSE = ',  round(sqrt(mean(errval^2)))))
  print(paste(model_name, 'Test MAPE = ', round((mean(abs(testing[,target]- Pred)/testing[,target]))*100,2)))
  model_results[i,5] <- round(mean(abs(errval)) )
  model_results[i,6] <- round(sqrt(mean(errval^2)))
  model_results[i,7] <- round((mean(abs(testing[,target] - Pred)/testing[,target]))*100,2)
  
  print(model_results)
  
  return(model_results)
}

### example for 1 model

model_df= model_result_func(1,training,testing,target,Linear_Regression,'Linear_Regression',model_df)


############################## Stacking ############################## 

avg_Function = function(i,training,testing,target,model_name,model_results,Linear_Regression , Elastic_Net, SVM,GBM) {
  
  #Training RMSE
  suppressWarnings(results_training <- as.data.frame(predict(Linear_Regression, training[, names(training) != target])))
  colnames(results_training) = "LR"
  results_training$en <- predict(Elastic_Net, training[, names(training) != target])
  results_training$svm <- predict(SVM, training[, names(training) != target])
  results_training$gbm <- predict(GBM, training[, names(training) != target])
  #results_training$rf <- predict(RF, training[, names(training) != target])
  
  Pred <- apply(results_training,1,median,na.rm=TRUE)
  errval <- training[,target] - Pred 
  print(paste(model_name, 'Train MSE = ', round(mean(abs(errval)) )))
  print(paste(model_name, 'Train RMSE = ',  round(sqrt(mean(errval^2)))))
  print(paste(model_name, 'Train MAPE = ', round((mean(abs(training[,target] - Pred)/training[,target]))*100,2)))
  model_results[i,1] <- model_name
  model_results[i,2] <- round(mean(abs(errval)) )
  model_results[i,3] <- round(sqrt(mean(errval^2)))
  model_results[i,4] <- round((mean(abs(training[,target] - Pred)/training[,target]))*100,2)
  
  #Testing RMSE
  suppressWarnings(results_testing <- as.data.frame(predict(Linear_Regression, testing[, names(testing) != target])))
  colnames(results_testing) = "LR"
  results_testing$en <- predict(Elastic_Net, testing[, names(testing) != target])
  results_testing$svm <- predict(SVM, testing[, names(testing) != target])
  results_testing$gbm <- predict(GBM, testing[, names(testing) != target])
  #results_testing$rf <- predict(RF, testing[, names(testing) != target])
  
  Pred <- apply(results_testing,1,median,na.rm=TRUE)
  errval <- testing[,target] - Pred 
  print(paste(model_name, 'Test MSE = ', round(mean(abs(errval)) )))
  print(paste(model_name, 'Test RMSE = ',  round(sqrt(mean(errval^2)))))
  print(paste(model_name, 'Test MAPE = ', round((mean(abs(testing[,target]- Pred)/testing[,target]))*100,2)))
  model_results[i,5] <- round(mean(abs(errval)) )
  model_results[i,6] <- round(sqrt(mean(errval^2)))
  model_results[i,7] <- round((mean(abs(testing[,target] - Pred)/testing[,target]))*100,2)
  
  print(model_results)
  
  return(model_results)
}
model_df= avg_Function(6,training,testing,target,'Stacking',model_df,Linear_Regression , Elastic_Net, SVM,GBM)





