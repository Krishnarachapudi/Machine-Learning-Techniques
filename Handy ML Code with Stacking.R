
############################## Variables that need to be initialied before running the modeling code ############################## 

#Data location
setwd("Z:/Final")

#Target variable name
target = 'total_daily_calls' 

#Data Start Dates 
#testDate = '2018-09-01' # Start of test data (6 months test data)
#predDate = '2019-03-01' # Start of prediction time period (6 months to predict)
#testDate = '2018-03-01' # Start of test data (6 months test data)
#predDate = '2018-09-01' # Start of prediction time period (6 months to predict)



############################## Libraries ############################## 

# Install libraries
#install.packages("caret", dependencies = c("Depends", "Suggests")) 
#install.packages('xgboost')
#install.packages('e1071')
#install.packages('dummies')
#install.packages('glmnet')
#install.packages('lubridate')
#install.packages('randomForest')
#install.packages('gbm')
#install.packages('kernlab')

#Load libraries 
library(caret)
library(ggplot2)
library(dplyr)
library(dummies)
library(randomForest) 
library(glmnet)
library(xgboost)
library(e1071)
library(ggplot2)
library(lubridate)
library(gbm)
library(parallel)
library(doParallel)
library(kernlab)

### Parallel

cluster = makeCluster(detectCores() -1 )
registerDoParallel(cluster)


############################## Load Data ############################## 

#Load data
df<- read.csv('overall_dataset.csv')

############################## Prepare dataset ############################## 
# Convert to date to date format
# Automatically calculate test and prediction start dates
# Create dummy variables for categorical variables
# Drop categorical variables
# Split dataset into test, train  and prediction

#Convert to date
df$Date <- as.Date(df$Date_week, format= "%Y-%m-%d")

#Dummy categorical variables
df <- cbind(df, dummy(df$month, sep = "__month"), 
            dummy(df$quarter, sep = "__quarter"),
            dummy(df$year, sep = "__year"),
            dummy(df$week_month, sep = "__week_month"),
           dummy(df$week, sep = "__week")
)

#Remove created dummy variables 
df = df[ , -which(names(df) %in% c("X", "day_week","month","quarter","Date_week" ,'week_month','week',
                                   'day_month','SD_4weeks','SD_same_week','SD_week_month'))]


#Train, Test and prediction dataset dreation
training <- df[( df$split_flag == 'train'),]
testing <- df[(df$split_flag == 'test'  ),]
train_predict <- df[(df$split_flag != 'predict'  ),]
prediction <- df[( df$split_flag == 'predict'),]

# #Train Test dataset dreation
 #training <- df[( df$Date < testDate),]
 #testing <- df[(df$Date >= testDate  &  df$Date < predDate  ),]
 #prediction <- df[( df$Date >= predDate),]

#Remove date column and split flag
training = training [, -which(names(training) %in% c("Date", 'split_flag'))] 
testing = testing [, -which(names(testing) %in% c("Date", 'split_flag'))] 
testing = testing %>% filter(df__month4 != 1)
prediction = prediction [, -which(names(prediction) %in% c("Date", 'split_flag'))] 
train_predict = train_predict [, -which(names(train_predict) %in% c("Date", 'split_flag'))] 

#Remove null values from training dataset
training =  training[complete.cases(training), ]
train_predict =  train_predict[complete.cases(train_predict), ]

############################## Model function ##############################

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

############################## Models ############################## 


############################## Linear Regression ############################## 

# Linear regression is a basic and commonly used type of predictive analysis.  
# The overall idea of regression is to examine two things: 
# (1) does a set of predictor variables do a good job in predicting an outcome (dependent) variable?  
# (2) Which variables in particular are significant predictors of the outcome variable, 
# and in what way do they-indicated by the magnitude and sign of the beta estimates-impact the outcome variable?  
# These regression estimates are used to explain the relationship between one dependent variable and one or more independent variables.

LR_Function = function(X_train , data_model) {
  lm( X_train ~ ., data =data_model  )
}

Linear_Regression = LR_Function(  training[,target],training[, names(training) != target])
summary(Linear_Regression)
model_df= model_result_func(1,training,testing,target,Linear_Regression,'Linear_Regression',model_df)

############################## Elastic Net ############################## 

#Elastic net is a regularized regression method that linearly combines the L1 and L2 penalties of the lasso and ridge methods
#The solution is to combine the penalties of ridge regression and lasso to get the best of both worlds.
#??(alpha) is the mixing parameter between ridge (??=0) and lasso (??=1).
#??(lambda) parameter is the regularization penalty. Setting ?? to 0 is the same as using the OLS, while the larger its value, the stronger is the coefficients' size penalized.

set.seed(10)
my_control <-trainControl(method="cv", number=5, allowParallel = TRUE) 

#ElasticGrid <- expand.grid(alpha = seq(0,1,by = 0.5), lambda = seq(0,20,by = 2) )
ElasticGrid <- expand.grid(alpha = seq(0,1,by = 0.01), lambda = seq(0,40,by = 0.1) )

EN_Function = function(X_train , y_train) {
  suppressWarnings(train(x=X_train, y=y_train, method='glmnet',
                         trControl= my_control, tuneGrid=ElasticGrid , preProcess = c("center", "scale")))
}

Elastic_Net = EN_Function(training[, names(training) != target],training[,target])

Elastic_Net$bestTune
model_df= model_result_func(2,training,testing,target,Elastic_Net,'Elastic_Net',model_df)

############################## SVM ############################## 

polyGrid <- expand.grid(degree = c(1,2),
                        scale =  seq(0.01,1,by = 0.1),
                        C = seq(0.01,1,by = 0.1))

SVM_Function = function(X_train , y_train) {
  suppressWarnings(train(X_train,y_train, method = "svmPoly", tuneLength = 5,
                         trControl = my_control)
  )
}


# SVM_Function = function(X_train , y_train) {
#   suppressWarnings(train(X_train,y_train, method = "svmPoly", tuneGrid=polyGrid ,
# #                         preProcess = c("center", "scale"),
#                          trControl = my_control)
#   )
# }


SVM = SVM_Function(training[, names(training) != target],training[,target])
plot(SVM)
SVM$bestTune
model_df= model_result_func(3,training,testing,target,SVM,'SVM',model_df)

############################## GBM ############################## 

## Parameters in GBM
# n.trees - Boosting Iterations
# interaction.depth - Max Tree Depth  
# n.minobsinnode - Minimum number of observations in the terminal nodes of the trees. This is the actual number of observations, not the total weight.
# shrinkage - srinkage Also known as the learning rate or step-size reduction

# GBMGrid <- expand.grid(interaction.depth = seq(1, 10, by = 1),
#                        n.trees = seq(100, 1000, by = 50),
#                        n.minobsinnode = seq(1, 10, by = 1),
#                        shrinkage = seq(.01, 1, by = .01))



GBMGrid <- expand.grid(interaction.depth = seq(1, 5, by = 1),
                       n.trees = seq(100, 500, by = 100),
                       n.minobsinnode = seq(1, 10, by = 2),
                       shrinkage = seq(.01, 1, by = .1)) 


# Initial run to estimate best tune parameters


GBM_Function = function(X_train , y_train) {
  suppressWarnings(train(x=X_train, y=y_train,
                         method = "gbm",
                         trControl = my_control,        
                         tuneGrid=GBMGrid,verbose=FALSE)
  )
}

GBM = GBM_Function(training[, names(training) != target],training[,target])

GBM$bestTune

model_df= model_result_func(4,training,testing,target,GBM,'GBM',model_df)


# Plot relative feature importance

varImp(GBM)
plot(varImp(GBM))
plot(GBM)



############################## Random Forest ############################## 
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

model_df= model_result_func(5,training,testing,target,RF,'RF',model_df)


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

write.csv(model_df, 'model_df_without_april.csv')


############################## Final Prediction ############################## 

#Get best model name

best_model = model_df[order(model_df$`Test RMSE`),][1,1]

if (best_model=='Linear_Regression') {
  Linear_Regression_predict = LR_Function(  train_predict[,target],train_predict[, names(train_predict) != target])
  Pred_final <- predict(Linear_Regression_predict, prediction)
} else if (best_model=='Elastic_Net') {
  Elastic_Net_predict = EN_Function(train_predict[, names(training) != target],train_predict[,target])
  Pred_final <- predict(Elastic_Net_predict, prediction)
} else if (best_model=='SVM') {
  SVM_predict = SVM_Function(train_predict[, names(training) != target],train_predict[,target])
  Pred_final <- predict(SVM_predict, prediction)
} else if (best_model=='GBM') {
  GBM_predict = GBM_Function(train_predict[, names(training) != target],train_predict[,target])
  Pred_final <- predict(GBM_predict, prediction)
} else if (best_model=='RF') { 
  RF_predict = RF_Function(train_predict[, names(train_predict) != target],train_predict[,target])
  Pred_final <- predict(RF_predict, prediction)
} else { 
  #get individual models
  Linear_Regression = LR_Function(  train_predict[,target],train_predict[, names(training) != target])
  Elastic_Net = EN_Function(train_predict[, names(training) != target],train_predict[,target])
  SVM = SVM_Function(train_predict[, names(training) != target],train_predict[,target])
  GBM = GBM_Function(train_predict[, names(training) != target],train_predict[,target])
  
  #individual results
  results <- as.data.frame(predict(Linear_Regression, prediction[, names(prediction) != target]))
  colnames(results) = "LR"
  results$en <- predict(Elastic_Net, prediction[, names(prediction) != target])
  results$svm <- predict(SVM, prediction[, names(prediction) != target])
  results$gbm <- predict(GBM, prediction[, names(prediction) != target])
  
  Pred_final <- apply(results,1,mean,na.rm=TRUE)
}


#output predicted dataset
prediction$pred = Pred_final
prediction$date =as.Date( df[(  df$split_flag == 'predict'),]$Date)
write.csv(prediction, 'final_prediction_results.csv')

# End parallel processing
stopCluster(cluster)

registerDoSEQ()

############################## Plot Results ############################## 

Pred <- predict(GBM, testing)
testing$pred = Pred
testing$date = as.Date(df[(df$split_flag == 'test'),]$Date)

write.csv(testing,'testingresults.csv')

g <- ggplot(testing, aes(date)) + 
  geom_line(aes(y = total_daily_calls), color ='#0AA147', size = 1.25) +
  geom_line(aes(y = pred) , color ='#95C93D' ,  size = 1.25)+
  geom_point(aes(y = total_daily_calls), color ='#0AA147', size = 2.5) +
  geom_point(aes(y = pred) , color ='#95C93D' ,  size = 2.5) +
  labs(y = "Total Weekly Calls", x = "Date") +
  ggtitle("Weekly Call Volume Forecast September 2018 - Feb 2019") +
  scale_x_date(date_labels = "%b-%y",date_breaks='1 month') + scale_y_continuous(limits = c(0, 5000))+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.title=element_text(size=12, face="bold"), 
        legend.text=element_text(size=10, face="bold"),
        axis.text=element_text(size=15, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        legend.key.size = unit(2,"line")) +
  scale_color_discrete(name = "Y", labels = c("Y2", "Y1"))


g

# 
# ############################## XG Boost ##############################
# 
# my_control <-trainControl(method="cv", number=5)
# 
# #Hyperparameter grid
# xgb_grid = expand.grid(
#   nrounds = 1000,
#   eta = c(0.1, 0.05, 0.01),
#   max_depth = seq(4,15),
#   gamma = c(0.0, 0.2, 1),
#   colsample_bytree=1,
#   min_child_weight=c(1, 2, 3, 4 ,5),
#   subsample=1
# )
# 
# xgb_grid = expand.grid(
#   nrounds = 1000,
#   eta =1,
#   max_depth = 1,
#   gamma = 1,
#   colsample_bytree=1,
#   min_child_weight=1,
#   subsample=1
# )
# 
# #Hyperparameter tuining
# xgb_caret <- train(x=training[, names(training) != target], y=training[,target], method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
# xgb_caret$bestTune
# 
# #Train on entire dataset
# label_train <- training[,target]
# dtrain <- xgb.DMatrix(data=as.matrix(training[, names(training) != target]), label=label_train)
# dtrain_cal <- xgb.DMatrix(data=as.matrix(training[, names(training) != target]))
# dtest <- xgb.DMatrix(data= as.matrix(testing[, names(testing) != target]))
# 
# 
# xgb_model <- xgb.train(data = dtrain,
#                        params = xgb_caret$bestTune,
#                        watchlist = list(train = dtrain),
#                        nrounds = 5000,
#                        verbose = 1,
#                        print_every_n = 100,
#                        seed =1
# )
# 
# 
# #Feature importance
# names <- dimnames(training[, names(training) != target])[[2]]
# importance_matrix <- xgb.importance(names,model=xgb_model)
# 
# importance_matrix %>% top_n(Gain,n=20) %>%
#   ggplot(aes(reorder(Feature,Gain),Gain)) + geom_bar(stat= "identity") + 
#   xlab("Features") + theme_minimal() + coord_flip() +
#   theme(axis.text.x = element_blank()) + ylab("Importance")+
#   ggtitle("Feature importance using xgboost")
# 
# 
# 
# Pred <- predict(xgb_model, dtest)
# 
# #Testing RMSE
# errval <- testing$total_daily_calls - Pred 
# print(paste('XGBoost Test MSE = ', round(mean(abs(errval)) )))
# print(paste('XGBoost Test RMSE = ',  round(sqrt(mean(errval^2)))))
# print(paste('XGBoost Test MAPE = ', round((mean(abs(testing$total_daily_calls - Pred)/testing$total_daily_calls))*100,2)))
# 
# i=3
# model_df[i,1] <-'XGBoost'
# model_df[i,2] <- round(mean(abs(errval)) )
# model_df[i,3] <- round(sqrt(mean(errval^2)))
# model_df[i,4] <- round((mean(abs(testing$total_daily_calls - Pred)/testing$total_daily_calls))*100,2)
# Pred <- predict(xgb_model, dtrain_cal)
# 
# model_df= model_result_func(3,dtrain_cal,dtest, target,xgb_model,'XGBoost',model_df)