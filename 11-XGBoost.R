
#### EXTREME GRADIENT BOOSTING 

# General notes:
#Popular in machine learning challenges
#Fast and accurate
#Can handle missing values
#It requires a numeric matrix for its input


# STEP 1: Load required libraries and the dataset.

library(magrittr)
library(dplyr)
library(Matrix)
library(xgboost)
library(gbm)             # for fitting the model
library(caret)           # for general data preparation and model fitting
library(rpart.plot)
library(tidyverse)
library(ROSE)
library(e1071) 
library(dplyr)
library(ggplot2)
library(PresenceAbsence)
library(pROC)
library(data.table)
library(cowplot)
library(StratifiedMedicine)


dataset <- read.csv("2b_normalized.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset = subset(dataset, select = -c(X, enrollee_id,city))
set.seed(210) #sets the starting number used to generate a sequence of random numbers 
predictions_summary_xg <- data.frame()



# STEP 2: Run the model using tunned parameters (below) and K-Fold Cross-Validation

n_folds <- 10

folds <- createFolds(dataset$target, k= n_folds)

for (i  in 1: n_folds){
  train <- dataset[-folds[[i]], ]
  test <- dataset[folds[[i]], ]
  m_test <- as.matrix(subset(test, select =-c(target)))
  
  #m_test <- as.matrix(subset(test, select =-c(target)))
  
  # Considering Oversampling:
  #train <- ovun.sample(target~., data = train, method = "over", N = 2*max(table(train$target)), seed = 10)$data
  
  # Considering Undersampling:
  #train <- ovun.sample(target~., data = train, method = "under", N = 2*min(table(train$target)), seed = 20)$data
  
  # Considering Both over and Undersampling
  #train <- ovun.sample(target~., data = train, method = "both", N = 2*nrow(train), seed = 30)$data
  
  # Considering Random Over Sampling Examples (ROSE)
  # train <- ROSE(target~., data = train, N = 2*nrow(train), seed = 40)$data
  
  
  l_train <- train[,"target"]
  m_train <- as.matrix(subset(train, select =-c(target)))
  
  # XGBoost model
  
  xgb_model <- xgboost(data = m_train, label = l_train, nrounds = 100,eta = 0.1, max_depth = 7, objetive="binary:logistic")
  
  # Predictions 
  
  prediction <- predict(xgb_model, m_test)
  predictions_summary_xg <-rbind(predictions_summary_xg, data.frame(target=test$target, prediction))
  
}
summary(predictions_summary_xg)

predictions_summary_xg$target<-as.numeric(as.character(predictions_summary_xg$target))
predictions_summary_xg$prediction<-as.numeric(as.character(predictions_summary_xg$prediction))

predictions_summary_xg$prediction <- round(predictions_summary_xg$prediction)

# Get the evaluation metrics using Confusion Matrix

cm = confusionMatrix(factor(predictions_summary_xg$target),factor(predictions_summary_xg$prediction), mode = "everything")
cm

gmean <- sqrt(cm$byClass[[1]]*cm$byClass[[2]])

# STEP 3: Parameter tuning using grid search to optimize the model

# Specifying the CV technique which will be passed into the train() function later

train_control <-  trainControl(method = "cv")  # Used first to define the method of cross validation to be carried out 

# Customizing the tuning grid

gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = (1:10)*50,    # number of trees
                        # default values below
                        eta = 0.1,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)


# Training a XGBoost Regression tree model while tuning parameters

train$target <- as.factor (train$target)

model <- train(target~., data = train, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)
print(model)



# STEP 4: EXTRA - Get feature importance

# Give an importance score for each variable, describing the importance of that feature for the prediction. 

importance <- xgb.importance(feature_names = colnames(m_train), model = xgb_model)
head(importance)

aux =  subset(importance, importance$Importance > 0.05)
boxplot(importance$Importance )

importanceRaw <- xgb.importance(feature_names = colnames(m_train), model = xgb_model, data = m_train, label = l_train)

importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceClean)

xgb.plot.importance(importance_matrix = importance)
