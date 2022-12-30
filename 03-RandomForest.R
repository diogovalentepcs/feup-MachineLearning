
#Import Datasets
dataset_4NA_NOEXP_PRI_KNN <- read.csv(file = "Dataset\\dataset_4NA_EXP_PRI_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_4NA_NOEXP_PRI_MICE <- read.csv(file = "Dataset\\dataset_4NA_EXP_PRI_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_4NA_NOEXP_POST_KNN <- read.csv(file = "Dataset\\dataset_4NA_NOEXP_POST_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_4NA_NOEXP_POST_MICE <- read.csv(file = "Dataset\\dataset_4NA_NOEXP_POST_MICE.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_4NA_EXP_PRI_KNN <- read.csv(file = "Dataset\\dataset_4NA_EXP_PRI_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_4NA_EXP_PRI_MICE <- read.csv(file = "Dataset\\dataset_4NA_EXP_PRI_MICE.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_4NA_EXP_POST_KNN <- read.csv(file = "Dataset\\dataset_4NA_EXP_POST_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_4NA_EXP_POST_MICE <- read.csv(file = "Dataset\\dataset_4NA_EXP_POST_MICE.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_5NA_NOEXP_PRI_KNN <- read.csv(file = "Dataset\\dataset_5NA_NOEXP_PRI_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_5NA_NOEXP_PRI_MICE <- read.csv(file = "Dataset\\dataset_5NA_NOEXP_PRI_MICE.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_5NA_NOEXP_POST_KNN <- read.csv(file = "Dataset\\dataset_5NA_NOEXP_POST_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_5NA_NOEXP_POST_MICE <- read.csv(file = "Dataset\\dataset_5NA_NOEXP_POST_MICE.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_5NA_EXP_PRI_KNN <- read.csv(file = "Dataset\\dataset_5NA_EXP_PRI_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_5NA_EXP_PRI_MICE <- read.csv(file = "Dataset\\dataset_5NA_EXP_PRI_MICE.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_5NA_EXP_POST_KNN <- read.csv(file = "Dataset\\dataset_5NA_EXP_POST_KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset_5NA_EXP_POST_MICE <- read.csv(file = "Dataset\\dataset_5NA_EXP_POST_MICE.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))


######################Classification######################


num_folds<-10

#dataset_4NA_EXP_PRI_KNN$target <- as.factor(dataset_4NA_EXP_PRI_KNN$target)
#dataset_4NA_EXP_PRI_KNN$target <- as.numeric(dataset_4NA_EXP_PRI_KNN$target)
set.seed(200)


#######Random Forest#######

#Data partition for one of the datasets:
set.seed(200)
data_type <- partition(dataset_4NA_EXP_PRI_KNN$target, p = c(train = 0.7, test = 0.3)) 

train <- dataset_4NA_EXP_PRI_KNN[data_type$train , ]
test  <- dataset_4NA_EXP_PRI_KNN[data_type$test , ]

Model_RF <- randomForest(target ~ ., data = train, ntree = 500, mtry = 6, importance = TRUE)

prediction <- predict(Model_RF, test, type="response")

summary(prediction)
print(Model_RF)

prediction$target<-as.numeric(as.character(prediction$target))

confusionMatrix <- cmx (prediction, 0.5)
confusionMatrix


######Random Forest com K-Folds######

dataset_4NA_EXP_PRI_KNN$city <- as.factor(as.character(as.numeric(dataset_4NA_EXP_PRI_KNN$city)))
dataset_4NA_EXP_PRI_KNN$gender <- as.factor(dataset_4NA_EXP_PRI_KNN$gender)
dataset_4NA_EXP_PRI_KNN$relevent_experience <- as.factor(dataset_4NA_EXP_PRI_KNN$relevent_experience)
dataset_4NA_EXP_PRI_KNN$enrolled_university <- as.factor(dataset_4NA_EXP_PRI_KNN$enrolled_university)
dataset_4NA_EXP_PRI_KNN$education_level <- as.factor(dataset_4NA_EXP_PRI_KNN$education_level)
dataset_4NA_EXP_PRI_KNN$major_discipline <- as.factor(dataset_4NA_EXP_PRI_KNN$major_discipline)
dataset_4NA_EXP_PRI_KNN$experience <- as.factor(dataset_4NA_EXP_PRI_KNN$experience)
dataset_4NA_EXP_PRI_KNN$company_size <- as.factor(dataset_4NA_EXP_PRI_KNN$company_size)
dataset_4NA_EXP_PRI_KNN$company_type <- as.factor(dataset_4NA_EXP_PRI_KNN$company_type)
dataset_4NA_EXP_PRI_KNN$last_new_job <- as.factor(dataset_4NA_EXP_PRI_KNN$last_new_job)
dataset_4NA_EXP_PRI_KNN$training_hours <- as.numeric(dataset_4NA_EXP_PRI_KNN$training_hours)


folds1 <- createFolds(dataset_4NA_EXP_PRI_KNN$target, k=num_folds)
dataset_4NA_EXP_PRI_KNN$target <- as.factor(dataset_4NA_EXP_PRI_KNN$target)
set.seed(200)
predictions_summary_normal <- data.frame()
predictions_summary_under <- data.frame()
predictions_summary_over <- data.frame()
predictions_summary_both <- data.frame()
predictions_summary_ROSE <- data.frame()
predictions_summary_SMOTE <- data.frame()

#Data partition for one of the datasets:
#data_type <- partition(dataset_4NA_EXP_PRI_KNN$target, p = c(train = 0.7, test = 0.3)) 

#train <- dataset_4NA_EXP_PRI_KNN[data_type$train , ]
#test  <- dataset_4NA_EXP_PRI_KNN[data_type$test , ]

#Parameter Tuning 
#?tuneRF
bestmtry <- tuneRF(x = dataset_4NA_EXP_PRI_KNN[,-c(1, 2, 13, 14)],y = dataset_4NA_EXP_PRI_KNN[,14], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)

#selecionar melhor mtry após visualizar o obtido com o bestmtry

for (k in 1:num_folds) {
  
  train <- dataset_4NA_EXP_PRI_KNN[-folds1[[k]],]
  test <- dataset_4NA_EXP_PRI_KNN[folds1[[k]],]

  
  train_under <- ovun.sample(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train, method = "under", N = 2*min(table(train$target)), seed = 10)$data

  train_over <- ovun.sample(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train, method = "over", N = 2*max(table(train$target)), seed = 20)$data

  train_both <- ovun.sample(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train, method = "both", N = nrow(train), p = 0.5, seed = 30)$data

  #Synthetic Data Generation (ROSE)
  train_ROSE <- ROSE(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train, N = nrow(train), seed = 40)$data

  #SMOTE
  train_SMOTE <- smote(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train, perc.over = 2, k = 5, perc.under = 1)

  
  Model_RF_normal <- randomForest(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train, ntree = 500, mtry = 3, importance = TRUE)
  Model_RF_under <- randomForest(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train_under, ntree = 500, mtry = 3, importance = TRUE)
  Model_RF_over <- randomForest(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train_over, ntree = 500, mtry = 3, importance = TRUE)
  Model_RF_both <- randomForest(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train_both, ntree = 500, mtry = 3, importance = TRUE)
  Model_RF_ROSE <- randomForest(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train_ROSE, ntree = 500, mtry = 3, importance = TRUE)
  Model_RF_SMOTE <- randomForest(target ~ city_development_index+gender+relevent_experience+enrolled_university+education_level+major_discipline+experience+company_size+company_type+last_new_job, data = train_SMOTE, ntree = 500, mtry = 3, importance = TRUE)
  
  
  
  prediction_normal <- predict(Model_RF_normal, test, type = "response")
  prediction_under <- predict(Model_RF_under, test, type="response")
  prediction_over <- predict(Model_RF_over, test, type="response")
  prediction_both <- predict(Model_RF_both, test, type="response")
  prediction_ROSE <- predict(Model_RF_ROSE, test, type="response")
  prediction_SMOTE <- predict(Model_RF_SMOTE, test, type="response")
  
  predictions_summary_normal <- rbind(predictions_summary_normal, data.frame(id=dataset_4NA_EXP_PRI_KNN[folds1[[k]],1], class=dataset_4NA_EXP_PRI_KNN$target[folds1[[k]]], prediction_normal))
  predictions_summary_under <- rbind(predictions_summary_under, data.frame(id=dataset_4NA_EXP_PRI_KNN[folds1[[k]],1], class=dataset_4NA_EXP_PRI_KNN$target[folds1[[k]]], prediction_under))
  predictions_summary_over <- rbind(predictions_summary_over, data.frame(id=dataset_4NA_EXP_PRI_KNN[folds1[[k]],1], class=dataset_4NA_EXP_PRI_KNN$target[folds1[[k]]], prediction_over))
  predictions_summary_both <- rbind(predictions_summary_both, data.frame(id=dataset_4NA_EXP_PRI_KNN[folds1[[k]],1], class=dataset_4NA_EXP_PRI_KNN$target[folds1[[k]]], prediction_both))
  predictions_summary_ROSE <- rbind(predictions_summary_ROSE, data.frame(id=dataset_4NA_EXP_PRI_KNN[folds1[[k]],1], class=dataset_4NA_EXP_PRI_KNN$target[folds1[[k]]], prediction_ROSE))
  predictions_summary_SMOTE <- rbind(predictions_summary_SMOTE, data.frame(id=dataset_4NA_EXP_PRI_KNN[folds1[[k]],1], class=dataset_4NA_EXP_PRI_KNN$target[folds1[[k]]], prediction_SMOTE))
  
}
#?randomForest
#length(prediction_normal)
#length(dataset_4NA_EXP_PRI_KNN[folds1[[k]],1])



summary (predictions_summary_normal)
summary(predictions_summary_under)
summary(predictions_summary_over)
summary(predictions_summary_both)
summary(predictions_summary_ROSE)
summary(predictions_summary_SMOTE)


###Evaluation Metrics
#Normal
predictions_summary_normal$class<-as.factor(predictions_summary_normal$class)
predictions_summary_normal$prediction_normal<-as.factor(predictions_summary_normal$prediction_normal)

xtab_1 <- table(predictions_summary_normal$prediction_normal, predictions_summary_normal$class)
confusion_matrix_1<- confusionMatrix(xtab_1)
confusion_matrix_1

f1_1 = confusion_matrix_1$byClass[[7]]
precision_1 = confusion_matrix_1$byClass[[5]]
gmean_1 = sqrt(confusion_matrix_1$byClass[[1]]*confusion_matrix_1$byClass[[2]])


#Under
predictions_summary_under$class<-as.factor(predictions_summary_under$class)
predictions_summary_under$prediction_under<-as.factor(predictions_summary_under$prediction_under)

xtab_2 <- table(predictions_summary_under$prediction_under, predictions_summary_under$class)
confusion_matrix_2<- confusionMatrix(xtab_2)
confusion_matrix_2

f1_2 = confusion_matrix_2$byClass[[7]]
precision_2 = confusion_matrix_2$byClass[[5]]
gmean_2 = sqrt(confusion_matrix_2$byClass[[1]]*confusion_matrix_2$byClass[[2]])


#Over
predictions_summary_over$class<-as.factor(predictions_summary_over$class)
predictions_summary_over$prediction_over<-as.factor(predictions_summary_over$prediction_over)

xtab_3 <- table(predictions_summary_over$prediction_over, predictions_summary_over$class)
confusion_matrix_3<- confusionMatrix(xtab_3)
confusion_matrix_3

f1_3 = confusion_matrix_3$byClass[[7]]
precision_3 = confusion_matrix_3$byClass[[5]]
gmean_3 = sqrt(confusion_matrix_3$byClass[[1]]*confusion_matrix_3$byClass[[2]])


#Both
predictions_summary_both$class<-as.factor(predictions_summary_both$class)
predictions_summary_both$prediction_both<-as.factor(predictions_summary_both$prediction_both)

xtab_4 <- table(predictions_summary_both$prediction_both, predictions_summary_both$class)
confusion_matrix_4<- confusionMatrix(xtab_4)
confusion_matrix_4

f1_4 = confusion_matrix_4$byClass[[7]]
precision_4 = confusion_matrix_4$byClass[[5]]
gmean_4 = sqrt(confusion_matrix_4$byClass[[1]]*confusion_matrix_4$byClass[[2]])


#ROSE
predictions_summary_ROSE$class<-as.factor(predictions_summary_ROSE$class)
predictions_summary_ROSE$prediction_ROSE<-as.factor(predictions_summary_ROSE$prediction_ROSE)

xtab_5 <- table(predictions_summary_ROSE$prediction_ROSE, predictions_summary_ROSE$class)
confusion_matrix_5<- confusionMatrix(xtab_5)
confusion_matrix_5

f1_5 = confusion_matrix_5$byClass[[7]]
precision_5 = confusion_matrix_5$byClass[[5]]
gmean_5 = sqrt(confusion_matrix_5$byClass[[1]]*confusion_matrix_5$byClass[[2]])


#SMOTE
predictions_summary_SMOTE$class<-as.factor(predictions_summary_SMOTE$class)
predictions_summary_SMOTE$prediction_SMOTE<-as.factor(predictions_summary_SMOTE$prediction_SMOTE)

xtab_6 <- table(predictions_summary_SMOTE$prediction_SMOTE, predictions_summary_SMOTE$class)
confusion_matrix_6<- confusionMatrix(xtab_6)
confusion_matrix_6

f1_6 = confusion_matrix_6$byClass[[7]]
precision_6 = confusion_matrix_6$byClass[[5]]
gmean_6 = sqrt(confusion_matrix_6$byClass[[1]]*confusion_matrix_6$byClass[[2]])





performance_metrics<-presence.absence.accuracy(predictions_summary_normal, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
performance_metrics



roc_curve<-plot.roc(predictions_summary$target, predictions_summary$prediction, legacy.axes= T, direction="<")
roc_curve


auc_list<-auc(predictions_summary_over$class, predictions_summary_over$prediction,  direction="<")
auc_list



TN =confusion_matrix[1,1]
TP =confusion_matrix[2,2]
FP =confusion_matrix[1,2]
FN =confusion_matrix[2,1]
precision =(TP)/(TP+FP)
recall_score =(TP)/(TP+FN)

f1_score=2*((precision*recall_score)/(precision+recall_score))
accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
False_positive_rate =(FP)/(FP+TN)
False_negative_rate =(FN)/(FN+TP)

print(paste("Precision value of the model: ",round(precision,2)))
print(paste("Accuracy of the model: ",round(accuracy_model,4)))
print(paste("Recall value of the model: ",round(recall_score,2)))
print(paste("False Positive rate of the model: ",round(False_positive_rate,2)))
print(paste("False Negative rate of the model: ",round(False_negative_rate,2)))
print(paste("f1 score of the model: ",round(f1_score,2)))



predictions_summary_normal$class<-as.factor(predictions_summary_normal$class)
predictions_summary_normal$prediction_normal<-as.factor(predictions_summary_normal$prediction_normal)

xtab <- table(predictions_summary_normal$prediction_normal, predictions_summary_normal$class)
confusion_matrix<- confusionMatrix(xtab)
confusion_matrix

f1 = confusion_matrix$byClass[[7]]
precision = confusion_matrix$byClass[[5]]
gmean = sqrt(confusion_matrix$byClass[[1]]*confusion_matrix$byClass[[2]])
