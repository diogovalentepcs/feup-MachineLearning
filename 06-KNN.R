

dataset <- read.csv("Dataset\\dataset_1-KNN.csv",header = T,sep = ",",dec = ".",stringsAsFactors = F)



partitions<- partition(dataset$target, p=c(train_validation=0.8, test=0.2))

test_sample<-data.frame()
train_validation_sample<-data.frame()

train_validation_sample <- dataset[partitions$train_validation,]
test_sample<-dataset[partitions$test,]
#-c(4,7,10,14,16,18,22,24,26,28)

library(ROSE)

#OVERSAMPLING
over <- ovun.sample(target~., data = train_validation_sample, method = "over", N = 2*max(table(train_validation_sample$target)))$data

#UNDERSAMPLING
under <- ovun.sample(target~., data = train_validation_sample, method = "under", N = 6718)$data

#BOTH OVER AND UNDERSAMPLING
both <- ovun.sample(target~., data = train_validation_sample, method = "both", p = 0.5, seed = 222, N = 13425)$data

#Synthetic Data Generation (ROSE)
rose <- ROSE(target~., data = train_validation_sample, N = 5000, seed = 111)$data

#SMOTE
#dataset$city_development_index<-as.factor(dataset$city_development_index)
#dataset$experience<-as.factor(dataset$experience)
#dataset$company_size<-as.factor(dataset$company_size)
#dataset$last_new_job<-as.factor(dataset$last_new_job)
#smote <- smote(target ~ city_development_index+experience+company_size+last_new_job, data = train_validation_sample, perc.over = 2, k = 5, perc.under = 1)


performance_summary<-data.frame()

num_folds = 10
max_n_neighbours = 50

set.seed(123)

folds<-createFolds(train_validation_sample$target, k=num_folds)
folds_over <- createFolds(over$target, k=num_folds)
folds_under <- createFolds(under$target, k=num_folds)
folds_both <- createFolds(both$target, k=num_folds)
folds_rose <- createFolds(rose$target, k=num_folds)
#folds_smote <- createFolds(smote$target, k=num_folds)

library(PresenceAbsence)

for (j in 1:max_n_neighbours){
  predictions_summary<-data.frame()
  predictions_summary_over<-data.frame()
  predictions_summary_under<-data.frame()
  predictions_summary_both<-data.frame()
  predictions_summary_rose<-data.frame()
  #predictions_summary_smote<-data.frame()
  
  for (k in 1:num_folds){
    
    Model_knn<-knn(train_validation_sample[-folds[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,29)],train_validation_sample[folds[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,19)],train_validation_sample[-folds[[k]],29],k=j, prob=TRUE)
    Model_knn_over<-knn(over[-folds_over[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,29)],over[folds_over[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,19)],over[-folds_over[[k]],29],k=j, prob=TRUE)
    Model_knn_under<-knn(under[-folds_under[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,19)],under[folds_under[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,19)],under[-folds_under[[k]],29],k=j, prob=TRUE)
    Model_knn_both<-knn(both[-folds_both[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,19)],both[folds_both[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,19)],both[-folds_both[[k]],29],k=j, prob=TRUE)
    Model_knn_rose<-knn(rose[-folds_rose[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,19)],rose[folds_rose[[k]],-c(1,4,7,10,14,16,18,22,24,26,28,19)],rose[-folds_rose[[k]],29],k=j, prob=TRUE)
    #Model_knn_smote<-knn(smote[-folds_smote[[k+1]],-c(1,8)],smote[folds_smote[[k+1]],-c(1,8)],smote[-folds_smote[[k+1]],8],k=j, prob=TRUE)
    
    prob_predictions=ifelse(Model_knn==1,attr(Model_knn, 'prob'),1-attr(Model_knn, 'prob'))
    prob_predictions_over=ifelse(Model_knn_over==1,attr(Model_knn_over, 'prob'),1-attr(Model_knn_over, 'prob'))
    prob_predictions_under=ifelse(Model_knn_under==1,attr(Model_knn_under, 'prob'),1-attr(Model_knn_under, 'prob'))
    prob_predictions_both=ifelse(Model_knn_both==1,attr(Model_knn_both, 'prob'),1-attr(Model_knn_both, 'prob'))
    prob_predictions_rose=ifelse(Model_knn_rose==1,attr(Model_knn_rose, 'prob'),1-attr(Model_knn_rose, 'prob'))
    #prob_predictions_smote=ifelse(Model_knn_smote==1,attr(Model_knn_smote, 'prob'),1-attr(Model_knn_smote, 'prob'))
    
    predictions_summary<-rbind(predictions_summary, data.frame(ID=train_validation_sample[folds[[k]],1], target=train_validation_sample$target[folds[[k]]], pred=prob_predictions))    
    predictions_summary_over<-rbind(predictions_summary_over, data.frame(ID=over[folds_over[[k]],1], target=over$target[folds_over[[k]]], pred=prob_predictions_over))
    predictions_summary_under<-rbind(predictions_summary_under, data.frame(ID=under[folds_under[[k]],1], target=under$target[folds_under[[k]]], pred=prob_predictions_under))
    predictions_summary_both<-rbind(predictions_summary_both, data.frame(ID=both[folds_both[[k]],1], target=both$target[folds_both[[k]]], pred=prob_predictions_both))
    predictions_summary_rose<-rbind(predictions_summary_rose, data.frame(ID=rose[folds_rose[[k]],1], target=rose$target[folds_rose[[k]]], pred=prob_predictions_rose))
    #predictions_summary_smote<-rbind(predictions_summary_smote, data.frame(ID=smote[folds_smote[[k]],1], target=smote$target[folds_smote[[k]]], pred=prob_predictions_smote))
    }
  
  predictions_summary$target<-as.numeric(as.character(predictions_summary$target))
  predictions_summary_over$target<-as.numeric(as.character(predictions_summary_over$target))
  predictions_summary_under$target<-as.numeric(as.character(predictions_summary_under$target))
  predictions_summary_both$target<-as.numeric(as.character(predictions_summary_both$target))
  predictions_summary_rose$target<-as.numeric(as.character(predictions_summary_rose$target))
  #predictions_summary_smote$target<-as.numeric(as.character(predictions_summary_rose$target))
  
  performance_metrics<-presence.absence.accuracy(predictions_summary, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
  performance_metrics_over<-presence.absence.accuracy(predictions_summary_over, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
  performance_metrics_under<-presence.absence.accuracy(predictions_summary_under, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
  performance_metrics_both<-presence.absence.accuracy(predictions_summary_both, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
  performance_metrics_rose<-presence.absence.accuracy(predictions_summary_rose, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
  #performance_metrics_smote<-presence.absence.accuracy(predictions_summary_smote, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
  
  performance_summary<-rbind(performance_summary, data.frame(num_neighbors = j, accuracy=performance_metrics$PCC, accuracy_over = performance_metrics_over$PCC, accuracy_under = performance_metrics_under$PCC, accuracy_both = performance_metrics_both$PCC, accuracy_rose = performance_metrics_rose$PCC, aux_sum = (performance_metrics$PCC+performance_metrics_over$PCC+performance_metrics_under$PCC+performance_metrics_both$PCC+performance_metrics_rose$PCC)))

  }

num_neighbors<-performance_summary[performance_summary$aux_sum==max(performance_summary$aux_sum),"num_neighbors"]

Final_Model_knn<-knn(train_validation_sample[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], test_sample[,-cc(1,4,7,10,14,16,18,22,24,26,28,19)], train_validation_sample[,29], num_neighbors[1], prob=TRUE)
Final_Model_knn_over<-knn(over[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], test_sample[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], over[,29], num_neighbors[1], prob=TRUE)
Final_Model_knn_under<-knn(under[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], test_sample[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], under[,29], num_neighbors[1], prob=TRUE)
Final_Model_knn_both<-knn(both[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], test_sample[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], both[,29], num_neighbors[1], prob=TRUE)
Final_Model_knn_rose<-knn(rose[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], test_sample[,-c(1,4,7,10,14,16,18,22,24,26,28,19)], rose[,29], num_neighbors[1], prob=TRUE)
#Final_Model_knn_smote<-knn(smote[,-c(1,8)], test_sample[,-c(1,8)], smote[,8], num_neighbors[1], prob=TRUE)

prob_predictions=ifelse(Final_Model_knn==1,attr(Final_Model_knn, 'prob'),1-attr(Final_Model_knn, 'prob'))
prob_predictions_over=ifelse(Final_Model_knn_over==1,attr(Final_Model_knn_over, 'prob'),1-attr(Final_Model_knn_over, 'prob'))
prob_predictions_under=ifelse(Final_Model_knn_under==1,attr(Final_Model_knn_under, 'prob'),1-attr(Final_Model_knn_under, 'prob'))
prob_predictions_both=ifelse(Final_Model_knn_both==1,attr(Final_Model_knn_both, 'prob'),1-attr(Final_Model_knn_both, 'prob'))
prob_predictions_rose=ifelse(Final_Model_knn_rose==1,attr(Final_Model_knn_rose, 'prob'),1-attr(Final_Model_knn_rose, 'prob'))
#prob_predictions_smote=ifelse(Final_Model_knn_smote==1,attr(Final_Model_knn_smote, 'prob'),1-attr(Final_Model_knn_smote, 'prob'))

predictions_summary<-data.frame(ID=test_sample[,1], target=test_sample$target, pred=prob_predictions)
predictions_summary_over<-data.frame(ID=test_sample[,1], target=test_sample$target, pred=prob_predictions_over)
predictions_summary_under<-data.frame(ID=test_sample[,1], target=test_sample$target, pred=prob_predictions_under)
predictions_summary_both<-data.frame(ID=test_sample[,1], target=test_sample$target, pred=prob_predictions_both)
predictions_summary_rose<-data.frame(ID=test_sample[,1], target=test_sample$target, pred=prob_predictions_rose)
#predictions_summary_smote<-data.frame(ID=test_sample[,1], target=test_sample$target, pred=prob_predictions_smote)

predictions_summary$target<-as.numeric(as.character(predictions_summary$target))
predictions_summary_over$target<-as.numeric(as.character(predictions_summary_over$target))
predictions_summary_under$target<-as.numeric(as.character(predictions_summary_under$target))
predictions_summary_both$target<-as.numeric(as.character(predictions_summary_both$target))
predictions_summary_rose$target<-as.numeric(as.character(predictions_summary_rose$target))
#predictions_summary_smote$target<-as.numeric(as.character(predictions_summary_smote$target))


#accuracy
#install.packages('PresenceAbsence')
library(PresenceAbsence)

performance_metrics<-presence.absence.accuracy(predictions_summary, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
performance_metrics_over<-presence.absence.accuracy(predictions_summary_over, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
performance_metrics_under<-presence.absence.accuracy(predictions_summary_under, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
performance_metrics_both<-presence.absence.accuracy(predictions_summary_both, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
performance_metrics_rose<-presence.absence.accuracy(predictions_summary_rose, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
#performance_metrics_smote<-presence.absence.accuracy(predictions_summary_smote, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)

accuracy_summary<-data.frame()
accuracy_summary<-rbind(accuracy_summary, data.frame(dataset=nrow(accuracy_summary)+1, normal=performance_metrics$PCC,over=performance_metrics_over$PCC, under =performance_metrics_under$PCC, both=performance_metrics_both$PCC, rose = performance_metrics_rose$PCC))

library(pROC)

#confusion matrix
confusion_matrix<-cmx(predictions_summary)
confusion_matrix_over<-cmx(predictions_summary_over)
confusion_matrix_under<-cmx(predictions_summary_over)
confusion_matrix_both<-cmx(predictions_summary_over)
confusion_matrix_rose<-cmx(predictions_summary_over)
#confusion_matrix_smote<-cmx(predictions_summary_over)
confusion_matrix
confusion_matrix_over
confusion_matrix_under
confusion_matrix_both
confusion_matrix_rose
#confusion_matrix_smote




#verificar se não é exatamente a mesma coisa
#auc_list<-auc(predictions_summary$target, predictions_summary$prediction,  direction="<")
#auc_list

install.packages('MLmetrics')

#f1 score
TN =confusion_matrix[1,1]
TP =confusion_matrix[2,2]
FP =confusion_matrix[1,2]
FN =confusion_matrix[2,1]
precision =(TP)/(TP+FP)
recall_score =(TP)/(TP+FN)

f1_score=2*((precision*recall_score)/(precision+recall_score))


TN_over =confusion_matrix_over[1,1]
TP_over =confusion_matrix_over[2,2]
FP_over =confusion_matrix_over[1,2]
FN_over =confusion_matrix_over[2,1]
precision_over =(TP_over)/(TP_over+FP_over)
recall_score_over =(TP_over)/(TP_over+FN_over)
  
f1_score_over=2*((precision_over*recall_score_over)/(precision_over+recall_score_over))

TN_under =confusion_matrix_under[1,1]
TP_under =confusion_matrix_under[2,2]
FP_under =confusion_matrix_under[1,2]
FN_under =confusion_matrix_under[2,1]
precision_under =(TP_under)/(TP_under+FP_under)
recall_score_under =(TP_under)/(TP_under+FN_under)

f1_score_under=2*((precision_under*recall_score_under)/(precision_under+recall_score_under))


TN_both =confusion_matrix_both[1,1]
TP_both =confusion_matrix_both[2,2]
FP_both =confusion_matrix_both[1,2]
FN_both =confusion_matrix_both[2,1]
precision_both =(TP_both)/(TP_both+FP_both)
recall_score_both =(TP_both)/(TP_both+FN_both)

f1_score_both=2*((precision_both*recall_score_both)/(precision_both+recall_score_both))


TN_rose =confusion_matrix_rose[1,1]
TP_rose =confusion_matrix_rose[2,2]
FP_rose =confusion_matrix_rose[1,2]
FN_rose =confusion_matrix_rose[2,1]
precision_rose =(TP_rose)/(TP_rose+FP_rose)
recall_score_rose =(TP_rose)/(TP_rose+FN_rose)

f1_score_rose=2*((precision_rose*recall_score_rose)/(precision_rose+recall_score_rose))


