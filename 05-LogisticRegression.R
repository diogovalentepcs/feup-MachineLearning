data <- read.csv("Dataset\\dataset_1-KNN.csv",header = T,sep = ",",dec = ".",stringsAsFactors = F)


num_folds<-10
set.seed(123)
Logistic_data <- chi2data[-c(1,7,9)] 

folds <- createFolds(Logistic_data$target, k=num_folds)
predictions_summary<-data.frame()

for (k in 1:num_folds){
  logistic <- glm(target~.,Logistic_data, family="binomial")
  summary(logistic)
  prediction <- predict(logistic, Logistic_data[folds[[k]],], type="response")
  predictions_summary<-rbind(predictions_summary, data.frame(ID=data[folds[[k]],1],target = Logistic_data$target[folds[[k]]], prediction))
}              
summary(predictions_summary)


predictions_summary$target<-as.numeric(as.character(predictions_summary$target))



confusion_matrix<-cmx(predictions_summary, 0.5)
confusion_matrix

TN =confusion_matrix[1,1]
TP =confusion_matrix[2,2]
FP =confusion_matrix[1,2]
FN =confusion_matrix[2,1]
precision =(TP)/(TP+FP)
recall_score =(TP)/(TP+FN)
f1_score=2*((precision*recall_score)/(precision+recall_score))
precision
recall_score
f1_score

performance_metrics<-presence.absence.accuracy(predictions_summary, threshold = 0.5, find.auc = FALSE,  st.dev = FALSE)
performance_metrics