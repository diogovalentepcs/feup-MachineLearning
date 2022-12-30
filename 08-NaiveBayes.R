
dataset <- read.csv("Dataset\\dataset_1-KNN.csv",header = T,sep = ",",dec = ".",stringsAsFactors = F)


predictions_summary<-data.frame()


#install.packages('ROSE')
library(ROSE)
#install.packages("performanceEstimation")
library(performanceEstimation)
#oversampling
  #dataset <- ovun.sample(target ~ ., data = dataset, method = "over", N = 2 * max(table(dataset$target)), seed = 10)$data
#undersampling
  #dataset <- ovun.sample(target ~ ., data = dataset, method = "under", N = 2 * min(table(dataset$target)), seed = 20)$data
#both
  #dataset <- ovun.sample(target ~ ., data = dataset, method = "both", N = nrow(dataset), p=0.5, seed = 30)$data
#rose
  #dataset <- ROSE(target ~., data = dataset, N = nrow(dataset), seed = 40)$data
#smote
  #dataset <- smote(target ~., data = dataset, perc.over = 2, k = 5, perc.under = 1)


num_folds = 10

folds <- createFolds(dataset$target, k=num_folds)

for (k in 1:num_folds){
  
  Model_NaiveBayes<-naiveBayes(target~.-enrollee_id,data=dataset[-folds[[k]],])
  
  prediction <- predict(Model_NaiveBayes, newdata=dataset[folds[[k]],], type='raw', threshold = 0.001)
  
  predictions_summary<-rbind(predictions_summary, data.frame(ID=dataset[folds[[k]],1], target=dataset$target[folds[[k]]], prediction=prediction[,'1']))
}

predictions_summary$target<-as.numeric(as.character(predictions_summary$target))
predictions_summary$prediction<-round(predictions_summary$prediction)

cm = confusionMatrix(factor(predictions_summary$pred),factor(predictions_summary$target))
cm_accuracy = cm$overall[[1]]


