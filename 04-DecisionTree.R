



dataset <- read.csv("Dataset\\dataset_1-KNN.csv",header = T,sep = ",",dec = ".",stringsAsFactors = F)






partitions<- partition(dataset$target, p=c(train_validation=0.8, test=0.2))

test_sample<-data.frame()
df1<-data.frame()

df1 <- dataset[partitions$train_validation,]
test_sample<-dataset[partitions$test,]

#####################DECISION TREE#################


#Preparation
paramater_datat <- expand.grid(
  criteria= c('information','gini'), 
  cp=c(0.01,0),
  min_num_obs= seq(1, 30, by = 1),
  accuracy=NA)

num_flods<-10
folds<-createFolds(df1$target,num_flods)

predictions_summary<-data.frame()
#Algoritmo

for (i in 1:nrow(paramater_datat)) {
  predictions_summary=NULL
  
  for (k in 1:num_flods) {
  
    data_test<-df1[folds[[k]],]
    data_train<-df1[-folds[[k]],]
    model_tree<-rpart(target~.-enrollee_id,data=data_train,method = "class",parms=list(split=paramater_datat$criteria[i]),minsplit=(paramater_datat$min_num_obs[i]+1),minbucket=paramater_datat$min_num_obs[i],cp=paramater_datat$cp[i]) 
    #minisplit - the minimum number of observations that must exist in a node in order for a split to be attempted
    #minibucket - the minimum number of observations in any terminal node
    #link:https://www.gormanalysis.com/blog/decision-trees-in-r-using-rpart/
    predict<-predict(model_tree,newdata=data_test,type = "prob")
    #Performance measures
    
    predict=max.col(predict)-1
    predictions_summary=rbind(predictions_summary,data.frame(target=df1$target[folds[[k]]],prediction=predict))
    
    
    
  }

  xtab <- table(predictions_summary$prediction, predictions_summary$target)
  cm<- confusionMatrix(xtab)
  accuracy <- cm$overall[[1]]
  paramater_datat$accuracy[i]<-accuracy*100
}
print(paste("best index is : ",which.max(paramater_datat$accuracy)))


model_tree<-rpart(target~.-enrollee_id,data=data_train,method = "class",parms=list(split=paramater_datat$criteria[which.max(paramater_datat$accuracy)]),minsplit=(paramater_datat$min_num_obs[which.max(paramater_datat$accuracy)]+1),minbucket=paramater_datat$min_num_obs[which.max(paramater_datat$accuracy)],cp=paramater_datat$cp[which.max(paramater_datat$accuracy)]) 
predict<-predict(model_tree,newdata=data_test,type = "prob")

predict=max.col(predict)-1
predictions_summary=rbind(predictions_summary,data.frame(target=df1$target[folds[[k]]],prediction=predict))

xtab <- table(predictions_summary$prediction, predictions_summary$target)
cm<- confusionMatrix(xtab)
accuracy <- cm$overall[[1]]


fancyRpartPlot(model_tree)



