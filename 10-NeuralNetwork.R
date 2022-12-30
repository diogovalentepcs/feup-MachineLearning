
################################################
############# 10. Neural Network ###############
################################################



# data loading
data = read.csv('Dataset\\dataset_1-KNN.csv', header = T, sep =',',dec='.', stringsAsFactors = F)

data = subset(data,  select=-c(X,enrollee_id)) #drop id

set.seed(123)

################################################
########### 10.1 Parameter tunning #############
################################################

# Parameter Tunning (for single partition, due to time restricitons)
max_nodes_per_layer = 6 # Empiric decision -> 7+ nodes takes too long to compute (3h+)
max_layers = 2 # Empiric decision -> unusual to have more than 2 hidden layers for simple predicitons (nodes in 2nd layer are calculated after optimal 1st layer nodes)


# Layer 1 Tunning
summary = data.frame(Accuracy = numeric(), F1 = numeric(), GMean = numeric(), Sum = numeric())

for (node in 1:max_nodes_per_layer){
  
  indexes=createDataPartition(data$target, p=.8, list = F)
  train = data[indexes, ]
  test = data[-indexes, ] 
  xtest = subset(test,  select=-c(target))
  ytest = subset(test,  select=c(target))
  
  nnet = neuralnet(target~., train, hidden = node, linear.output = FALSE, threshold = 0.01, lifesign = "full", stepmax = 1e+06)
  
  
  ypred = neuralnet::compute(nnet, xtest) 
  ypred = ypred$net.result
  ypred = data.frame(ypred)
  ypred$ypred = round(ypred$ypred ) # round to match target
  
  # model analyisis
  cm=confusionMatrix(factor(ytest$target), factor(ypred$ypred))
  
  CM_table = cm$table
  cm_accuracy = cm$overall[[1]]
  cm_sensitivity =cm$byClass[[1]]
  cm_f1 = cm$byClass[[7]]
  cm_gmean = sqrt(cm_sensitivity * cm$byClass[[2]] )
  
  
  # add to  summary
  summary[nrow(summary) + 1,] = c(cm_accuracy, cm_f1, cm_gmean, cm_accuracy + cm_f1 + cm_accuracy)
}

# Best node number for layer 1
layer_1 = as.numeric(which.max(summary$Sum)) # retrieves index of the highest sum of metrics

# Layer 2 Tunning
max_nodes_per_layer = layer_1

summary = data.frame(Accuracy = numeric(), F1 = numeric(), GMean = numeric(), Sum = numeric())

for (node in 1:max_nodes_per_layer){
  
  indexes=createDataPartition(data$target, p=.8, list = F)
  train = data[indexes, ]
  test = data[-indexes, ] 
  xtest = subset(test,  select=-c(target))
  ytest = subset(test,  select=c(target))
  
  nnet = neuralnet(target~., train, hidden = c(layer_1,node), linear.output = FALSE, threshold = 0.01, lifesign = "full", stepmax = 1e+06)
  
  
  ypred = neuralnet::compute(nnet, xtest) 
  ypred = ypred$net.result
  ypred = data.frame(ypred)
  ypred$ypred = round(ypred$ypred ) # round to match target
  
  # model analyisis
  cm=confusionMatrix(factor(ytest$target), factor(ypred$ypred))
  
  CM_table = cm$table
  cm_accuracy = cm$overall[[1]]
  cm_sensitivity =cm$byClass[[1]]
  cm_f1 = cm$byClass[[7]]
  cm_gmean = sqrt(cm_sensitivity * cm$byClass[[2]] )
  
  
  # add to  summary
  summary[nrow(summary) + 1,] = c(cm_accuracy, cm_f1, cm_gmean, cm_accuracy + cm_f1 + cm_accuracy)
}

# Best node number for layer 2 (only if better results than without)
layer_2 = ifelse(max(summary_2$Sum)>max(summary$Sum), as.numeric(which.max(summary_2$Sum)), 0)

####### Results: hidden = 3 (only 1 layer)

# layer_1 = 3
# layer_2 = 0


################################################
########### 10.2 Cross Validation ##############
################################################

num_folds = 10
folds = createFolds(data$target, num_folds)

predictions = data.frame(target = numeric(), prediction = numeric())

for (k in 1:num_folds){
  k=1
  train = data[-folds[[k]],]
  
  test = data[folds[[k]],]
  xtest = subset(test,  select=-c(target))
  ytest = subset(test,  select=c(target))
  
  # model run
  if (layer_2 > 0){
    nnet = neuralnet(target~., train, hidden = c(layer_1,layer_2), linear.output = FALSE, threshold = 0.01, lifesign = "full", stepmax = 1e+06)
  } else {
    nnet = neuralnet(target~., train, hidden = layer_1, linear.output = FALSE, threshold = 0.01, lifesign = "full", stepmax = 1e+06)
  }
    

  ypred = neuralnet::compute(nnet, xtest) 
  ypred = ypred$net.result
  ypred = data.frame(ypred)
  ypred$ypred = round(ypred$ypred ) # round to match target
  
  predictions = rbind(predictions, data.frame(target = ytest$target, prediction = ypred$ypred))
}


################################################
############# 10.3 Plot & Metrics ##############
################################################

plot(nnet)
dev.print(pdf, 'Visualizations\\Neural_Network.pdf') 


# Calculating Metrics
cm=confusionMatrix(factor(ytest$target), factor(ypred$ypred))
CM_table = cm$table
cm_accuracy = cm$overall[[1]]
cm_sensitivity =cm$byClass[[1]]
cm_f1 = cm$byClass[[7]]
cm_gmean = sqrt(cm_sensitivity * cm$byClass[[2]] )

results = data.frame(Accuracy = c(cm_accuracy), F1 = c(cm_f1), GMean = c(cm_gmean))
