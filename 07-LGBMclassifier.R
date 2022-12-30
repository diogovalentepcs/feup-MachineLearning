

# LightGBM is a gradient boosting framework that uses tree based learning algorithms. It is designed to be distributed and efficient with the following advantages:
# Faster training speed and higher efficiency
# Lower memory usage
# Better accuracy.
# Support of parallel, distributed, and GPU learning.
# Capable of handling large-scale data.




dataset <- read.csv("Dataset\\dataset_1-KNN.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))

# Change categorical or ordinal variables to factor
dataset = subset(dataset, select = -c(X, enrollee_id))


col_names <- names(dataset)
dataset[,col_names] <- lapply(dataset[,col_names], factor)



### Explanatory data analysis

#dependent variable

table(dataset$target)

# Like always we will split the data into training and validation sets before moving forward.

dataset$target <- as.numeric (as.character(dataset$target))

# training and validation model

set.seed(210)

smp_size <- floor(0.7*nrow(dataset))
part <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[part,]
val <- dataset[-part,]

# NOTE: LightGBM demands we prepare our training and validation data set in a special manner unlike logistic or other classifiers. The data type should be lbg dataset and the target variable needs to be predefined.

trainm <- sparse.model.matrix (target~., data = train)
train_label <- train[,"target"]

valm <- sparse.model.matrix (target~., data = val) 
val_label <- val[,"target"]

train_matrix <- lgb.Dataset (data = as.matrix(trainm), label = train_label)
val_matrix <- lgb.Dataset(data=as.matrix(valm), label = val_label)


# Training LightGBM model

valid <- list(test = val_matrix)

# model parameters
params = list(max_bin = 5,
              learning_rate = 0.001,
              objective = "binary",
              metric = 'binary_logloss')

#model training
bst = lightgbm(params = params, train_matrix, valid, nrounds = 200)


# Prediction and Model Evaluation

# Prediction & confusion matrix
p <- predict (bst,valm)
val$predicted <- ifelse (p>0.3,1,0)
confusion_matrix <- confusionMatrix(factor(val$predicted), factor(val$target))
confusion_matrix


# Evaluation Curve
pred=prediction(p,val$target)
eval= performance(pred,"acc")
plot(eval)


# ROC
roc=performance(pred,"tpr","fpr")
plot(roc,main="ROC curve")
abline(a=0,b=1)






