
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


######################OneR######################



########################################Using a Random Subsampling Method###################################

                #Datasets Post Transformations

#Data Preparation
dataset_4NA_EXP_POST_MICE$gender_Other <- as.factor(dataset_4NA_EXP_POST_MICE$gender_Other)
dataset_4NA_EXP_POST_MICE$gender_M <- as.factor(dataset_4NA_EXP_POST_MICE$gender_M)
dataset_4NA_EXP_POST_MICE$gender_F <- as.factor(dataset_4NA_EXP_POST_MICE$gender_F)
dataset_4NA_EXP_POST_MICE$relevent_experience_N <- as.factor(dataset_4NA_EXP_POST_MICE$relevent_experience_N)
dataset_4NA_EXP_POST_MICE$relevent_experience_Y <- as.factor(dataset_4NA_EXP_POST_MICE$relevent_experience_Y)
dataset_4NA_EXP_POST_MICE$enrolled_university_None <- as.factor(dataset_4NA_EXP_POST_MICE$enrolled_university_None)
dataset_4NA_EXP_POST_MICE$enrolled_university_PartTime <- as.factor(dataset_4NA_EXP_POST_MICE$enrolled_university_PartTime)
dataset_4NA_EXP_POST_MICE$enrolled_university_FullTime <- as.factor(dataset_4NA_EXP_POST_MICE$enrolled_university_FullTime)
dataset_4NA_EXP_POST_MICE$education_level <- as.factor(dataset_4NA_EXP_POST_MICE$education_level)
dataset_4NA_EXP_POST_MICE$major_discipline_None <- as.factor(dataset_4NA_EXP_POST_MICE$major_discipline_None)
dataset_4NA_EXP_POST_MICE$major_discipline_Arts <- as.factor(dataset_4NA_EXP_POST_MICE$major_discipline_Arts)
dataset_4NA_EXP_POST_MICE$major_discipline_Humanities <- as.factor(dataset_4NA_EXP_POST_MICE$major_discipline_Humanities)
dataset_4NA_EXP_POST_MICE$major_discipline_Business <- as.factor(dataset_4NA_EXP_POST_MICE$major_discipline_Business)
dataset_4NA_EXP_POST_MICE$major_discipline_STEM <- as.factor(dataset_4NA_EXP_POST_MICE$major_discipline_STEM)
dataset_4NA_EXP_POST_MICE$major_discipline_Other <- as.factor(dataset_4NA_EXP_POST_MICE$major_discipline_Other)
dataset_4NA_EXP_POST_MICE$experience <- as.factor(dataset_4NA_EXP_POST_MICE$experience)
dataset_4NA_EXP_POST_MICE$company_size <- as.factor(dataset_4NA_EXP_POST_MICE$company_size)
dataset_4NA_EXP_POST_MICE$company_type_FundedStartup <- as.factor(dataset_4NA_EXP_POST_MICE$company_type_FundedStartup)
dataset_4NA_EXP_POST_MICE$company_type_EarlyStageStartup <- as.factor(dataset_4NA_EXP_POST_MICE$company_type_EarlyStageStartup)
dataset_4NA_EXP_POST_MICE$company_type_PvtLtd <- as.factor(dataset_4NA_EXP_POST_MICE$company_type_PvtLtd)
dataset_4NA_EXP_POST_MICE$company_type_PublicSector <- as.factor(dataset_4NA_EXP_POST_MICE$company_type_PublicSector)
dataset_4NA_EXP_POST_MICE$company_type_NGO <- as.factor(dataset_4NA_EXP_POST_MICE$company_type_NGO)
dataset_4NA_EXP_POST_MICE$company_type_Other <- as.factor(dataset_4NA_EXP_POST_MICE$company_type_Other)
dataset_4NA_EXP_POST_MICE$last_new_job <- as.factor(dataset_4NA_EXP_POST_MICE$last_new_job)
dataset_4NA_EXP_POST_MICE$training_hours <- as.numeric(dataset_4NA_EXP_POST_MICE$training_hours)
#dataset_4NA_EXP_POST_MICE$target <- as.factor(dataset_4NA_EXP_POST_MICE$target)

#10 iterations
number_iterations <- 10
accuracy <- vector(length=number_iterations)

for (j in 1:number_iterations){
  set.seed(j)
  
  partitions <- partition(dataset_4NA_EXP_POST_MICE$target, p = c(train = 0.7, test = 0.3))
  
  Test  <- dataset_4NA_EXP_POST_MICE[partitions$test,]
  Train <- dataset_4NA_EXP_POST_MICE[partitions$train,]
  
  Model_OneR  <- OneR(Train[,-c(1, 4, 10, 14, 16, 18, 22, 24, 26, 28)])
  prediction_normal  <- predict(Model_OneR, Test[,-c(1, 4, 10, 14, 16, 18, 22, 24, 26, 28)])
  accuracy[j] <- sum(prediction_normal == Test$target)/nrow(Test)
}

#Accuracy is the mean of the vector
mean_accuracy <- mean(accuracy)

#Prediction Summary
predictions_summary_normal <- data.frame()
predictions_summary_normal <- rbind(predictions_summary_normal, data.frame(id=Test[,1], class=Test[,29], prediction_normal))

#Factors
predictions_summary_normal$class<-as.factor(predictions_summary_normal$class)
predictions_summary_normal$prediction_normal<-as.factor(predictions_summary_normal$prediction_normal)

#Obtaining the Confusion Matrix
xtab <- table(predictions_summary_normal$prediction_normal, predictions_summary_normal$class)
confusion_matrix<- confusionMatrix(xtab)
confusion_matrix

#Other Evaluation Metrics
f1 = confusion_matrix$byClass[[7]]
precision = confusion_matrix$byClass[[5]]
gmean = sqrt(confusion_matrix$byClass[[1]]*confusion_matrix$byClass[[2]])


                      #Datasets Pre Transformations

#Data Preparation
dataset_4NA_EXP_PRI_MICE$city <- as.factor(as.character(as.numeric(dataset_4NA_EXP_PRI_MICE$city)))
dataset_4NA_EXP_PRI_MICE$gender <- as.factor(dataset_4NA_EXP_PRI_MICE$gender)
dataset_4NA_EXP_PRI_MICE$relevent_experience <- as.factor(dataset_4NA_EXP_PRI_MICE$relevent_experience)
dataset_4NA_EXP_PRI_MICE$enrolled_university <- as.factor(dataset_4NA_EXP_PRI_MICE$enrolled_university)
dataset_4NA_EXP_PRI_MICE$education_level <- as.factor(dataset_4NA_EXP_PRI_MICE$education_level)
dataset_4NA_EXP_PRI_MICE$major_discipline <- as.factor(dataset_4NA_EXP_PRI_MICE$major_discipline)
dataset_4NA_EXP_PRI_MICE$experience <- as.factor(dataset_4NA_EXP_PRI_MICE$experience)
dataset_4NA_EXP_PRI_MICE$company_size <- as.factor(dataset_4NA_EXP_PRI_MICE$company_size)
dataset_4NA_EXP_PRI_MICE$company_type <- as.factor(dataset_4NA_EXP_PRI_MICE$company_type)
dataset_4NA_EXP_PRI_MICE$last_new_job <- as.factor(dataset_4NA_EXP_PRI_MICE$last_new_job)
dataset_4NA_EXP_PRI_MICE$training_hours <- as.numeric(dataset_4NA_EXP_PRI_MICE$training_hours)
#dataset_4NA_EXP_PRI_MICE$target <- as.factor(dataset_4NA_EXP_PRI_MICE$target)

#10 iterations
number_iterations <- 10
accuracy <- vector(length=number_iterations)

for (j in 1:number_iterations){
  set.seed(j)
  
  partitions <- partition(dataset_4NA_EXP_PRI_MICE$target, p = c(train = 0.7, test = 0.3))
  
  Test  <- dataset_4NA_EXP_PRI_MICE[partitions$test,]
  Train <- dataset_4NA_EXP_PRI_MICE[partitions$train,]
  
  Model_OneR  <- OneR(Train[,-c(1, 13)])
  prediction_normal  <- predict(Model_OneR, Test[,-c(1, 13)])
  accuracy[j] <- sum(prediction_normal == Test$target)/nrow(Test)
}

#Accuracy is the mean of the vector
mean_accuracy <- mean(accuracy)

#Prediction Summary
predictions_summary_normal <- data.frame()
predictions_summary_normal <- rbind(predictions_summary_normal, data.frame(id=Test[,1], class=Test[,14], prediction_normal))

#Factors
predictions_summary_normal$class<-as.factor(predictions_summary_normal$class)
predictions_summary_normal$prediction_normal<-as.factor(predictions_summary_normal$prediction_normal)

#Obtaining the Confusion Matrix
xtab <- table(predictions_summary_normal$prediction_normal, predictions_summary_normal$class)
confusion_matrix<- confusionMatrix(xtab)
confusion_matrix

#Other Evaluation Metrics
f1 = confusion_matrix$byClass[[7]]
precision = confusion_matrix$byClass[[5]]
gmean = sqrt(confusion_matrix$byClass[[1]]*confusion_matrix$byClass[[2]])
