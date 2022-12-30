
################################################
##### 2. Preprocessing & Dataset Creation ######
################################################

# Goals: Process data and create different data sets for testing
# Main Outputs: Datasets (csv) and dataset description

dataset <- read.csv(file = "Datasets\\dataset_eda.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
dataset <- subset(dataset, select = -c(X))


#HOW TO OBTAIN DATASET_4NA_EXP_POST (THIS IS EXPLAINED LATER IN THE SCRIPT)
aux <- apply(dataset, 1, function(x) sum(is.na(x)))
which(aux >= 5)
dataset_4NA_EXP_POST <- dataset[- c(which(aux >= 5)), ]

#HOW TO OBTAIN DATASET_5NA_EXP_POST (THIS IS EXPLAINED LATER IN THE SCRIPT)
aux <- apply(dataset, 1, function(x) sum(is.na(x)))
which(aux >= 6)
dataset_5NA_EXP_POST <- dataset[- c(which(aux >= 6)), ]



################################################
##### 2.1 Encoding Categorical Variables #######
################################################


install.packages('forcats') #combining factor levels
library(forcats)
library(ggplot2)

#NOMINAL VARIABLES

#CITY
#1: TARGET ENCODING 
#Target encoding is an approach to transform the categorical column to a new numeric columns based on values of target variable.
install.packages("dataPreparation") #data preparation package needed for target encoding
library(dataPreparation)

#Needed transformations for target encoding
dataset_4NA_EXP_POST$city <- as.character(dataset_4NA_EXP_POST$city)
dataset_5NA_EXP_POST$city <- as.character(dataset_5NA_EXP_POST$city)
dataset_4NA_EXP_POST$target <- as.numeric(as.character(dataset_4NA_EXP_POST$target))
dataset_5NA_EXP_POST$target <- as.numeric(as.character(dataset_5NA_EXP_POST$target))

#Using the target_encode function
aux <- build_target_encoding(dataset_4NA_EXP_POST, cols_to_encode = "city", target_col = "target", functions = "mean", verbose = TRUE)
dataset_4NA_EXP_POST <- target_encode(dataset_4NA_EXP_POST, aux, drop = FALSE, verbose = TRUE)

aux <- build_target_encoding(dataset_5NA_EXP_POST, cols_to_encode = "city", target_col = "target", functions = "mean", verbose = TRUE)
dataset_5NA_EXP_POST <- target_encode(dataset_5NA_EXP_POST, aux, drop = FALSE, verbose = TRUE)

#Replacing the city code column with the new city column in which the target encoding was performed
dataset_4NA_EXP_POST$city <- dataset_4NA_EXP_POST$target_mean_by_city
dataset_5NA_EXP_POST$city <- dataset_5NA_EXP_POST$target_mean_by_city

dataset_4NA_EXP_POST$target_mean_by_city <- NULL
dataset_5NA_EXP_POST$target_mean_by_city <- NULL

#Changing order between id column and city column
dataset_4NA_EXP_POST <- dataset_4NA_EXP_POST[,c(2,1,3:14)]
dataset_5NA_EXP_POST <- dataset_5NA_EXP_POST[,c(2,1,3:14)]


#OTHER NOMINAL VARIABLES
#2: ONE HOT ENCODING 
#One-hot encoding is an approach to convert one categorical column to multiple binary (0 or 1) columns as many as the number of distinct levels in the original column
library(mltools) #Package that contains the one_hot function
library(data.table)

dataset_4NA_EXP_POST <- one_hot(as.data.table(dataset_4NA_EXP_POST))
dataset_5NA_EXP_POST <- one_hot(as.data.table(dataset_5NA_EXP_POST))


# Dropping extra variables (One Hot Class With Less Observations)
dataset_4NA_EXP_POST <- subset(dataset_4NA_EXP_POST, select(-c(15,17,20,23)))
dataset_5NA_EXP_POST <- subset(dataset_5NA_EXP_POST, select(-c(15,17,20,23)))

#ORDINAL VARIABLES
#3: LABEL ENCODING
#Label encoding is an approach to convert the levels to integers
dataset_4NA_EXP_POST$education_level = factor(dataset_4NA_EXP_POST$education_level,
                                              levels = c("Primary School", "High School", "Graduate", "Masters", "Phd"),
                                              labels = c(0:4))

levels(dataset_4NA_EXP_POST$experience) <- c(0:21)
levels(dataset_4NA_EXP_POST$company_size) <- c(0:7)
levels(dataset_4NA_EXP_POST$last_new_job) <- c(0:5)

dataset_5NA_EXP_POST$education_level = factor(dataset_5NA_EXP_POST$education_level,
                                              levels = c("Primary School", "High School", "Graduate", "Masters", "Phd"),
                                              labels = c(0:4))

levels(dataset_5NA_EXP_POST$experience) <- c(0:21)
levels(dataset_5NA_EXP_POST$company_size) <- c(0:7)
levels(dataset_5NA_EXP_POST$last_new_job) <- c(0:5)



#Moving on with sixteen different datasets taking in account four different factors (2^4 combinations):
#1st Factor: Maximum number of NA's per observation (4 or 5)
#2nd Factor: Eliminating observations in variables with a very small number of  NA's (experience)
#3rd Factor: Prior or post variable transformation
#4th Factor: Perform knnImputation for missing values for all the dataset or MICE for all variables below 5% of NA'S and knnImputation for the other 3 variables

#NOTE:
#Multivariate imputation by chained equations (MICE) is one of the principled methods of addressing missing data.
#MICE creates multiple imputations, as opposed to single imputations, and accounts for the statistical uncertainty in the imputations.
#In addition, the chained equations approach is very flexible and can handle variables of varying types (e.g. continuous or binary) as well as complexities such as bounds or survey skip patterns

#Justification for the 4 factors chosen:
#1st Factor: data reduction while preserving still a high number number of observations (over 98%)
#2nd Factor: data reduction (one less feature to predict) while only removing a small number of observations (65)
#3rd Factor: verify if data transformation was successful or not
#4th Factor:
# knnImputation is more adequate than replacing NA's with the mode of the column due to the fact that the data is imbalanced in almost all features
# MICE shouldn't be used in features that contain  more than 5% of missing data. For those variables, we decided to replace using kNN

#Dataset description
#dataset_4NA_NOEXP_PRI_KNN  -> Contains only observations with 4 or less variables with NA. Observations with NA's in the experience feature were removed. Dataset prior to variable transformations. knn imputation method used in all NA's 
#dataset_4NA_NOEXP_PRI_MICE  -> Contains only observations with 4 or less variables with NA. Observations with NA's in the experience feature were removed. Dataset prior to variable transformations. knn imputation method and MICE for missing values
#dataset_4NA_NOEXP_POST_KNN -> Contains only observations with 4 or less variables with NA. Observations with NA's in the experience feature were removed. Dataset post to variable transformations. knn imputation method used in all NA's 
#dataset_4NA_NOEXP_POST_MICE -> Contains only observations with 4 or less variables with NA. Observations with NA's in the experience feature were removed. Dataset post to variable transformations. knn imputation method and MICE for missing values
#dataset_4NA_EXP_PRI_KNN  -> Contains only observations with 4 or less variables with NA. Observations with NA's in the experience feature were not removed. Dataset prior to variable transformations. knn imputation method used in all NA's
#dataset_4NA_EXP_PRI_MICE  -> Contains only observations with 4 or less variables with NA. Observations with NA's in the experience feature were not removed. Dataset prior to variable transformations. knn imputation method and MICE for missing values
#dataset_4NA_EXP_POST_KNN ->  Contains only observations with 4 or less variables with NA. Observations with NA's in the experience feature were not removed. Dataset post to variable transformations. knn imputation method used in all NA's 
#dataset_4NA_EXP_POST_MICE ->  Contains only observations with 4 or less variables with NA. Observations with NA's in the experience feature were not removed. Dataset post to variable transformations. knn imputation method and MICE for missing values
#dataset_5NA_NOEXP_PRI_KNN  -> Contains only observations with 5 or less variables with NA. Observations with NA's in the experience feature were removed. Dataset prior to variable transformations. knn imputation method used in all NA's 
#dataset_5NA_NOEXP_PRI_MICE  -> Contains only observations with 5 or less variables with NA. Observations with NA's in the experience feature were removed. Dataset prior to variable transformations. knn imputation method and MICE for missing values
#dataset_5NA_NOEXP_POST_KNN -> Contains only observations with 5 or less variables with NA. Observations with NA's in the experience feature were removed. Dataset post to variable transformations. knn imputation method used in all NA's
#dataset_5NA_NOEXP_POST_MICE ->  Contains only observations with 5 or less variables with NA. Observations with NA's in the experience feature were removed. Dataset post to variable transformations. knn imputation method and MICE for missing values
#dataset_5NA_EXP_PRI_KNN  ->  Contains only observations with 5 or less variables with NA. Observations with NA's in the experience feature were not removed. Dataset prior to variable transformations. knn imputation method used in all NA's
#dataset_5NA_EXP_PRI_MICE  -> Contains only observations with 5 or less variables with NA. Observations with NA's in the experience feature were not removed. Dataset prior to variable transformations. knn imputation method and MICE for missing values
#dataset_5NA_EXP_POST_KNN -> Contains only observations with 5 or less variables with NA. Observations with NA's in the experience feature were not removed. Dataset post to variable transformations. knn imputation method used in all NA's 
#dataset_5NA_EXP_POST_MICE -> Contains only observations with 5 or less variables with NA. Observations with NA's in the experience feature were not removed. Dataset post to variable transformations. knn imputation method and MICE for missing values


################################################
####### 2.2 Missing Values Imputation ##########
################################################

#HOW TO OBTAIN DATASET_4NA_EXP_PRI
aux <- apply(dataset_test, 1, function(x) sum(is.na(x)))
which(aux >= 5)
dataset_4NA_EXP_PRI <- dataset_test[- c(which(aux >= 5)), ]


#HOW TO OBTAIN DATASET_5NA_EXP_PRI
aux <- apply(dataset_test, 1, function(x) sum(is.na(x)))
which(aux >= 6)
dataset_5NA_EXP_PRI <- dataset_test[- c(which(aux >= 6)), ]


#HOW TO OBTAIN DATASETS WITHOUT OBSERVATIONS THAT HAVE MISSING VALUES IN THE EXPERIENCE FEATURE
dataset_4NA_NOEXP_PRI <- dataset_4NA_EXP_PRI[!(is.na(dataset_4NA_EXP_PRI$experience)),]
dataset_4NA_NOEXP_POST <- dataset_4NA_EXP_POST[!(is.na(dataset_4NA_EXP_POST$experience)),]
dataset_5NA_NOEXP_PRI <- dataset_5NA_EXP_PRI[!(is.na(dataset_5NA_EXP_PRI$experience)),]
dataset_5NA_NOEXP_POST <- dataset_5NA_EXP_POST[!(is.na(dataset_5NA_EXP_POST$experience)),]


#REPLACING MISSING VALUES (TWO OPTIONS CHOSED)
#1: ONLY KNN-IMPUTATION (EIGHT DATASETS)

library(VIM) #package containing k-Nearest Neighbour Imputation

dataset_4NA_NOEXP_PRI_KNN <- dataset_4NA_NOEXP_PRI
Total_NA <- as.numeric(table(is.na(dataset_4NA_NOEXP_PRI_KNN))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_4NA_NOEXP_PRI_KNN <- kNN(dataset_4NA_NOEXP_PRI_KNN, k = k) #kNN imputation
dataset_4NA_NOEXP_PRI_KNN <- dataset_4NA_NOEXP_PRI_KNN[,-c(15:28)]

dataset_5NA_NOEXP_PRI_KNN <- dataset_5NA_NOEXP_PRI
Total_NA <- as.numeric(table(is.na(dataset_5NA_NOEXP_PRI_KNN))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_5NA_NOEXP_PRI_KNN <- kNN(dataset_5NA_NOEXP_PRI_KNN, k = k) #kNN imputation
dataset_5NA_NOEXP_PRI_KNN <- dataset_5NA_NOEXP_PRI_KNN[,-c(15:28)]

dataset_4NA_NOEXP_POST_KNN <- dataset_4NA_NOEXP_POST
Total_NA <- as.numeric(table(is.na(dataset_4NA_NOEXP_POST_KNN))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_4NA_NOEXP_POST_KNN <- kNN(dataset_4NA_NOEXP_POST_KNN, k = k) #kNN imputation
dataset_4NA_NOEXP_POST_KNN <- dataset_4NA_NOEXP_POST_KNN[,-c(30:58)]

dataset_5NA_NOEXP_POST_KNN <- dataset_5NA_NOEXP_POST
Total_NA <- as.numeric(table(is.na(dataset_5NA_NOEXP_POST_KNN))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_5NA_NOEXP_POST_KNN <- kNN(dataset_5NA_NOEXP_POST_KNN, k = k) #kNN imputation
dataset_5NA_NOEXP_POST_KNN <- dataset_5NA_NOEXP_POST_KNN[,-c(30:58)]

dataset_4NA_EXP_PRI_KNN <- dataset_4NA_EXP_PRI
Total_NA <- as.numeric(table(is.na(dataset_4NA_EXP_PRI_KNN))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_4NA_EXP_PRI_KNN <- kNN(dataset_4NA_EXP_PRI_KNN, k = k) #kNN imputation
dataset_4NA_EXP_PRI_KNN <- dataset_4NA_EXP_PRI_KNN[,-c(15:28)]

dataset_5NA_EXP_PRI_KNN <- dataset_5NA_EXP_PRI
Total_NA <- as.numeric(table(is.na(dataset_5NA_EXP_PRI_KNN))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_5NA_EXP_PRI_KNN <- kNN(dataset_5NA_EXP_PRI_KNN, k = k) #kNN imputation
dataset_5NA_EXP_PRI_KNN <- dataset_5NA_EXP_PRI_KNN[,-c(15:28)]

dataset_4NA_EXP_POST_KNN <- dataset_4NA_EXP_POST
Total_NA <- as.numeric(table(is.na(dataset_4NA_EXP_POST_KNN))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_4NA_EXP_POST_KNN <- kNN(dataset_4NA_EXP_POST_KNN, k = k) #kNN imputation
dataset_4NA_EXP_POST_KNN <- dataset_4NA_EXP_POST_KNN[,-c(30:58)]

dataset_5NA_EXP_POST_KNN <- dataset_5NA_EXP_POST
Total_NA <- as.numeric(table(is.na(dataset_5NA_EXP_POST_KNN))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_5NA_EXP_POST_KNN <- kNN(dataset_5NA_EXP_POST_KNN, k = k) #kNN imputation
dataset_5NA_EXP_POST_KNN <- dataset_5NA_EXP_POST_KNN[,-c(30:58)]

#2: kNN AND MICE (EIGHT DATASETS)
install.packages("mice")
library("mice")

#KNN FOR GENDER, COMPANY SIZE AND COMPANY TYPE
dataset_4NA_NOEXP_PRI_MICE <- dataset_4NA_NOEXP_PRI
Total_NA <- as.numeric(table(is.na(dataset_4NA_NOEXP_PRI_MICE))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_4NA_NOEXP_PRI_MICE <- kNN(data = dataset_4NA_NOEXP_PRI_MICE, variable = c('gender', 'company_size', 'company_type'), k = k) #kNN imputation
dataset_4NA_NOEXP_PRI_MICE <- dataset_4NA_NOEXP_PRI_MICE[,-c(15:17)]


dataset_4NA_NOEXP_PRI_MICE  <- complete(mice(dataset_4NA_NOEXP_PRI_MICE, maxit = 1, method = 'rf'))


dataset_5NA_NOEXP_PRI_MICE <- dataset_5NA_NOEXP_PRI
Total_NA <- as.numeric(table(is.na(dataset_5NA_NOEXP_PRI_MICE))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_5NA_NOEXP_PRI_MICE <- kNN(data = dataset_5NA_NOEXP_PRI_MICE, variable = c('gender', 'company_size', 'company_type'), k = k) #kNN imputation
dataset_5NA_NOEXP_PRI_MICE <- dataset_5NA_NOEXP_PRI_MICE[,-c(15:17)]

dataset_5NA_NOEXP_PRI_MICE  <- complete(mice(dataset_5NA_NOEXP_PRI_MICE, maxit = 1, method = 'rf'))


dataset_4NA_NOEXP_POST_MICE <- dataset_4NA_NOEXP_POST
Total_NA <- as.numeric(table(is.na(dataset_4NA_NOEXP_POST_MICE))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_4NA_NOEXP_POST_MICE <- kNN(data = dataset_4NA_NOEXP_POST_MICE, variable = c('gender_Other', 'gender_M', 'gender_F', 'company_size', 'company_type_Funded Startup', 'company_type_Early Stage Startup', 'company_type_Pvt Ltd', 'company_type_Public Sector', 'company_type_NGO', 'company_type_Other'), k = k) #kNN imputation
dataset_4NA_NOEXP_POST_MICE <- dataset_4NA_NOEXP_POST_MICE[,-c(30:39)]

#CHANGING COLUMN NAMES IN ORDER TO USE MICE
colnames(dataset_4NA_NOEXP_POST_MICE)[10] <- "enrolled_university_PartTime"
colnames(dataset_4NA_NOEXP_POST_MICE)[11] <- "enrolled_university_FullTime"
colnames(dataset_4NA_NOEXP_POST_MICE)[21] <- "company_type_FundedStartup"
colnames(dataset_4NA_NOEXP_POST_MICE)[22] <- "company_type_EarlyStageStartup"
colnames(dataset_4NA_NOEXP_POST_MICE)[23] <- "company_type_PvtLtd"
colnames(dataset_4NA_NOEXP_POST_MICE)[24] <- "company_type_PublicSector"

dataset_4NA_NOEXP_POST_MICE  <- complete(mice(dataset_4NA_NOEXP_POST_MICE, maxit = 1, method = 'rf'))


dataset_5NA_NOEXP_POST_MICE <- dataset_5NA_NOEXP_POST
Total_NA <- as.numeric(table(is.na(dataset_5NA_NOEXP_POST_MICE))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_5NA_NOEXP_POST_MICE <- kNN(data = dataset_5NA_NOEXP_POST_MICE, variable = c('gender_Other', 'gender_M', 'gender_F', 'company_size', 'company_type_Funded Startup', 'company_type_Early Stage Startup', 'company_type_Pvt Ltd', 'company_type_Public Sector', 'company_type_NGO', 'company_type_Other'), k = k) #kNN imputation
dataset_5NA_NOEXP_POST_MICE <- dataset_5NA_NOEXP_POST_MICE[,-c(30:39)]

#CHANGING COLUMN NAMES IN ORDER TO USE MICE
colnames(dataset_5NA_NOEXP_POST_MICE)[10] <- "enrolled_university_PartTime"
colnames(dataset_5NA_NOEXP_POST_MICE)[11] <- "enrolled_university_FullTime"
colnames(dataset_5NA_NOEXP_POST_MICE)[21] <- "company_type_FundedStartup"
colnames(dataset_5NA_NOEXP_POST_MICE)[22] <- "company_type_EarlyStageStartup"
colnames(dataset_5NA_NOEXP_POST_MICE)[23] <- "company_type_PvtLtd"
colnames(dataset_5NA_NOEXP_POST_MICE)[24] <- "company_type_PublicSector"

dataset_5NA_NOEXP_POST_MICE  <- complete(mice(dataset_5NA_NOEXP_POST_MICE, maxit = 1, method = 'rf'))


dataset_4NA_EXP_PRI_MICE <- dataset_4NA_EXP_PRI
Total_NA <- as.numeric(table(is.na(dataset_4NA_EXP_PRI_MICE))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_4NA_EXP_PRI_MICE <- kNN(data = dataset_4NA_EXP_PRI_MICE, variable = c('gender', 'company_size', 'company_type'), k = k) #kNN imputation
dataset_4NA_EXP_PRI_MICE <- dataset_4NA_EXP_PRI_MICE[,-c(15:17)]


dataset_4NA_EXP_PRI_MICE  <- complete(mice(dataset_4NA_EXP_PRI_MICE, maxit = 1, method = 'rf'))



dataset_5NA_EXP_PRI_MICE <- dataset_5NA_EXP_PRI
Total_NA <- as.numeric(table(is.na(dataset_5NA_EXP_PRI_MICE))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_5NA_EXP_PRI_MICE <- kNN(data = dataset_5NA_EXP_PRI_MICE, variable = c('gender', 'company_size', 'company_type'), k = k) #kNN imputation
dataset_5NA_EXP_PRI_MICE <- dataset_5NA_EXP_PRI_MICE[,-c(15:17)]

dataset_5NA_EXP_PRI_MICE  <- complete(mice(dataset_5NA_EXP_PRI_MICE, maxit = 1, method = 'rf'))


dataset_4NA_EXP_POST_MICE <- dataset_4NA_EXP_POST
Total_NA <- as.numeric(table(is.na(dataset_4NA_EXP_POST_MICE))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_4NA_EXP_POST_MICE <- kNN(data = dataset_4NA_EXP_POST_MICE, variable = c('gender_Other', 'gender_M', 'gender_F', 'company_size', 'company_type_Funded Startup', 'company_type_Early Stage Startup', 'company_type_Pvt Ltd', 'company_type_Public Sector', 'company_type_NGO', 'company_type_Other'), k = k) #kNN imputation
dataset_4NA_EXP_POST_MICE <- dataset_4NA_EXP_POST_MICE[,-c(30:39)]

#CHANGING COLUMN NAMES IN ORDER TO USE MICE
colnames(dataset_4NA_EXP_POST_MICE)[10] <- "enrolled_university_PartTime"
colnames(dataset_4NA_EXP_POST_MICE)[11] <- "enrolled_university_FullTime"
colnames(dataset_4NA_EXP_POST_MICE)[21] <- "company_type_FundedStartup"
colnames(dataset_4NA_EXP_POST_MICE)[22] <- "company_type_EarlyStageStartup"
colnames(dataset_4NA_EXP_POST_MICE)[23] <- "company_type_PvtLtd"
colnames(dataset_4NA_EXP_POST_MICE)[24] <- "company_type_PublicSector"

dataset_4NA_EXP_POST_MICE  <- complete(mice(dataset_4NA_EXP_POST_MICE, maxit = 1, method = 'rf'))


dataset_5NA_EXP_POST_MICE <- dataset_5NA_EXP_POST
Total_NA <- as.numeric(table(is.na(dataset_5NA_EXP_POST_MICE))[2])  #Number of samples
k <- round(sqrt(Total_NA), 0) #optimal k
dataset_5NA_EXP_POST_MICE <- kNN(data = dataset_5NA_EXP_POST_MICE, variable = c('gender_Other', 'gender_M', 'gender_F', 'company_size', 'company_type_Funded Startup', 'company_type_Early Stage Startup', 'company_type_Pvt Ltd', 'company_type_Public Sector', 'company_type_NGO', 'company_type_Other'), k = k) #kNN imputation
dataset_5NA_EXP_POST_MICE <- dataset_5NA_EXP_POST_MICE[,-c(30:39)]

#CHANGING COLUMN NAMES IN ORDER TO USE MICE
colnames(dataset_5NA_EXP_POST_MICE)[10] <- "enrolled_university_PartTime"
colnames(dataset_5NA_EXP_POST_MICE)[11] <- "enrolled_university_FullTime"
colnames(dataset_5NA_EXP_POST_MICE)[21] <- "company_type_FundedStartup"
colnames(dataset_5NA_EXP_POST_MICE)[22] <- "company_type_EarlyStageStartup"
colnames(dataset_5NA_EXP_POST_MICE)[23] <- "company_type_PvtLtd"
colnames(dataset_5NA_EXP_POST_MICE)[24] <- "company_type_PublicSector"

dataset_5NA_EXP_POST_MICE  <- complete(mice(dataset_5NA_EXP_POST_MICE, maxit = 1, method = 'rf'))



################################################
####### 2.3 Feature Correlation ##########
################################################


#The enrollee_id should be ignored
#The training_hours feature will not be included in future models since it has a very weak correlation with the target variable
#The training_hours feature would be the only numeric feature that would need to be standardized (using the z-score method) in order to be used in the models

#1: BORUTA 

#Boruta is a feature selection algorithm.
#Precisely, it works as a wrapper algorithm around Random Forest

install.packages("Boruta")
library(Boruta)

# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(target ~ .-enrollee_id, data=na.omit(dataset_4NA_NOEXP_POST_MICE), doTrace=2)  # perform Boruta search
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

# ["city"]                            ["city_development_index"]           ["gender_M"]                      
# ["gender_F"]                        ["relevent_experience_N"]            ["relevent_experience_Y"]         
# ["enrolled_university_None"]        ["enrolled_university_PartTime"]     ["enrolled_university_FullTime"]  
# ["education_level"]                 ["major_discipline_None"]            ["major_discipline_Humanities"]   
# ["major_discipline_STEM"]           ["experience"]                       ["company_size"]                  
# ["company_type_FundedStartup"]      ["company_type_EarlyStageStartup"]   ["company_type_PvtLtd"]           
# ["company_type_PublicSector"]       ["company_type_NGO"]                 ["company_type_Other"]            
# ["last_new_job"]

plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance


#2: CHI-SQUARE TEST (POST TRANSFORMATION)
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$city)                             #variables related 
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$gender_M)                         #variables related 
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$gender_F)                         #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$gender_Other)                     #variables not related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$relevent_experience_N)            #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$relevent_experience_Y)            #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$enrolled_university_None)         #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$enrolled_university_PartTime)     #variables not related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$enrolled_university_FullTime)     #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$education_level)                  #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$major_discipline_None)            #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$major_discipline_Arts)            #variables not related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$major_discipline_Humanities)      #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$major_discipline_Business)        #variables not related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$major_discipline_Other)           #variables not related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$major_discipline_STEM)            #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$company_type_FundedStartup)       #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$company_type_EarlyStageStartup)   #variables not related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$company_type_PvtLtd)              #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$company_type_PublicSector)        #variables not related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$company_type_NGO)                 #variables related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$company_type_Other)               #variables not related
chisq.test(dataset_4NA_NOEXP_POST_MICE$target, dataset_4NA_NOEXP_POST_MICE$last_new_job)                     #variables related



################################################
####### 2.4 Data Sampling ##########
################################################

#Data partition for one of the datasets:
library(splitTools)

set.seed(200)
data_type <- partition(dataset_4NA_EXP_POST_KNN$target, p = c(train = 0.7, test = 0.3)) 

data_train <- dataset_4NA_EXP_POST_KNN[data_type$train, ]
data_test  <- dataset_4NA_EXP_POST_KNN[data_type$test, ]


#Verify if the target variable data distribution is still imbalanced
table(data_train$target)   
prop.table(table(data_train$target))



#Undersampling
data_train_Under <- ovun.sample(target ~ ., data = data_train, method = "under", N = 2 * min(table(data_train$target)), seed = 10)$data
table(data_train_Under$target) 

#Oversampling
data_train_Over <- ovun.sample(target ~ ., data = data_train, method = "over", N = 2 * max(table(data_train$target)), seed = 20)$data
table(data_train_Over$target)  


#Both
data_train_Both <- ovun.sample(target ~ ., data = data_train, method = "both", N = nrow(data_train), p = 0.5, seed = 30)$data
table(data_train_Both$target)  


#Synthetic Data Generation (ROSE) 

#ROSE (Random Over-Sampling Ex- amples) is a bootstrap-based technique which aids the task of binary classification in the presence of rare classes.
#It handles both continuous and categorical data by generating synthetic examples from a conditional density estimate of the two classes.

install.packages("ROSE")
library(ROSE)

data_train_ROSE <- ROSE(target ~ ., data = data_train, N = nrow(data_train), seed = 40)$data
table(data_train_ROSE$target)

#SMOTE

#Synthetic Minority Oversampling Technique (SMOTE) is a statistical technique for increasing the number of cases in your dataset in a balanced way.
#The component works by generating new instances from existing minority cases that you supply as input.

install.packages("performanceEstimation")
library(performanceEstimation)

data_train_SMOTE <- smote(target~., data = data_train, perc.over = 2, k = 5, perc.under = 1)
table(data_train_SMOTE$target)

#EXPORT ALL DATASETS
write.csv(x = dataset_4NA_NOEXP_PRI_KNN, file = "Dataset\\dataset_4NA_NOEXP_PRI_KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_NOEXP_PRI_MICE,"Dataset\\dataset_4NA_NOEXP_PRI_MICE.csv", row.names = FALSE)
write.csv(dataset_4NA_NOEXP_POST_KNN,"Dataset\\dataset_4NA_NOEXP_POST_KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_NOEXP_POST_MICE,"Dataset\\dataset_4NA_NOEXP_POST_MICE.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_PRI_KNN,"Dataset\\Dataset\\dataset_4NA_EXP_PRI_KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_PRI_MICE,"Dataset\\dataset_4NA_EXP_PRI_MICE.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_POST_KNN,"Dataset\\dataset_4NA_EXP_POST_KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_POST_MICE,"Dataset\\dataset_4NA_EXP_POST_MICE.csv", row.names = FALSE)
write.csv(dataset_5NA_NOEXP_PRI_KNN,"Dataset\\dataset_5NA_NOEXP_PRI_KNN.csv", row.names = FALSE)
write.csv(dataset_5NA_NOEXP_PRI_MICE,"Dataset\\dataset_5NA_NOEXP_PRI_MICE.csv", row.names = FALSE)
write.csv(dataset_5NA_NOEXP_POST_KNN,"Dataset\\dataset_5NA_NOEXP_POST_KNN.csv", row.names = FALSE)
write.csv(dataset_5NA_NOEXP_POST_MICE,"Dataset\\dataset_5NA_NOEXP_POST_MICE.csv", row.names = FALSE)
write.csv(dataset_5NA_EXP_PRI_KNN,"Dataset\\dataset_5NA_EXP_PRI_KNN.csv", row.names = FALSE)
write.csv(dataset_5NA_EXP_PRI_MICE,"Dataset\\dataset_5NA_EXP_PRI_MICE.csv", row.names = FALSE)
write.csv(dataset_5NA_EXP_POST_KNN,"Dataset\\dataset_5NA_EXP_POST_KNN.csv", row.names = FALSE)
write.csv(dataset_5NA_EXP_POST_MICE,"Dataset\\dataset_5NA_EXP_POST_MICE.csv", row.names = FALSE)



################################################
############## 2.5 Best Datasets ###############
################################################

# After testing some models we decided to focus on the following datasets:
# dataset_4NA_EXP_POST_KNN
# dataset_4NA_EXP_POST_MICE

write.csv(dataset_4NA_EXP_POST_KNN,"Dataset\\dataset_1-KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_POST_MICE,"Dataset\\dataset_2-MICE.csv", row.names = FALSE)
