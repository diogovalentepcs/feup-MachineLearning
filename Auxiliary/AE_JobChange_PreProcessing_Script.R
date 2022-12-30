#Work Directory
setwd("C:/Users/filip/OneDrive/Ambiente de Trabalho/AE_Work")

#Importing Dataset
dataset <- read.csv(file = "C:\\Users\\filip\\Downloads\\Job_change\\data.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))

#Altering the original dataset 
dataset$city <- gsub("city_", "", dataset$city)
dataset$city <- as.factor(as.integer(dataset$city))
dataset$company_size[dataset$company_size=="10/49"]<-"10-49"
dataset$major_discipline[is.na(dataset$major_discipline) & dataset$education_level == 'Primary School'] <- "No Major" 
dataset$major_discipline[is.na(dataset$major_discipline) & dataset$education_level == 'High School'] <- "No Major" 


# Are there missing values in the dataset?
library(naniar)
any_na(dataset)

# How many?
n_miss(dataset)
prop_miss(dataset)

# Which variables are affected?
dataset %>% is.na() %>% colSums()

# Get number of missings per variable (n and %)
miss_var_summary(dataset)
miss_var_table(dataset)

# Get number of missings per participant (n and %)
miss_case_summary(dataset)
miss_case_table(dataset)

# Which variables contain the most missing variables?
gg_miss_var(dataset)

# Where are missings located?
library(caret)
vis_miss(dataset) + theme(axis.text.x = element_text(angle=80))

# Which combinations of variables occur to be missing together?
gg_miss_upset(dataset)

#Removing observations with no logical sense (44 observations)
dataset_test <- dataset
dataset_test <- dataset_test[!(dataset_test$enrolled_university == 'Part time course' & dataset_test$education_level == 'Primary School'),]
dataset_test <- dataset_test[!(dataset_test$enrolled_university == 'Full time course' & dataset_test$education_level == 'Primary School'),]

#Encoding Categorical Variables

#Binary
dataset_test$relevent_experience = factor(dataset_test$relevent_experience,
                                          levels = c("No relevent experience", "Has relevent experience"),
                                          labels = c("N", "Y"))

dataset_test$target = factor(dataset_test$target,
                             levels = c(0,1))

#Nominal
dataset_test$gender = factor(dataset_test$gender,
                             levels = c("Other", "Male", "Female"),
                             labels = c("Other", "M", "F"))

dataset_test$company_type = factor(dataset_test$company_type,
                                   levels = c("Funded Startup", "Early Stage Startup", "Pvt Ltd", "Public Sector", "NGO", "Other"))


dataset_test$major_discipline = factor(dataset_test$major_discipline,
                                       levels = c("No Major", "Arts", "Humanities", "Business Degree", "STEM", "Other"),
                                       labels = c("None", "Arts", "Humanities", "Business", "STEM", "Other"))


dataset_test$enrolled_university = factor(dataset_test$enrolled_university,
                                          levels = c("no_enrollment", "Part time course", "Full time course"),
                                          labels = c("None", "Part-Time", "Full-Time"))



#Ordinal
dataset_test$education_level = factor(dataset_test$education_level, ordered = TRUE,
                                      levels = c("Primary School", "High School", "Graduate", "Masters", "Phd"))


dataset_test$company_size = factor(dataset_test$company_size, ordered = TRUE,
                                   levels = c("<10", "10-49", "50-99", "100-500", "500-999", "1000-4999", "5000-9999", "10000+"))


dataset_test$experience = factor(dataset_test$experience, ordered = TRUE,
                                 levels = c("<1", 1:20, ">20"))




dataset_test$last_new_job = factor(dataset_test$last_new_job, ordered = TRUE,
                                   levels = c("never", 1:4, ">4"))




#Library Declaration
library(dplyr) #Data Manipulation
library(data.table) #Extension of data.frame

#Checking for imbalanced data through data table and barplots

#city (too many levels) -> the data follows, approximately, a Pareto distribution, since 20% of the levels explain 80% of the frequency
city_data <- setDT(dataset_test)[ , .N, keyby = city][ , "%" := .N, by = c("city", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
city_data <- city_data[order(city_data$N, decreasing = TRUE),]

#gender (imbalanced)
gender_data <- setDT(dataset_test)[ , .N, keyby = gender][ , "%" := .N, by = c("gender", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$gender)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#relevent experience (imbalanced)
relexperience_data <- setDT(dataset_test)[ , .N, keyby = relevent_experience][ , "%" := .N, by = c("relevent_experience", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$relevent_experience)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#enrolled university (imbalanced)
enruniversity_data <- setDT(dataset_test)[ , .N, keyby = enrolled_university][ , "%" := .N, by = c("enrolled_university", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$enrolled_university)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#education level (imbalanced)
edlevel_data <- setDT(dataset_test)[ , .N, keyby = education_level][ , "%" := .N, by = c("education_level", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$education_level)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#major discipline (imbalanced)
majdiscipline_data <- setDT(dataset_test)[ , .N, keyby = major_discipline][ , "%" := .N, by = c("major_discipline", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$major_discipline)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#experience (imbalanced)
experience_data <- setDT(dataset_test)[ , .N, keyby = experience][ , "%" := .N, by = c("experience", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$experience)),
        col = rainbow(2),
        ylim = c(0, 0.2),
        main = "Class Distribution")

#company size (balanced)
compsize_data <- setDT(dataset_test)[ , .N, keyby = company_size][ , "%" := .N, by = c("company_size", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$company_size)),
        col = rainbow(2),
        ylim = c(0, 0.4),
        main = "Class Distribution")

#company type (imbalanced)
comptype_data <- setDT(dataset_test)[ , .N, keyby = company_type][ , "%" := .N, by = c("company_type", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$company_type)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#last new job (slightly imbalanced)
lastjob_data <- setDT(dataset_test)[ , .N, keyby = last_new_job][ , "%" := .N, by = c("last_new_job", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$last_new_job)),
        col = rainbow(2),
        ylim = c(0, 0.6),
        main = "Class Distribution")

#target (imbalanced)
target_data <- setDT(dataset_test)[ , .N, keyby = target][ , "%" := .N, by = c("target", "N")][, "%" := round(N/(nrow(dataset_test)) * 100, 2)][]
barplot(prop.table(table(dataset_test$target)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

                                                        #OUTLIER DETECTION

# Verify if there's outliers in the numeric features

#TRAINING HOURS

#Checking data symmetry 
hist(dataset_test$training_hours,
     xlab = "Training Hours",
     main = "Histogram of Training Hours",
     breaks = 30
) # set number of bins

# calculate skewness in r
install.packages("moments") #contains the skewness function 
library(moments)

skewness_th <- skewness(dataset_test$training_hours)

#One of the limitations of using the standard boxplot outlier rule is that it's not appropriated for highly asymmetric data
#In this variable, the data observed is highly asymmetric (skewness value greater than 1). That means we should use a adjusted boxplot to verify the existence of outliers
install.packages("robustbase")
library(robustbase)

adjbox(x = dataset_test$training_hours, data = dataset_test) #NO OUTLIERS


#CITY DEVELOPMENT INDEX

hist(dataset_test$city_development_index,
     xlab = "City Development Index",
     main = "Histogram of City Development Index",
     breaks = 20
) # set number of bins

skewness_cdi <- skewness(dataset_test$city_development_index)

#Since the skewness value is close to -1, we can say that the data observed is also highly asymmetric.
#However, for safety reasons we decide to use both the adjusted box plot and the standard box plot
adjbox(x = dataset_test$city_development_index, data = dataset_test) #TWO OUTLIERS
boxplot(dataset_test$city_development_index, ylab = "City Development Index", main = "City Development Index Boxplot") #ONE OUTLIER

#Since we don't have information that these outliers are due to incorrectly entered or measured data, we shouldn't drop them immediately
#That, along with the contradictory outliers obtained before, led us to ignore these observations.


                                                  #FEATURE SELECTION (PRE TRANSFORMATION)

#Verifying the correlation between the numeric features and the target variable
install.packages("ltm") #Since our target variable is dichotomous, we can use the point-biserial correlation. There is a function to do this in the ltm package
library(ltm)

biserial.cor(dataset_test$city_development_index, dataset_test$target)                 #correlation value = 0.34 -> moderately correlated
biserial.cor(dataset_test$training_hours, dataset_test$target)                         #correlation value = 0.02 -> weakly correlated


# Verify whether each of the categorical features influences the target variable 
# Using the Chi-squared test
#H0: The The two variables are independent
#H1: The two variables are related. 

chisq.test(dataset_test$target, dataset_test$city)                #variables related 
chisq.test(dataset_test$target, dataset_test$gender)              #variables related
chisq.test(dataset_test$target, dataset_test$relevent_experience) #variables related 
chisq.test(dataset_test$target, dataset_test$enrolled_university) #variables related
chisq.test(dataset_test$target, dataset_test$education_level)     #variables related
chisq.test(dataset_test$target, dataset_test$major_discipline)    #variables related
chisq.test(dataset_test$target, dataset_test$experience)          #variables related
chisq.test(dataset_test$target, dataset_test$company_size)        #variables related
chisq.test(dataset_test$target, dataset_test$company_type)        #variables related
chisq.test(dataset_test$target, dataset_test$last_new_job)        #variables related

# Verify correlations between features using:

# Polychoric correlation between ordinal variables
library(polycor)

polychor(dataset_test$education_level, dataset_test$experience)     #[0.31697]     
polychor(dataset_test$education_level, dataset_test$company_size)   #[0.10676]   
polychor(dataset_test$education_level, dataset_test$last_new_job)   #[0.28054]   
polychor(dataset_test$experience, dataset_test$company_size)        #[0.12124] 
polychor(dataset_test$experience, dataset_test$last_new_job)        #[0.51227]
polychor(dataset_test$company_size, dataset_test$last_new_job)      #[0.10943]


# Cramer's V test between nominal variables
install.packages("rcompanion")
library(rcompanion)

cramerV(dataset_test$city, dataset_test$gender)                              #[0.0864]
cramerV(dataset_test$city, dataset_test$relevent_experience)                 #[0.1469]
cramerV(dataset_test$city, dataset_test$enrolled_university)                 #[0.1665]
cramerV(dataset_test$city, dataset_test$major_discipline)                    #[0.1162]
cramerV(dataset_test$city, dataset_test$company_type)                        #[0.1013]
cramerV(dataset_test$gender, dataset_test$relevent_experience)               #[0.0514]
cramerV(dataset_test$gender, dataset_test$enrolled_university)               #[0.0181]
cramerV(dataset_test$gender, dataset_test$major_discipline)                  #[0.0607]
cramerV(dataset_test$gender, dataset_test$company_type)                      #[0.0314]
cramerV(dataset_test$relevent_experience, dataset_test$enrolled_university)  #[0.3824]
cramerV(dataset_test$relevent_experience, dataset_test$major_discipline)     #[0.2964]
cramerV(dataset_test$relevent_experience, dataset_test$company_type)         #[0.1704]
cramerV(dataset_test$enrolled_university, dataset_test$major_discipline)     #[0.1030]
cramerV(dataset_test$enrolled_university, dataset_test$company_type)         #[0.0666]
cramerV(dataset_test$major_discipline, dataset_test$company_type)            #[0.0288]


# Chi-square independence test between different types of variables
chisq.test(dataset_test$city, dataset_test$city_development_index)                 #variables related 
chisq.test(dataset_test$city, dataset_test$education_level)                        #variables related 
chisq.test(dataset_test$city, dataset_test$experience)                             #variables related 
chisq.test(dataset_test$city, dataset_test$company_size)                           #variables related
chisq.test(dataset_test$city, dataset_test$last_new_job)                           #variables related
chisq.test(dataset_test$city_development_index, dataset_test$gender)               #variables related
chisq.test(dataset_test$city_development_index, dataset_test$relevent_experience)  #variables related
chisq.test(dataset_test$city_development_index, dataset_test$enrolled_university)  #variables related
chisq.test(dataset_test$city_development_index, dataset_test$education_level)      #variables related
chisq.test(dataset_test$city_development_index, dataset_test$major_discipline)     #variables related
chisq.test(dataset_test$city_development_index, dataset_test$experience)           #variables related
chisq.test(dataset_test$city_development_index, dataset_test$company_size)         #variables related
chisq.test(dataset_test$city_development_index, dataset_test$company_type)         #variables related
chisq.test(dataset_test$city_development_index, dataset_test$last_new_job)         #variables related
chisq.test(dataset_test$gender, dataset_test$education_level)                      #variables related
chisq.test(dataset_test$gender, dataset_test$experience)                           #variables related
chisq.test(dataset_test$gender, dataset_test$company_size)                         #variables not related
chisq.test(dataset_test$gender, dataset_test$last_new_job)                         #variables related
chisq.test(dataset_test$relevent_experience, dataset_test$education_level)         #variables related
chisq.test(dataset_test$relevent_experience, dataset_test$experience)              #variables related
chisq.test(dataset_test$relevent_experience, dataset_test$company_size)            #variables related
chisq.test(dataset_test$relevent_experience, dataset_test$last_new_job)            #variables related
chisq.test(dataset_test$major_discipline, dataset_test$education_level)            #variables related
chisq.test(dataset_test$major_discipline, dataset_test$experience)                 #variables related
chisq.test(dataset_test$major_discipline, dataset_test$company_size)               #variables related
chisq.test(dataset_test$major_discipline, dataset_test$last_new_job)               #variables related
chisq.test(dataset_test$company_type, dataset_test$education_level)                #variables related
chisq.test(dataset_test$company_type, dataset_test$experience)                     #variables related
chisq.test(dataset_test$company_type, dataset_test$company_size)                   #variables related
chisq.test(dataset_test$company_type, dataset_test$last_new_job)                   #variables related

#All categorical features appear to be related with the target variable and almost all features are related with each other according to the chi-square test
#However, almost all categorical features present in the dataset have an imbalanced data distribution and suffer from high cardinality. This is probably affecting the chi-squared test results.
#Additional transformations are required in our opinion to perform a better feature selection in our models

#NEW DATASET
dataset_changes <- dataset_test

#HOW TO OBTAIN DATASET_4NA_EXP_POST (THIS IS EXPLAINED LATER IN THE SCRIPT)
aux <- apply(dataset_changes, 1, function(x) sum(is.na(x)))
which(aux >= 5)
dataset_4NA_EXP_POST <- dataset_changes[- c(which(aux >= 5)), ]

#HOW TO OBTAIN DATASET_5NA_EXP_POST (THIS IS EXPLAINED LATER IN THE SCRIPT)
aux <- apply(dataset_changes, 1, function(x) sum(is.na(x)))
which(aux >= 6)
dataset_5NA_EXP_POST <- dataset_changes[- c(which(aux >= 6)), ]

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




                                                 #FEATURE SELECTION (POST TRANSFORMATION)
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

["city"]                            ["city_development_index"]           ["gender_M"]                      
["gender_F"]                        ["relevent_experience_N"]            ["relevent_experience_Y"]         
["enrolled_university_None"]        ["enrolled_university_PartTime"]     ["enrolled_university_FullTime"]  
["education_level"]                 ["major_discipline_None"]            ["major_discipline_Humanities"]   
["major_discipline_STEM"]           ["experience"]                       ["company_size"]                  
["company_type_FundedStartup"]      ["company_type_EarlyStageStartup"]   ["company_type_PvtLtd"]           
["company_type_PublicSector"]       ["company_type_NGO"]                 ["company_type_Other"]            
["last_new_job"]

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



                                                       #DATA SAMPLING

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
write.csv(x = dataset_4NA_NOEXP_PRI_KNN, file = "C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_4NA_NOEXP_PRI_KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_NOEXP_PRI_MICE,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_4NA_NOEXP_PRI_MICE.csv", row.names = FALSE)
write.csv(dataset_4NA_NOEXP_POST_KNN,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_4NA_NOEXP_POST_KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_NOEXP_POST_MICE,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_4NA_NOEXP_POST_MICE.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_PRI_KNN,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_4NA_EXP_PRI_KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_PRI_MICE,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_4NA_EXP_PRI_MICE.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_POST_KNN,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_4NA_EXP_POST_KNN.csv", row.names = FALSE)
write.csv(dataset_4NA_EXP_POST_MICE,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_4NA_EXP_POST_MICE.csv", row.names = FALSE)
write.csv(dataset_5NA_NOEXP_PRI_KNN,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_5NA_NOEXP_PRI_KNN.csv", row.names = FALSE)
write.csv(dataset_5NA_NOEXP_PRI_MICE,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_5NA_NOEXP_PRI_MICE.csv", row.names = FALSE)
write.csv(dataset_5NA_NOEXP_POST_KNN,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_5NA_NOEXP_POST_KNN.csv", row.names = FALSE)
write.csv(dataset_5NA_NOEXP_POST_MICE,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_5NA_NOEXP_POST_MICE.csv", row.names = FALSE)
write.csv(dataset_5NA_EXP_PRI_KNN,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_5NA_EXP_PRI_KNN.csv", row.names = FALSE)
write.csv(dataset_5NA_EXP_PRI_MICE,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_5NA_EXP_PRI_MICE.csv", row.names = FALSE)
write.csv(dataset_5NA_EXP_POST_KNN,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_5NA_EXP_POST_KNN.csv", row.names = FALSE)
write.csv(dataset_5NA_EXP_POST_MICE,"C:\\Users\\filip\\OneDrive\\Ambiente de Trabalho\\FACULDADE\\QUARTO ANO\\dataset_5NA_EXP_POST_MICE.csv", row.names = FALSE)
