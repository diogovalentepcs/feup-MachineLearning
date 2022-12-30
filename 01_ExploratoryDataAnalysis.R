
################################################
####### 1. Exploratory Data Analysis ###########
################################################

# Goals: Understand the data, its problems and create processing strategy
# Main Outputs: Visualizations (grpahs, plots, numbers) of different data stats (check Visualizations/EDA/ folder)

################################################
##### 1.1 Dataset Summary and Noisy Data #######
################################################

# Importing Dataset
dataset <- read.csv(file = "Job_change\\data.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c("","NA"))
summary(dataset)

# Change City to just city number
dataset$city <- gsub("city_", "", dataset$city)

# Are IDs unique?
length(dataset$enrollee_id) == length(unique(dataset$enrollee_id)) #no duplicates

# Check for random values in each category
for (col in 2:ncol(dataset)){
  print(unique(dataset[,col]))  
}

# Found error in company size type of writing
dataset$company_size[dataset$company_size=="10/49"]<-"10-49"


# Incongruences in education - In university but with education == Primary School) - 44 occasions
education_incongruences = length(dataset[(dataset$enrolled_university == 'Part time course' & dataset$education_level == 'Primary School'),]) + 
  length(dataset[(dataset$enrolled_university == 'Full time course' & dataset$education_level == 'Primary School'),])

# Delete incongruences form model
dataset <- dataset[!(dataset$enrolled_university == 'Part time course' & dataset$education_level == 'Primary School'),]
dataset <- dataset[!(dataset$enrolled_university == 'Full time course' & dataset$education_level == 'Primary School'),]

################################################
############ 1.2 Missing Values ################
################################################

# Are there missing values in the dataset?
any_na(dataset)

# How many?
n_miss(dataset)
prop_miss(dataset)

# Which variables are affected?
dataset %>% is.na() %>% colSums()

# Get number of missings per variable (n and %)
miss_var = miss_var_summary(dataset)
miss_var_table(dataset)

# Get number of missings per participant (n and %)
miss_case = miss_case_summary(dataset)
miss_case_table(dataset)

# Which variables contain the most missing variables?
gg_miss_var(dataset)
dev.print(pdf, 'Visualizations\\EDA_MissingData.pdf') 


# Where are missings located?
vis_miss(dataset) + theme(axis.text.x = element_text(angle=80))
dev.print(pdf, 'Visualizations\\EDA_MissingDataLocation.pdf') 


# Which combinations of variables occur to be missing together?
gg_miss_upset(dataset)
dev.print(pdf, 'Visualizations\\EDA_MissingDataPairs.pdf')

################################################
#### 1.3 Factors for Categorical Variables #####
################################################

#Binary
dataset$relevent_experience = factor(dataset$relevent_experience,
                                     levels = c("No relevent experience", "Has relevent experience"),
                                     labels = c("N", "Y"))

dataset$target = factor(dataset$target,
                        levels = c(0,1))

#Nominal
dataset$gender = factor(dataset$gender,
                        levels = c("Other", "Male", "Female"),
                        labels = c("Other", "M", "F"))

dataset$company_type = factor(dataset$company_type,
                              levels = c("Funded Startup", "Early Stage Startup", "Pvt Ltd", "Public Sector", "NGO", "Other"))


# adding No major to entries withou university
dataset$major_discipline[is.na(dataset$major_discipline) & dataset$education_level == 'Primary School'] <- "No Major" 
dataset$major_discipline[is.na(dataset$major_discipline) & dataset$education_level == 'High School'] <- "No Major" 
dataset$major_discipline = factor(dataset$major_discipline,
                                  levels = c("No Major", "Arts", "Humanities", "Business Degree", "STEM", "Other"),
                                  labels = c("None", "Arts", "Humanities", "Business", "STEM", "Other"))

dataset$enrolled_university = factor(dataset$enrolled_university,
                                     levels = c("no_enrollment", "Part time course", "Full time course"),
                                     labels = c("None", "Part-Time", "Full-Time"))

#Ordinal
dataset$education_level = factor(dataset$education_level, ordered = TRUE,
                                 levels = c("Primary School", "High School", "Graduate", "Masters", "Phd"))

dataset$company_size = factor(dataset$company_size, ordered = TRUE,
                              levels = c("<10", "10-49", "50-99", "100-500", "500-999", "1000-4999", "5000-9999", "10000+"))

dataset$experience = factor(dataset$experience, ordered = TRUE,
                            levels = c("<1", 1:20, ">20"))

dataset$last_new_job = factor(dataset$last_new_job, ordered = TRUE,
                              levels = c("never", 1:4, ">4"))



################################################
############### 1.4 Imbalances #################
################################################

#Checking for imbalanced data through data table and barplots

#city (too many levels) -> the data follows, approximately, a Pareto distribution, since 20% of the levels explain 80% of the frequency
city_data <- setDT(dataset)[ , .N, keyby = city][ , "Perc" := .N, by = c("city", "N")][, "Perc" := round(N/(nrow(dataset)) * 100, 2)][]
city_data <- city_data[order(city_data$N, decreasing = TRUE),]
city_data = data.frame(city_data)
ggplot(city_data, aes(city_data$city, city_data$Perc))+geom_point()

#gender (imbalanced)
gender_data <- setDT(dataset)[ , .N, keyby = gender][ , "%" := .N, by = c("gender", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$gender)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#relevent experience (imbalanced)
relexperience_data <- setDT(dataset)[ , .N, keyby = relevent_experience][ , "%" := .N, by = c("relevent_experience", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$relevent_experience)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#enrolled university (imbalanced)
enruniversity_data <- setDT(dataset)[ , .N, keyby = enrolled_university][ , "%" := .N, by = c("enrolled_university", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$enrolled_university)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#education level (imbalanced)
edlevel_data <- setDT(dataset)[ , .N, keyby = education_level][ , "%" := .N, by = c("education_level", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$education_level)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#major discipline (imbalanced)
majdiscipline_data <- setDT(dataset)[ , .N, keyby = major_discipline][ , "%" := .N, by = c("major_discipline", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$major_discipline)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#experience (imbalanced)
experience_data <- setDT(dataset)[ , .N, keyby = experience][ , "%" := .N, by = c("experience", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$experience)),
        col = rainbow(2),
        ylim = c(0, 0.2),
        main = "Class Distribution")

#company size (balanced)
compsize_data <- setDT(dataset)[ , .N, keyby = company_size][ , "%" := .N, by = c("company_size", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$company_size)),
        col = rainbow(2),
        ylim = c(0, 0.4),
        main = "Class Distribution")

#company type (imbalanced)
comptype_data <- setDT(dataset)[ , .N, keyby = company_type][ , "%" := .N, by = c("company_type", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$company_type)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")

#last new job (slightly imbalanced)
lastjob_data <- setDT(dataset)[ , .N, keyby = last_new_job][ , "%" := .N, by = c("last_new_job", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$last_new_job)),
        col = rainbow(2),
        ylim = c(0, 0.6),
        main = "Class Distribution")

#target (imbalanced)
target_data <- setDT(dataset)[ , .N, keyby = target][ , "%" := .N, by = c("target", "N")][, "%" := round(N/(nrow(dataset)) * 100, 2)][]
barplot(prop.table(table(dataset$target)),
        col = rainbow(2),
        ylim = c(0, 1),
        main = "Class Distribution")
dev.print(pdf, 'Visualizations\\EDA_TargetImbalance.pdf')

################################################
############ 1.5 Outlier Detection #############
################################################

# Verify if there's outliers in the numeric features

#TRAINING HOURS
#Checking data symmetry 
hist(dataset$training_hours,
     xlab = "Training Hours",
     main = "Histogram of Training Hours",
     breaks = 30
) # set number of bins

skewness_th <- skewness(dataset$training_hours)

#One of the limitations of using the standard boxplot outlier rule is that it's not appropriated for highly asymmetric data
#In this variable, the data observed is highly asymmetric (skewness value greater than 1). That means we should use a adjusted boxplot to verify the existence of outliers

adjbox(x = dataset$training_hours, data = dataset) #NO OUTLIERS


#CITY DEVELOPMENT INDEX

hist(dataset$city_development_index,
     xlab = "City Development Index",
     main = "Histogram of City Development Index",
     breaks = 20
) # set number of bins

skewness_cdi <- skewness(dataset$city_development_index)

#Since the skewness value is close to -1, we can say that the data observed is also highly asymmetric.
#However, for safety reasons we decide to use both the adjusted box plot and the standard box plot
adjbox(x = dataset$city_development_index, data = dataset) #TWO OUTLIERS
boxplot(dataset$city_development_index, ylab = "City Development Index", main = "City Development Index Boxplot") #ONE OUTLIER

#Since we don't have information that these outliers are due to incorrectly entered or measured data, we shouldn't drop them immediately
#That, along with the contradictory outliers obtained before, led us to ignore these observations.

################################################
# 1.6 Feature Correlation (Pre-Transformation) #
################################################


#Verifying the correlation between the numeric features and the target variable

biserial.cor(dataset$city_development_index, dataset$target)                 #correlation value = 0.34 -> moderately correlated
biserial.cor(dataset$training_hours, dataset$target)                         #correlation value = 0.02 -> weakly correlated


# Verify whether each of the categorical features influences the target variable 
# Using the Chi-squared test
#H0: The The two variables are independent
#H1: The two variables are related. 

chisq.test(dataset$target, dataset$city)                #variables related 
chisq.test(dataset$target, dataset$gender)              #variables related
chisq.test(dataset$target, dataset$relevent_experience) #variables related 
chisq.test(dataset$target, dataset$enrolled_university) #variables related
chisq.test(dataset$target, dataset$education_level)     #variables related
chisq.test(dataset$target, dataset$major_discipline)    #variables related
chisq.test(dataset$target, dataset$experience)          #variables related
chisq.test(dataset$target, dataset$company_size)        #variables related
chisq.test(dataset$target, dataset$company_type)        #variables related
chisq.test(dataset$target, dataset$last_new_job)        #variables related

# Verify correlations between features using:

polychor(dataset$education_level, dataset$experience)     #[0.31697]     
polychor(dataset$education_level, dataset$company_size)   #[0.10676]   
polychor(dataset$education_level, dataset$last_new_job)   #[0.28054]   
polychor(dataset$experience, dataset$company_size)        #[0.12124] 
polychor(dataset$experience, dataset$last_new_job)        #[0.51227]
polychor(dataset$company_size, dataset$last_new_job)      #[0.10943]


# Cramer's V test between nominal variables

cramerV(dataset$city, dataset$gender)                              #[0.0864]
cramerV(dataset$city, dataset$relevent_experience)                 #[0.1469]
cramerV(dataset$city, dataset$enrolled_university)                 #[0.1665]
cramerV(dataset$city, dataset$major_discipline)                    #[0.1162]
cramerV(dataset$city, dataset$company_type)                        #[0.1013]
cramerV(dataset$gender, dataset$relevent_experience)               #[0.0514]
cramerV(dataset$gender, dataset$enrolled_university)               #[0.0181]
cramerV(dataset$gender, dataset$major_discipline)                  #[0.0607]
cramerV(dataset$gender, dataset$company_type)                      #[0.0314]
cramerV(dataset$relevent_experience, dataset$enrolled_university)  #[0.3824]
cramerV(dataset$relevent_experience, dataset$major_discipline)     #[0.2964]
cramerV(dataset$relevent_experience, dataset$company_type)         #[0.1704]
cramerV(dataset$enrolled_university, dataset$major_discipline)     #[0.1030]
cramerV(dataset$enrolled_university, dataset$company_type)         #[0.0666]
cramerV(dataset$major_discipline, dataset$company_type)            #[0.0288]


# Chi-square independence test between different types of variables
chisq.test(dataset$city, dataset$city_development_index)                 #variables related 
chisq.test(dataset$city, dataset$education_level)                        #variables related 
chisq.test(dataset$city, dataset$experience)                             #variables related 
chisq.test(dataset$city, dataset$company_size)                           #variables related
chisq.test(dataset$city, dataset$last_new_job)                           #variables related
chisq.test(dataset$city_development_index, dataset$gender)               #variables related
chisq.test(dataset$city_development_index, dataset$relevent_experience)  #variables related
chisq.test(dataset$city_development_index, dataset$enrolled_university)  #variables related
chisq.test(dataset$city_development_index, dataset$education_level)      #variables related
chisq.test(dataset$city_development_index, dataset$major_discipline)     #variables related
chisq.test(dataset$city_development_index, dataset$experience)           #variables related
chisq.test(dataset$city_development_index, dataset$company_size)         #variables related
chisq.test(dataset$city_development_index, dataset$company_type)         #variables related
chisq.test(dataset$city_development_index, dataset$last_new_job)         #variables related
chisq.test(dataset$gender, dataset$education_level)                      #variables related
chisq.test(dataset$gender, dataset$experience)                           #variables related
chisq.test(dataset$gender, dataset$company_size)                         #variables not related
chisq.test(dataset$gender, dataset$last_new_job)                         #variables related
chisq.test(dataset$relevent_experience, dataset$education_level)         #variables related
chisq.test(dataset$relevent_experience, dataset$experience)              #variables related
chisq.test(dataset$relevent_experience, dataset$company_size)            #variables related
chisq.test(dataset$relevent_experience, dataset$last_new_job)            #variables related
chisq.test(dataset$major_discipline, dataset$education_level)            #variables related
chisq.test(dataset$major_discipline, dataset$experience)                 #variables related
chisq.test(dataset$major_discipline, dataset$company_size)               #variables related
chisq.test(dataset$major_discipline, dataset$last_new_job)               #variables related
chisq.test(dataset$company_type, dataset$education_level)                #variables related
chisq.test(dataset$company_type, dataset$experience)                     #variables related
chisq.test(dataset$company_type, dataset$company_size)                   #variables related
chisq.test(dataset$company_type, dataset$last_new_job)                   #variables related

#All categorical features appear to be related with the target variable and almost all features are related with each other according to the chi-square test
#However, almost all categorical features present in the dataset have an imbalanced data distribution and suffer from high cardinality. This is probably affecting the chi-squared test results.
#Additional transformations are required in our opinion to perform a better feature selection in our models






################################################
############### Save Dataset ###################
################################################
write.csv(dataset, "Datasets\\dataset_eda.csv")
