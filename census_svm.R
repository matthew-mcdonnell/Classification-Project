# Load packages
library(class)
library(psych)
library(e1071)
library(caret)

# Read in data
census_all <- read.csv("census_all.csv", strip.white = TRUE)

# Remove capital gain/loss, fnlwgt, education and native country columns
census_all <- census_all[,-c(3,4,11,12,14)]

# Remove entries with '?' i.e. unknown values
census_all[census_all == "?"] <- NA
census_all <- na.omit(census_all)

# Make categories binary - <=50k -> 0 and >50k -> 1 and separate
census_all[census_all == "<=50K" | census_all == "<=50K."] <- 0
census_all[census_all == ">50K" | census_all == ">50K."] <- 1
census_all$category = as.factor(census_all$category)

# Scale continuous variables
census_all[, c("age", "education.num", "hours.per.week")] <- 
  scale(census_all[, c("age", "education.num", "hours.per.week")])

# Dummy code categorical variables
# 2-level factors
census_all$sex <- as.factor(census_all$sex)
census_all$sex <- dummy.code(census_all$sex)

# >2-level factors
# Create dummy variables
census_all$workclass <- as.factor(census_all$workclass)
workclass <- as.data.frame(dummy.code(census_all$workclass))

census_all$marital.status <- as.factor(census_all$marital.status)
marital.status <- as.data.frame(dummy.code(census_all$marital.status))

census_all$occupation <- as.factor(census_all$occupation)
occupation <- as.data.frame(dummy.code(census_all$occupation))

census_all$relationship <- as.factor(census_all$relationship)
relationship <- as.data.frame(dummy.code(census_all$relationship))

census_all$race <- as.factor(census_all$race)
race <- as.data.frame(dummy.code(census_all$race))

# Recombine and remove originals
census_all <- cbind(census_all, marital.status, occupation, race, relationship, workclass)
census_all <- subset(census_all, select = -c(marital.status, occupation, race, 
                                             relationship, workclass))

# SUPPORT VECTOR MACHINES
set.seed(12345)
census_train_control <- trainControl(method="cv", number=10)
census_svm <- train(category~., data = census_all, trControl=census_train_control, 
                   method="svmLinear")

svm_err_rate <- 1 - census_svm$results[1,2]
svm_kappa <- census_svm$results[1,3]
svm_err_vec <- 1 - census_svm$resample[, 1]
svm_err_se <- sd(svm_err_vec)/sqrt(length(svm_err_vec))
# Error Rate - 0.1744398
# SE - 0.001698546
# Kappa - 0.5077795

