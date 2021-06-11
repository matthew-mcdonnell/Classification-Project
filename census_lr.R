# Load packages
library(class)
library(psych)
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

# LOGISTIC REGRESSION
# Use cross-validation with V=10
set.seed(1234)
census_lr_control <- trainControl(method="cv", number=10)
census_lr <- train(category~., data = census_all, trControl=census_lr_control, 
                   method="glm", family="binomial")

lr_err_rate <- 1 - census_lr$results[1,2]
lr_kappa <- census_lr$results[1,3]
lr_err_vec <- 1 - census_lr$resample[, 1]
lr_err_se <- sd(lr_err_vec)/sqrt(length(lr_err_vec))
# Error Rate - 0.1690961
# SE - 0.0022761
# Kappa - 0.5157769

