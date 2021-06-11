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

# k-nearest neighbor method
# Use cross-validation with V=5
set.seed(123)
census_knn_control <- trainControl(method="cv", number=5, returnResamp = "all")
census_knn <- train(category~., data=census_all, trControl=census_knn_control,
                    method="knn", tuneGrid=expand.grid(k = seq(5, 80, 5)), metric="Accuracy")

knn_err_rate <- 1 - max(census_knn$results[,2])
knn_kappa <- census_knn$results[13,3]
knn_err_vec <- 1 - census_knn$resample[census_knn$resample$k == 65, 1]
knn_err_se <- sd(knn_err_vec)/sqrt(length(knn_err_vec))
# Error Rate - 0.1669019
# SE - 0.001921703
# Kappa - 0.5297454

# Plot of errors across different values of k
krange <- seq(5, 80, 5)
knn_err_mat <- matrix(0, nrow=5, ncol=16)
for (i in krange) {
  knn_err_mat[, i/5] <- 1 - census_knn$resample[census_knn$resample$k == i,1]
}
boxplot(knn_err_mat, names=krange, xlab= "K", ylab = "Error Rate")
