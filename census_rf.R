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


# Random Forests
set.seed(12345)
census_rf_control <- trainControl(method="cv", number=10, returnResamp = "all")
census_rf <- train(category~., data = census_all, trControl=census_rf_control, 
                   method="rf", tuneGrid=expand.grid(.mtry = c(2, 3, 6, 10)), 
                   metric="Accuracy")

rf_err_rate <- 1 - max(census_rf$results[,2])
rf_kappa <- census_rf$results[3,3]
rf_err_vec <- 1 - census_rf$resample[census_rf$resample$mtry == 6, 1]
rf_err_se <- sd(rf_err_vec)/sqrt(length(rf_err_vec))
# Error Rate - 0.1620138
# SE - 0.001995271
# Kappa - 0.5364631

# Plot Error Rates over different values of mtry
rf_err_mat <- matrix(0, nrow=10, ncol=4)
mtry_vals <- c(2, 3, 6, 10)
for (i in 1:4) {
  mval <- mtry_vals[i]
  rf_err_mat[, i] <- 1 - census_rf$resample[census_rf$resample$mtry == mval,1]
}
boxplot(rf_err_mat, names=mtry_vals, ylab = "Error Rate")

