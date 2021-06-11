install.packages("ggplot2")
library(ggplot2)
theme_set(theme_classic())

# Input data
method <- c("LR", "65-NN", "SVM", "RF")
mean_err <- c(lr_err_rate, knn_err_rate, svm_err_rate, rf_err_rate)
se_err <- c(lr_err_se, knn_err_se, svm_err_se, rf_err_se)
df <- data.frame(mean_err, se_err)

# Plot
plt <- ggplot(df, aes(x = method, y = mean_err, 
                      ymin = mean_err - 2*se_err, ymax = mean_err + 2*se_err))
plt + geom_errorbar(width=0.2) + geom_point(size=1.5) + labs(x = "Classifier", y = "Error Rate")
