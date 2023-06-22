##Feature Extraction
#Extracted some features for predicting match result

# Calculate rank_difference
match_df$rank_difference <- match_df$home_team_fifa_rank - match_df$away_team_fifa_rank

# Calculate average_rank
match_df$average_rank <- (match_df$home_team_fifa_rank + match_df$away_team_fifa_rank) / 2

# Calculate point_difference
match_df$point_difference <- match_df$home_team_total_fifa_points - match_df$away_team_total_fifa_points

# Create is_stake column
match_df$is_stake <- match_df$tournament != "Friendly"

# Create is_worldcup column
match_df$is_worldcup <- grepl("FIFA World Cup", match_df$tournament)

# Calculate score_difference
match_df$score_difference <- match_df$home_team_score - match_df$away_team_score

# Create is_won column
match_df$is_won <- match_df$score_difference > 0


##Training

library(caret)
library(glmnet)
library(randomForest)
library(kernlab)
library(e1071)
library(naivebayes)
library(kknn)
library(rpart)

# Load necessary libraries

# Machine Learning
library(glmnet)
library(randomForest)
library(kernlab)
library(e1071)
library(kknn)
library(rpart)
library(class)

# Splitting the data into X and y (look at notes)
X <- match_df[, c('average_rank', 'rank_difference', 'point_difference', 'is_stake', 'is_worldcup')]
y <- match_df$is_won

# Splitting the data into train and test sets
set.seed(42)  # Setting the random seed for reproducibility
train_indices <- caret::createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]


##trying different machine learning Techniques (look at notes to explain better)

# Logistic Regression
logreg <- glm(y_train ~ ., family = binomial(), data = cbind(X_train, y_train))
lg_pred <- predict(logreg, newdata = X_test, type = "response") > 0.5
acc_log <- round(mean(lg_pred == y_test) * 100, 2)
acc_log



# Support Vector Machines
y_train <- as.factor(y_train)  # Convert y_train to a factor

svm_model <- svm(y_train ~ ., data = cbind(X_train, y_train))
svm_pred <- predict(svm_model, newdata = X_test)
acc_svc <- round(mean(svm_pred == y_test) * 100, 2)
acc_svc


# K-Nearest Neighbors
knn_model <- knn(train = X_train, test = X_test, cl = y_train, k = 3)
acc_knn <- round(mean(knn_model == y_test) * 100, 2)
acc_knn

# Gaussian Naive Bayes
gnb_model <- naiveBayes(X_train, y_train)
gnb_pred <- predict(gnb_model, newdata = X_test)
acc_gaussian <- round(mean(gnb_pred == y_test) * 100, 2)
acc_gaussian

# Decision Tree
dt_model <- rpart(y_train ~ ., data = cbind(X_train, y_train))
dt_pred <- predict(dt_model, newdata = X_test, type = "class")
acc_decision_tree <- round(mean(dt_pred == y_test) * 100, 2)
acc_decision_tree



# Random Forest
rf_model <- randomForest(y_train ~ ., data = cbind(X_train, y_train), ntree = 100)
rf_pred <- predict(rf_model, newdata = X_test)
acc_random_forest <- round(mean(rf_pred == y_test) * 100, 2)
acc_random_forest

##Create a data frame with model names and scores
models <- data.frame(
    Model = c("Support Vector Machines", "KNN", "Logistic Regression",
              "Random Forest", "Naive Bayes", "Decision Tree"),
    Score = c(acc_svc, acc_knn, acc_log,
              acc_random_forest, acc_gaussian, acc_decision_tree)
)

# Sort the data frame by score in descending order
models <- models[order(-models$Score), ]

#models

model <- logreg

plot_model_accuracy <- function(models) {
    # Sort the data frame by score in descending order
    models <- models[order(-models$Score), ]

    # Set a custom color palette for the bar chart
    bar_colors <- c("#5E81AC", "#8FBCBB", "#A3BE8C", "#EBCB8B", "#BF616A", "#B48EAD")

    # Calculate the maximum score
    max_score <- max(models$Score)

    # Create a bar chart to compare the accuracy scores
    barplot(models$Score, names.arg = models$Model,
            ylab = "Accuracy Score",
            main = "Comparison of Model Accuracy",
            col = bar_colors, border = "black", ylim = c(0, max_score * 1.1),
            cex.names = 0.8, las = 2)

    # Add text labels above each bar
    text(x = 1:length(models$Model), y = models$Score, labels = paste0(models$Score, "%"),
         pos = 3, cex = 0.8, col = "black")

    # Add a horizontal line for the average accuracy
    avg_score <- mean(models$Score)
    abline(h = avg_score, col = "red", lwd = 2)
    text(x = length(models$Model) + 0.8, y = avg_score, labels = paste0("Avg: ", round(avg_score, 2), "%"),
         pos = 2, cex = 0.8, col = "red")


}

#plot_model_accuracy(models)




#CROSS_VALIDATION for Random forest
# Load necessary libraries
library(caret)
library(randomForest)

# Define the training control for cross-validation
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the random forest model using cross-validation
rf_model_cv <- train(y_train ~ ., data = cbind(X_train, y_train),
                     method = "rf", trControl = ctrl, ntree = 100)

# Print the results of cross-validation
print(rf_model_cv)

# Access the accuracy and other performance metrics
accuracy_cv <- rf_model_cv$results$Accuracy
other_metrics_cv <- rf_model_cv$results  # Other metrics can be accessed similarly

# Assess the bias and variance
bias <- mean(accuracy_cv) - acc_random_forest
variance <- var(accuracy_cv)

# Print the bias and variance
cat("Bias:", bias, "\n")
cat("Variance:", variance, "\n")


##TRYING TO GET GRAPH

library(caret)
library(ggplot2)

# Define the range of number of trees to explore
ntrees <- seq(10, 200, by = 10)

# Initialize vectors to store accuracy and complexity values
accuracy <- rep(0, length(ntrees))
complexity <- rep(0, length(ntrees))

# Perform cross-validation for different numbers of trees
for (i in 1:length(ntrees)) {
    rf_model_cv <- train(y_train ~ ., data = cbind(X_train, y_train),
                         method = "rf", trControl = ctrl, ntree = ntrees[i])
    accuracy[i] <- rf_model_cv$results$Accuracy
    complexity[i] <- ntrees[i]
}

# Create a data frame with accuracy and complexity values
df <- data.frame(Accuracy = accuracy, Complexity = complexity)

# Create a line plot to visualize the bias-variance trade-off
BVTO<-ggplot(df, aes(x = Complexity, y = Accuracy)) +
    geom_line() +
    geom_point() +
    labs(x = "Number of Trees", y = "Accuracy") +
    ggtitle("Bias-Variance Trade-off") +
    theme_minimal()





## cross-validation for logistic regression

library(caret)

# Create a train control object for cross-validation
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the logistic regression model using cross-validation
logreg_model_cv <- train(y_train ~ ., data = cbind(X_train, y_train),
                         method = "glm", family = binomial(), trControl = ctrl)

# Print the results of cross-validation
print(logreg_model_cv)

# Access the accuracy and other performance metrics
accuracy_cv <- logreg_model_cv$results$Accuracy
other_metrics_cv <- logreg_model_cv$results  # Other metrics can be accessed similarly

# Assess the bias and variance
bias <- mean(accuracy_cv) - acc_log
variance <- var(accuracy_cv)

# Print the bias and variance
cat("Bias:", bias, "\n")
cat("Variance:", variance, "\n")


library(pROC)

# Logistic Regression ROC curve
logreg <- glm(y_train ~ ., family = binomial(), data = cbind(X_train, y_train))
lg_pred <- predict(logreg, newdata = X_test, type = "response")

# Convert y_test to a binary factor
y_test <- factor(y_test, levels = c(0, 1), labels = c("control", "case"))

# Calculate the ROC curve
#roc_obj <- roc(y_test, lg_pred)

# Plot the ROC curve
#plot(roc_obj, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")

#print(roc_obj) #check notes for meaning

