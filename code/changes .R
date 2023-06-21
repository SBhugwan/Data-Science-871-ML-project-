match_df<-read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/Matches .csv")
colnames(match_df)
rank_df<-read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/fifa_ranking-2022-12-22.csv")
fifa2022WC <-read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/matchs-schudule.csv")
Q2022TG<- read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/Qatar2022-teams.csv")


#countries have different names, standardize
match_df$home_team <- ifelse(match_df$home_team == "IR Iran", "Iran", match_df$home_team)
match_df$home_team <- ifelse(match_df$home_team == "Korea Republic", "South Korea", match_df$home_team)
match_df$away_team <- ifelse(match_df$away_team == "IR Iran", "Iran", match_df$away_team)
match_df$away_team <- ifelse(match_df$away_team == "Korea Republic", "South Korea", match_df$away_team)
rank_df$country_full <- ifelse(rank_df$country_full == "IR Iran", "Iran", rank_df$country_full)
rank_df$country_full <- ifelse(rank_df$country_full == "Korea Republic", "South Korea", rank_df$country_full)



##Feature Extraction
#some features for predicting match result

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


#training

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



#CROSS_VALIDATION
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







##Create a data frame with model names and scores
models <- data.frame(
    Model = c("Support Vector Machines", "KNN", "Logistic Regression",
              "Random Forest", "Naive Bayes", "Decision Tree"),
    Score = c(acc_svc, acc_knn, acc_log,
              acc_random_forest, acc_gaussian, acc_decision_tree)
)

# Sort the data frame by score in descending order
models <- models[order(-models$Score), ]

models

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



##Prediction

# Let's define a small margin when we're safer to predict draw than win
Q2022TG <- separate(Q2022TG, col = `Team;Group`, into = c("Team", "Group"), sep = ";")

# Let's define a small margin when we're safer to predict draw than win
margin <- 0.05

# Let's define the rankings at the time of the World Cup
rank_df <- rank_df[rank_df$rank_date == max(rank_df$rank_date) & rank_df$country_full %in% Q2022TG$Team, ]
colnames(rank_df)

worldcup_rankings <- rank_df[, c("country_full", "rank", "country_abrv", "total_points", "previous_points", "rank_change", "confederation", "rank_date")]
#row.names(worldcup_rankings) <- worldcup_rankings$country_full

opponents <- c('First match \nagainst', 'Second match\n against', 'Third match\n against')

world_cup <- Q2022TG
world_cup$points <- 0
world_cup$total_prob <- 0
world_cup$expected_points <- 0
world_cup$rank <- NA

country_win_prob <- list()

for (country in unique(Q2022TG$Team)) {
    country_win_prob[[country]] <- list()
}

#Group Stage Predication

for (group in unique(Q2022TG$Group)) {
    cat(paste('___Starting group', group, ':___\n', sep=''))

    group_teams <- Q2022TG$Team[Q2022TG$Group == group]

    # Check if there are at least two non-NA values in group_teams
    if (length(na.omit(group_teams)) < 2) {
        cat("Insufficient teams in the group.\n")
        next
    }

    team_combinations <- combn(group_teams, 2)

    for (i in 1:ncol(team_combinations)) {
        home <- team_combinations[1, i]
        away <- team_combinations[2, i]
        cat(paste(home, 'vs.', away, ': ', sep=''))

        home_rank <- worldcup_rankings[match(home, worldcup_rankings$country_full), 'rank']
        home_points <- worldcup_rankings[match(home, worldcup_rankings$country_full), 'total_points']
        opp_rank <- worldcup_rankings[match(away, worldcup_rankings$country_full), 'rank']
        opp_points <- worldcup_rankings[match(away, worldcup_rankings$country_full), 'total_points']

        world_cup[match(home, world_cup$Team), 'rank'] <- home_rank
        world_cup[match(away, world_cup$Team), 'rank'] <- opp_rank

        average_rank <- (home_rank + opp_rank) / 2
        rank_difference <- home_rank - opp_rank
        point_difference <- home_points - opp_points

        # Convert average_rank, rank_difference, and point_difference to numeric
        average_rank <- as.numeric(average_rank)
        rank_difference <- as.numeric(rank_difference)
        point_difference <- as.numeric(point_difference)

        # Create a data frame for each match
        row <- data.frame(average_rank = average_rank, rank_difference = rank_difference, point_difference = point_difference)

        # Model Output
        home_win_prob <- predict(model, row)[, 2]

        # Saving Model Output
        world_cup[match(home, world_cup$Team), 'total_prob'] <- world_cup[match(home, world_cup$Team), 'total_prob'] + home_win_prob
        world_cup[match(away, world_cup$Team), 'total_prob'] <- world_cup[match(away, world_cup$Team), 'total_prob'] + 1 - home_win_prob

        country_win_prob[[home]] <- c(country_win_prob[[home]], home_win_prob)
        country_win_prob[[away]] <- c(country_win_prob[[away]], 1 - home_win_prob)

        # Determining Win / Draw / Lose based on home_win_prob
        points <- 0
        if (home_win_prob <= 0.5 - margin) {
            cat(paste(away, 'wins with', format(1 - home_win_prob, digits=2)), '\n')
            world_cup[match(away, world_cup$Team), 'points'] <- world_cup[match(away, world_cup$Team), 'points'] + 3
            world_cup[match(away, world_cup$Team), 'expected_points'] <- world_cup[match(away, world_cup$Team), 'expected_points'] + (1 - home_win_prob) * 3
        } else if (home_win_prob > 0.5 - margin && home_win_prob < 0.5 + margin) {
            cat('Draw\n')
            world_cup[match(home, world_cup$Team), 'points'] <- world_cup[match(home, world_cup$Team), 'points'] + 1
            world_cup[match(away, world_cup$Team), 'points'] <- world_cup[match(away, world_cup$Team), 'points'] + 1
            world_cup[match(home, world_cup$Team), 'expected_points'] <- world_cup[match(home, world_cup$Team), 'expected_points'] + home_win_prob * 1
            world_cup[match(away, world_cup$Team), 'expected_points'] <- world_cup[match(away, world_cup$Team), 'expected_points'] + (1 - home_win_prob) * 1
        } else {
            cat(paste(home, 'wins with', format(home_win_prob, digits=2)), '\n')
            world_cup[match(home, world_cup$Team), 'points'] <- world_cup[match(home, world_cup$Team), 'points'] + 3
            world_cup[match(home, world_cup$Team), 'expected_points'] <- world_cup[match(home, world_cup$Team), 'expected_points'] + home_win_prob * 3
        }
    }
}


#Expected points
unique_groups <- unique(Q2022TG$Group)
for (group in unique_groups) {
    cat(paste('___Starting group', group, '___\n'))
    teams <- Q2022TG[Q2022TG$Group == group, ]
    for (i in 1:nrow(teams)) {
        team_name <- teams[i, 1]
        expected_points <- world_cup[team_name, "expected points"]
        cat(paste(team_name, ": ", expected_points, "\n"))
    }
}


#Group stage survival prob

country_total_prob <- list()

for (country in names(country_win_prob)) {
    win_prob_list <- country_win_prob[[country]]

    total_prob <- 0
    temp <- 1
    for (i in 1:3) {
        temp <- temp * win_prob_list[i]
    }
    total_prob <- total_prob + temp

    for (i in 1:3) {
        temp <- 1
        for (j in 1:3) {
            if (i == j) {
                temp <- temp * (1 - win_prob_list[i])
            } else {
                temp <- temp * win_prob_list[i]
            }
        }
        total_prob <- total_prob + temp
    }

    country_total_prob[[country]] <- total_prob

    cat(paste(country, ":", total_prob, "\n"))
}


country_total_prob <- sort(country_total_prob, decreasing = TRUE, key = function(x) x[[2]])
country_total_prob





data <- list(
    c('Netherlands', 0.7207123136586894),
    c('Belgium', 0.7041053684174787),
    c('Portugal', 0.7025008859381351),
    c('France', 0.6893043301262857),
    c('Brazil', 0.6878946534259283),
    c('Spain', 0.6419635779920858),
    c('Argentina', 0.6086463678095058),
    c('Mexico', 0.5868558522665447),
    c('England', 0.5691708021782123),
    c('Serbia', 0.5383228703970194),
    c('Wales', 0.5323556070273117),
    c('Uruguay', 0.5313857116348),
    c('Senegal', 0.5238123555510896),
    c('Croatia', 0.5217642297484982),
    c('Denmark', 0.5151762204409138),
    c('Poland', 0.5128567082594242),
    c('USA', 0.5126970678459446),
    c('Germany', 0.4893859548936812),
    c('Switzerland', 0.45510860867744996),
    c('South Korea', 0.45081127452719727),
    c('Costa Rica', 0.43841533837914887),
    c('Japan', 0.4109739077103775),
    c('Ecuador', 0.4107822279220528),
    c('Australia', 0.3877831877546305),
    c('Morocco', 0.3819955705681707),
    c('Tunisia', 0.3721040502951258),
    c('Iran', 0.36831897628115345),
    c('Canada', 0.3482241700140098),
    c('Qatar', 0.28657994414763255),
    c('Cameroon', 0.25700333565836253),
    c('Ghana', 0.24790543284071997),
    c('Saudi Arabia', 0.22215428639701668)
)

# Extract country names and probabilities
countries <- sapply(data, "[[", 1)
probabilities <- as.numeric(sapply(data, "[[", 2))

# Create a bar plot
barplot(probabilities, names.arg = countries, ylab = "Probability", col = rainbow(length(data)), las = 2, cex.names = 0.8, ylim = c(0, max(probabilities) * 1.2))
title(main = "Probability of Countries", ylab = "Probability")


