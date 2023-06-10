#The idea here is to create possible features that have impact on predicting football games. By intuition, we say that features that impact could be:

#Past game points made
#Past goals scored and suffered
#The importance of game (friendly or not)
#Rank of the teams
#Rank increasement of the teams
#Goals made and suffered by ranking faced


df <- df_wc_ranked
result_finder <- function(home, away) {
    if (home > away) {
        return(c(0, 3, 0))
    } else if (home < away) {
        return(c(1, 0, 3))
    } else {
        return(c(2, 1, 1))
    }
}

#need to fix up

results <- apply(df, 1, function(x) result_finder(x["home_score"], x["away_score"]))
df[c("result", "home_team_points", "away_team_points")] <- results
corr_data <- df[, c("total_points", "rank", "total_points_away", "rank_away"), drop = FALSE]



corr_data[, c("variable", "value")] <- sapply(
    corr_data[, c("variable", "value")],
    as.numeric
)

corr_data <- corr_data[, !duplicated(colnames(corr_data))]

corr_matrix <- matrix(corr_data$value, nrow = length(unique(corr_data$variable)))
corr_matrix <- cor(corr_matrix)
plt <- ggplot(corr_data, aes(variable, variable, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "red", high = "steelblue") +
    labs(x = "Variable", y = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Correlation Heatmap")
print(plt)

#Now, we create columns that will help in the creation of the features: ranking difference, points won at the game vs. team faced rank, and goals difference in the game.

df$rank_dif <- df$rank - df$rank_away
df$sg <- df$home_score - df$away_score
df$points_home_by_rank <- df$home_team_points / df$rank_away
df$points_away_by_rank <- df$away_team_points / df$rank


#In order to create the features, I'll separate the dataset in home team's and away team's dataset, unify them and calculate the past game values.
home_team <- df[, c("date", "home_team", "home_score", "away_score", "rank", "rank_away", "rank_change", "total_points", "result", "rank_dif", "points_home_by_rank", "home_team_points")]
away_team <- df[, c("date", "away_team", "away_score", "home_score", "rank_away", "rank", "rank_change_away", "total_points_away", "result", "rank_dif", "points_away_by_rank", "away_team_points")]

# Modify column names of home_team
colnames(home_team) <- gsub("home_", "", colnames(home_team))
colnames(home_team) <- gsub("_home", "", colnames(home_team))
colnames(home_team) <- gsub("away_", "suf_", colnames(home_team))
colnames(home_team) <- gsub("_away", "_suf", colnames(home_team))

# Modify column names of away_team
colnames(away_team) <- gsub("away_", "", colnames(away_team))
colnames(away_team) <- gsub("_away", "", colnames(away_team))
colnames(away_team) <- gsub("home_", "suf_", colnames(away_team))
colnames(away_team) <- gsub("_home", "_suf", colnames(away_team))

team_stats <- rbind(home_team, away_team)
team_stats_raw <- team_stats



