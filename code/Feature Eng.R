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


#database ready to create predictive features. They will be:

#Mean goals of the team in World Cup Cycle.
#Mean goals of the team in last 5 games.
#Mean goals suffered of the team in World Cup Cycle.
#Mean goals suffered of the team in last 5 games.
#Mean FIFA Rank that team faced in World Cup Cycle.
#Mean FIFA Rank that team faced in last 5 games.
#FIFA Points won at the cycle.
#FIFA Points won in last 5 games.
#Mean game points at the Cycle.
#Mean game points at last 5 games.
#Mean game points by rank faced at the Cycle.
#Mean game points by rank faced at last 5 games.

stats_val <- matrix(nrow = nrow(team_stats), ncol = 12)

for (i in 1:nrow(team_stats)) {
    team <- team_stats[i, "team"]
    date <- team_stats[i, "date"]
    past_games <- team_stats[team_stats$team == team & team_stats$date < date, ]
    last5 <- head(past_games[order(past_games$date, decreasing = TRUE), ], 5)

    goals <- mean(past_games$score)
    goals_l5 <- mean(last5$score)

    goals_suf <- mean(past_games$suf_score)
    goals_suf_l5 <- mean(last5$suf_score)

    rank <- mean(past_games$rank_suf)
    rank_l5 <- mean(last5$rank_suf)

    if (nrow(last5) > 0) {
        points <- past_games$total_points[1] - past_games$total_points[nrow(past_games)]
        points_l5 <- last5$total_points[1] - last5$total_points[nrow(last5)]
    } else {
        points <- 0
        points_l5 <- 0
    }

    gp <- mean(past_games$team_points)
    gp_l5 <- mean(last5$team_points)

    gp_rank <- mean(past_games$points_by_rank)
    gp_rank_l5 <- mean(last5$points_by_rank)

    stats_val[i, ] <- c(goals, goals_l5, goals_suf, goals_suf_l5, rank, rank_l5, points, points_l5, gp, gp_l5, gp_rank, gp_rank_l5)
}

stats_cols <- c("goals_mean", "goals_mean_l5", "goals_suf_mean", "goals_suf_mean_l5", "rank_mean", "rank_mean_l5", "points_mean", "points_mean_l5", "game_points_mean", "game_points_mean_l5", "game_points_rank_mean", "game_points_rank_mean_l5")

stats_df <- data.frame(matrix(stats_val, ncol = length(stats_cols), byrow = TRUE))
colnames(stats_df) <- stats_cols

full_df <- cbind(team_stats, stats_df)

home_team_stats <- full_df[1:(nrow(full_df) / 2), ]
away_team_stats <- full_df[((nrow(full_df) / 2) + 1):nrow(full_df), ]

column_names <- names(home_team_stats)
selected_columns <- column_names[(length(column_names) - 11):length(column_names)]

home_team_stats <- home_team_stats[, tail(names(home_team_stats), 12)]
away_team_stats <- away_team_stats[, tail(names(away_team_stats), 12)]

colnames(home_team_stats) <- paste0("home_", colnames(home_team_stats))
colnames(away_team_stats) <- paste0("away_", colnames(away_team_stats))

match_stats <- cbind(home_team_stats, away_team_stats)
full_df <- cbind(df, match_stats)

colnames(full_df)


#Now to quantify game importance

full_df$is_friendly <- ifelse(grepl("Friendly", full_df$tournament), 1, 0)
library(fastDummies)
full_df <- dummy_cols(full_df, select_columns = "is_friendly")
names(full_df)

base_df <- full_df[, c("date", "home_team", "away_team", "rank", "rank_away", "home_score", "away_score", "result", "rank_dif", "rank_change", "rank_change_away",
                       "home_goals_mean", "home_goals_mean_l5", "home_goals_suf_mean", "home_goals_suf_mean_l5", "home_rank_mean", "home_rank_mean_l5",
                       "home_points_mean", "home_points_mean_l5", "away_goals_mean", "away_goals_mean_l5", "away_goals_suf_mean", "away_goals_suf_mean_l5",
                       "away_rank_mean", "away_rank_mean_l5", "away_points_mean", "away_points_mean_l5", "home_game_points_mean", "home_game_points_mean_l5",
                       "home_game_points_rank_mean", "home_game_points_rank_mean_l5", "away_game_points_mean", "away_game_points_mean_l5",
                       "away_game_points_rank_mean", "away_game_points_rank_mean_l5", "is_friendly_0", "is_friendly_1")]

tail(base_df)

colSums(is.na(base_df))
base_df_no_fg <- na.omit(base_df)




