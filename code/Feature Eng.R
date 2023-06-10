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



