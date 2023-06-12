#Know we analyze all features that were created and check if they have predictive power

# Create a function to handle the "no_draw" logic
no_draw <- function(x) {
    if (x == 2) {
        return(1)
    } else {
        return(x)
    }
}

# Apply the "no_draw" function to create the target variable
df$target <- sapply(df$result, no_draw)

# Select the columns for data1
data1 <- df[, c(names(df)[9:20], "target")]

# Standardize the data using mean and standard deviation
scaled_data1 <- scale(data1[-nrow(data1), -ncol(data1)])
scaled_data1$target <- data1$target

# Reshape the data using melt function
violin1 <- reshape2::melt(scaled_data1, id.vars = "target", variable.name = "features", value.name = "value")

# Select the columns for data2
data2 <- df[, 21:ncol(df)]

# Standardize the data using mean and standard deviation
scaled_data2 <- scale(data2[-nrow(data2), ])
scaled_data2$target <- data2$target

# Reshape the data using melt function
violin2 <- reshape2::melt(scaled_data2, id.vars = "target", variable.name = "features", value.name = "value")

# Create the violin plot for Home
violin_plot <- ggplot(violin1, aes(x = features, y = value, fill = target)) +
    geom_violin(trim = TRUE, scale = "width", inner = "quart") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Features", y = "Value") +
    scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))

# Set the plot size
options(repr.plot.width = 15, repr.plot.height = 10)

# Display the plot
print(violin_plot)


## Violin plot away


# Create the violin plot
violin_plot2 <- ggplot(violin2, aes(x = features, y = value, fill = target)) +
  geom_violin(trim = TRUE, scale = "width") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Features", y = "Value") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))

options(repr.plot.width = 15, repr.plot.height = 10)

print(violin_plot2)


#we can create features that get the differences between home and away team and analyze if they are good separating the data.

dif <- full_df
dif$goals_dif <- dif$home_goals_mean - dif$away_goals_mean
dif$goals_dif_l5 <- dif$home_goals_mean_l5 - dif$away_goals_mean_l5
dif$goals_suf_dif <- dif$home_goals_suf_mean - dif$away_goals_suf_mean
dif$goals_suf_dif_l5 <- dif$home_goals_suf_mean_l5 - dif$away_goals_suf_mean_l5
dif$goals_made_suf_dif <- dif$home_goals_mean - dif$away_goals_suf_mean
dif$goals_made_suf_dif_l5 <- dif$home_goals_mean_l5 - dif$away_goals_suf_mean_l5
dif$goals_suf_made_dif <- dif$home_goals_suf_mean - dif$away_goals_mean
dif$goals_suf_made_dif_l5 <- dif$home_goals_suf_mean_l5 - dif$away_goals_mean_l5

data_difs <- dif[, -(1:8)]
scaled <- scale(data_difs)
scaled$target <- data2$target
violin3 <- melt(scaled, id.vars = "target", variable.name = "features", value.name = "value")

plt <- ggplot(violin3, aes(x = features, y = value, fill = target)) +
    geom_violin(
        scale = "width",
        trim = FALSE,
        inner = "quart"
    ) +
    geom_boxplot(
        width = 0.1,
        fill = "white",
        outlier.shape = NA
    ) +
    coord_flip() +
    theme_minimal() +
    labs(x = "Features", y = "Value", fill = "Target") +
    theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 90)
    )

print(plt)

#we have 5 features:rank_dif,goals_dif,goals_dif_l5 ,goals_suf_dif,goals_suf_dif_l5

dif$dif_points <- dif$home_game_points_mean - dif$away_game_points_mean
dif$dif_points_l5 <- dif$home_game_points_mean_l5 - dif$away_game_points_mean_l5
dif$dif_points_rank <- dif$home_game_points_rank_mean - dif$away_game_points_rank_mean
dif$dif_points_rank_l5 <- dif$home_game_points_rank_mean_l5 - dif$away_game_points_rank_mean_l5

dif$dif_rank_agst <- dif$home_rank_mean - dif$away_rank_mean
dif$dif_rank_agst_l5 <- dif$home_rank_mean_l5 - dif$away_rank_mean_l5

# now we can calulate goals made and suffered by rank

dif$goals_per_ranking_dif <- (dif$home_goals_mean / dif$home_rank_mean) - (dif$away_goals_mean / dif$away_rank_mean)
dif$goals_per_ranking_suf_dif <- (dif$home_goals_suf_mean / dif$home_rank_mean) - (dif$away_goals_suf_mean / dif$away_rank_mean)
dif$goals_per_ranking_dif_l5 <- (dif$home_goals_mean_l5 / dif$home_rank_mean) - (dif$away_goals_mean_l5 / dif$away_rank_mean)
dif$goals_per_ranking_suf_dif_l5 <- (dif$home_goals_suf_mean_l5 / dif$home_rank_mean) - (dif$away_goals_suf_mean_l5 / dif$away_rank_mean)

plt2 <- ggplot(data=data_difs, aes(x = goals_per_ranking_dif, y = goals_per_ranking_dif_l5)) +
    geom_point() +
    geom_smooth(method = "lm") +
    xlab("goals_per_ranking_dif") +
    ylab("goals_per_ranking_dif_l5") +
    theme_minimal()

print(plt2) # 55

print(data_difs)

