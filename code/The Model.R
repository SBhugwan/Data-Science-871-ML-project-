create_db <- function(df) {
    columns <- c("home_team", "away_team", "target", "rank_dif", "home_goals_mean", "home_rank_mean",
                 "away_goals_mean", "away_rank_mean", "home_rank_mean_l5", "away_rank_mean_l5",
                 "home_goals_suf_mean", "away_goals_suf_mean", "home_goals_mean_l5", "away_goals_mean_l5",
                 "home_goals_suf_mean_l5", "away_goals_suf_mean_l5", "home_game_points_rank_mean",
                 "home_game_points_rank_mean_l5", "away_game_points_rank_mean", "away_game_points_rank_mean_l5",
                 "is_friendly_0", "is_friendly_1")

    base <- df[, columns]
    base$goals_dif <- base$home_goals_mean - base$away_goals_mean
    base$goals_dif_l5 <- base$home_goals_mean_l5 - base$away_goals_mean_l5
    base$goals_suf_dif <- base$home_goals_suf_mean - base$away_goals_suf_mean
    base$goals_suf_dif_l5 <- base$home_goals_suf_mean_l5 - base$away_goals_suf_mean_l5
    base$goals_per_ranking_dif <- (base$home_goals_mean / base$home_rank_mean) - (base$away_goals_mean / base$away_rank_mean)
    base$dif_rank_agst <- base$home_rank_mean - base$away_rank_mean
    base$dif_rank_agst_l5 <- base$home_rank_mean_l5 - base$away_rank_mean_l5
    base$dif_points_rank <- base$home_game_points_rank_mean - base$away_game_points_rank_mean
    base$dif_points_rank_l5 <- base$home_game_points_rank_mean_l5 - base$away_game_points_rank_mean_l5

    model_df <- base[, c("home_team", "away_team", "target", "rank_dif", "goals_dif", "goals_dif_l5",
                         "goals_suf_dif", "goals_suf_dif_l5", "goals_per_ranking_dif", "dif_rank_agst",
                         "dif_rank_agst_l5", "dif_points_rank", "dif_points_rank_l5",
                         "is_friendly_0", "is_friendly_1")]

    return(model_df)
}

model_db <- create_db(df)
print(model_db)
