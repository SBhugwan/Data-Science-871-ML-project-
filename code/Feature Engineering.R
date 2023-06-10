# This code is for possible features that can have an impact on predicting the matches
# Past game points made
#Past goals scored and suffered
#The importance of game (friendly or not)
#Rank of the teams
#Rank increasement of the teams
#Goals made and suffered by ranking faced


##first thing to do is create the feature that says which team won and how much points they made at game.

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

df <- cbind(df, t(sapply(results, unlist)))
colnames(df)[ncol(df)-2:ncol(df)] <- c("result", "home_team_points", "away_team_points")


print(df)



