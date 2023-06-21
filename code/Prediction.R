
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


