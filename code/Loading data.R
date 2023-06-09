#loading Libraries
suppressMessages(library("tidyverse"))
suppressMessages(library("viridis"))
suppressMessages(library("gridExtra"))
library("modelr")
suppressMessages(library("broom"))
library("ggrepel")
library("sqldf")
library(RColorBrewer)
library(fastDummies)
library(reshape2)
library(randomForest)
library(caret)
library(gbm)
set.seed(42)


#Data Preparation

#Import all International soccer matches
allmatches <- suppressMessages(read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/results.csv")) %>% mutate(Year=as.numeric(format(date,"%Y")))

# Mutate a new column "Year" by extracting the year from the "date" column
allmatches <- allmatches %>% mutate(Year = as.numeric(format(date, "%Y")))

# Filter the data up until October 31, 2022 excluding the world cup matches
filtered_matches <- allmatches %>% filter(date <= as.Date("2022-10-31"))

#Historical Win/Loose/Draw ratios for teams playing in the world cup
HWLD<- read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/historical_win-loose-draw_ratios_qatar2022_teams.csv")

#Qatar 2022 teams in allocated groups
#Q2022TG<- read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/Qatar2022-teams.csv")

#FIFA world cup rankings
rank<-read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/fifa_ranking-2022-12-22.csv")
# Convert "rank_date" column to datetime
rank$rank_date <- as.Date(rank$rank_date)

# Filter the rank data from August 1, 2018, onwards
rank <- rank[rank$rank_date >= as.Date("2018-08-01"), ]
row.names(rank) <- NULL  # Reset row index

#Some teams in the World Cup have different names in the ranking's dataset. So, it's needed to adjust.
# Replace country names
rank$country_full <- gsub("IR Iran", "Iran", rank$country_full)
rank$country_full <- gsub("Korea Republic", "South Korea", rank$country_full)
rank$country_full <- gsub("USA", "United States", rank$country_full)

#Merge is made to get FIFA games and rankings
# Complete the rank data for all dates and fill missing values
rank <- rank %>%
    group_by(country_full) %>%
    complete(rank_date = seq(min(rank_date), max(rank_date), by = "day")) %>%
    fill(c(rank, rank_change, total_points, previous_points), .direction = "down") %>%
    ungroup()

# Merge data frames based on 'date' and 'home_team'
df_wc_ranked <- sqldf("SELECT m.*, r.total_points, r.previous_points, r.rank, r.rank_change
                      FROM filtered_matches AS m
                      LEFT JOIN rank AS r ON m.date = r.rank_date AND m.home_team = r.country_full")

# Merge data frames based on 'date' and 'away_team'
df_wc_ranked <- sqldf("SELECT df.*, r.total_points AS total_points_away, r.previous_points AS previous_points_away, r.rank AS rank_away, r.rank_change AS rank_change_away
                      FROM df_wc_ranked AS df
                      LEFT JOIN rank AS r ON df.date = r.rank_date AND df.away_team = r.country_full")

#Example using Brazil
tail(df_wc_ranked[df_wc_ranked$home_team == "Brazil" | df_wc_ranked$away_team == "Brazil", ], 10)

# Import 2022 World cup matches
#fifa2022WC <-read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/matchs-schudule.csv")





#Updated
rank_df<-read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/fifa_ranking-2022-12-22.csv")
fifa2022WC <-read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/matchs-schudule.csv")
Q2022TG<- read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/Qatar2022-teams.csv")
matches_df<-read_csv("/Users/sahilbhugwan/Downloads/Data science/Data Science 871 ML project/data/Matches .csv")

match_df$home_team <- ifelse(match_df$home_team == "IR Iran", "Iran", match_df$home_team)
match_df$home_team <- ifelse(match_df$home_team == "Korea Republic", "South Korea", match_df$home_team)
match_df$away_team <- ifelse(match_df$away_team == "IR Iran", "Iran", match_df$away_team)
match_df$away_team <- ifelse(match_df$away_team == "Korea Republic", "South Korea", match_df$away_team)
rank_df$country_full <- ifelse(rank_df$country_full == "IR Iran", "Iran", rank_df$country_full)
rank_df$country_full <- ifelse(rank_df$country_full == "Korea Republic", "South Korea", rank_df$country_full)
