#I am going to analysis the data that spans 150 years, this code is going to analyse how many games played

head(filtered_matches)
tail(filtered_matches)
cbind(c("Games","Variables"),dim(filtered_matches ))


# Graph showing all matches played in the different tournaments
options(repr.plot.width = 7, repr.plot.height = 4)

top_7_tournaments <- filtered_matches %>% count(tournament) %>% top_n(7, n) %>% select(-n)
top_7_tournaments <- filtered_matches %>%
    filter(tournament != "Friendly") %>%
    ungroup() %>%
    mutate(
        Year = floor(Year / 4) * 4,
        tournament = ifelse(tournament %in% top_7_tournaments$tournament, tournament, "Other")
    ) %>%
    group_by(tournament)

custom_colors <- c("red", "blue", "green", "yellow", "pink", "purple", "orange")

Annualmatches<- ggplot(
    top_7_tournaments %>% count(Year) %>% filter(!is.na(Year) & !is.na(n) & Year >= 1872 & Year <= 2022),
    aes(x = Year, y = n , fill = reorder(tournament, n, sum))
) +
    geom_area(show.legend = T, color = "White", size = 0.5) +
    scale_fill_manual(values = custom_colors) +  # Use custom colors
    scale_x_continuous(
        limits = c(1872, 2022),
        breaks = seq(1872, 2022, 30),
        labels = seq(1872, 2022, 30)
    ) +
    labs(
        x = "Year",
        y = "Number of Matches"
    ) +
    ggtitle("Annual matches") +
    theme_minimal()

#Given that most of the matches played aren't in in major competitions
top_7_tournaments <- filtered_matches %>%
    filter(tournament != "Friendly") %>%
    ungroup() %>%
    mutate(
        Year = floor(Year / 4) * 4,
        tournament = ifelse(tournament %in% top_7_tournaments$tournament, tournament, "Other")
    ) %>%
    group_by(tournament)

num_colors <- length(unique(top_7_tournaments$tournament))
custom_palette <- brewer.pal(num_colors, "Set1")

MajorM<-ggplot(
    top_7_tournaments %>% filter(!is.na(tournament)) %>% count(tournament),
    aes(x = reorder(tournament, n, sum), y = n, fill = as.factor(n))
) +
    labs(y = "", x = "", fill = "") +
    geom_bar(stat = "identity", pos = "stack", show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = custom_palette) +
    ggtitle("Occasions") +
    theme_minimal()


#Not all matches played for international teams has the same importance (need to update some errors)
matches <- filtered_matches %>% mutate(
    Importance = ifelse(str_detect(tournament, "FIFA World Cup"), 1, NA),
    Importance = ifelse(str_detect(tournament, "UEFA Euro"), 0.9, Importance),
    Importance = ifelse(str_detect(tournament, "Copa América"), 0.5, Importance),
    Importance = ifelse(str_detect(tournament, "African Cup of Nations"), 0.5, Importance),
    Importance = ifelse(str_detect(tournament, "Friendly"), 0.01, Importance),
    Importance = ifelse(str_detect(tournament, "qualification"), Importance * 0.75, Importance)
)

top5competitions <- suppressMessages(
    matches %>%
        group_by(tournament) %>%
        summarise(n = n(), Importance = mean(Importance)) %>%
        arrange(desc(Importance)) %>%
        top_n(5)
)

options(repr.plot.width = 8, repr.plot.height = 4)
IBT<-ggplot(top5competitions, aes(x = n, y = Importance, colour = tournament, size = n)) +
    geom_point() +
    ggtitle("Importance by Tournament") +
    theme_minimal() +
    scale_colour_viridis(discrete = TRUE) +
    guides(size = FALSE) +
    theme(legend.position = "bottom") +
    labs(x = "\nNumber of Games 1872-2022", y = "", colour = "")


# Filter the matches to include only FIFA World Cup finals (maunually adjusted some of the winners)
fifa_finals <- filtered_matches %>%
    filter(str_detect(tournament, "FIFA World Cup") & !str_detect(tournament, "qualification")) %>%
    mutate(doy = as.numeric(format(date, "%j"))) %>%
    group_by(Year) %>%
    arrange(-Year, -doy) %>%
    filter(doy == max(doy)) %>%
    mutate(
        Winner = ifelse(home_score > away_score, home_team, away_team),
        Loser = ifelse(home_score < away_score, home_team, away_team)
    ) %>%
    group_by(Year) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(Year, Winner)

fifa_finals$Winner[4] <- "Italy"
fifa_finals$Winner[7] <- "Brazil"
fifa_finals$Winner[19] <- "Italy"

world_cup_winners <- fifa_finals %>%
    count(Winner, name = "Wins") %>%
    arrange(-Wins)

options(repr.plot.width = 10, repr.plot.height = 6)  # Adjust the plot size if needed

WCW <- ggplot(world_cup_winners, aes(x = reorder(Winner, Wins), y = Wins)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
    labs(x = "Country", y = "Number of World Cup Wins") +
    ggtitle("FIFA World Cup Winners") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(color = "blue", size = 20, face = "bold"),
          axis.text = element_text(color = "darkblue"),
          axis.title = element_text(color = "darkblue", size = 14),
          panel.background = element_rect(fill = "lightgray"))



