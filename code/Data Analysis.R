#Know we analyze all features that were created and check if they have predictive power

df <- base_df_no_fg

no_draw <- function(x) {
  if (x == 2) {
    return(1)
  } else {
    return(x)
  }
}

df$target <- sapply(df$result, no_draw)

#Violin Box analyze if the features have different distributions
# Scatter plots to analyze correlations

data1 <- df[, c(names(df)[9:20], "target")]
data2 <- df[, 21:ncol(df)]

scaled <- scale(data1[-nrow(data1), -ncol(data1)])
scaled$target <- data1$target
violin1 <- reshape2::melt(scaled, id.vars = "target", variable.name = "features", value.name = "value") #home 


scaled <- scale(data2[-nrow(data2), ])
scaled$target <- data2$target
violin2 <- reshape2::melt(scaled, id.vars = "target", variable.name = "features", value.name = "value") #away 


options(repr.plot.width = 15, repr.plot.height = 10)

# Violin plot home 
violin_plot1 <- ggplot(violin1, aes(x = features, y = value, fill = target)) +
  geom_violin(trim = TRUE, scale = "width") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Features", y = "Value") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))

print(violin_plot1)  # need to fx this 


## Violin plot away 

options(repr.plot.width = 15, repr.plot.height = 10)

# Create the violin plot
violin_plot2 <- ggplot(violin2, aes(x = features, y = value, fill = target)) +
  geom_violin(trim = TRUE, scale = "width") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Features", y = "Value") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))

print(violin_plot2)


