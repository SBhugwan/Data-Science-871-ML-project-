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

options(repr.plot.width = 15, repr.plot.height = 10)

# Create the violin plot
violin_plot2 <- ggplot(violin2, aes(x = features, y = value, fill = target)) +
  geom_violin(trim = TRUE, scale = "width") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Features", y = "Value") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))

print(violin_plot2)


