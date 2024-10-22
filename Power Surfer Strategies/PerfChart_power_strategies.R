strategy_scores <- data.frame(
  Strategy = c("Optimal", "Our ML approach", "2016 Event Winner", "Tier 1 Only","Previous Event Winner", "Random Guess"),
  E1 = c(695.94, 439.33, 486.1, 458.28),
  E2 = c(604.32, 444.9, 461.22,350.92),
  E3 = c(718.45, 560.59, 550.58,460.91),
  E4 = c(668.46, 529.5,  340.88,435),
  E5 = c(709.03, 432.62, 251.64,208.5),
  E6 = c(838.28, 611.36,  506.5,395.65),
  E7 = c(630.1, 550.86, 451.3,449.38),
  E8 = c(768.72, 545.66,  466.33,450.9),
  E9 = c(734.21, 580.11,  413.09,378.13),
  E10 = c(620.4, 419.93, 446.99,326.97),
  E11 = c(587.26, 422.55,  344.82,306.99)
)

library(ggplot2)
library(reshape2)

# Reshape the data for plotting
melted_data <- melt(strategy_scores, id.vars = "Strategy", 
                    variable.name = "Event", value.name = "Score")

# Create the line plot with different colors for each strategy
ggplot(melted_data, aes(x = Event, y = Score, color = Strategy, group = Strategy)) +
  geom_line(size = 1.2) +  # Line plot
  geom_point(size = 3) +  # Add points to the lines
  theme_minimal() +  # Clean theme
  labs(title = "", 
       x = "Event", y = "Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  theme(legend.position = "bottom")  # Move the legend to the bottom



strategy_totals <- strategy_scores
strategy_totals$Total <- rowSums(strategy_scores[, 2:12])

print(strategy_totals[, c("Strategy", "Total")])

# Calculate accumulated totals for each strategy across the events
cumulative_scores <- strategy_scores
cumulative_scores[,-1] <- t(apply(strategy_scores[,-1], 1, cumsum))  # Cumulative sum by row

# Reshape the data for plotting
melted_cumulative_data <- melt(cumulative_scores, id.vars = "Strategy", 
                               variable.name = "Event", value.name = "Cumulative Score")

# Create the line plot for cumulative scores
ggplot(melted_cumulative_data, aes(x = Event, y = `Cumulative Score`, color = Strategy, group = Strategy)) +
  geom_line(size = 1.2) +  # Line plot
  geom_point(size = 3) +  # Add points to the lines
  theme_minimal() +  # Clean theme
  labs(title = "", 
       x = "Event", y = "Accumulated Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Filtered Out scores

filtered_strategy_scores <- strategy_scores[strategy_scores$Strategy != "Optimal", ]

# Calculate accumulated totals for each strategy across the events
cumulative_scores1 <- filtered_strategy_scores
cumulative_scores1[,-1] <- t(apply(filtered_strategy_scores[,-1], 1, cumsum))  # Cumulative sum by row

# Reshape the data for plotting
melted_cumulative_data1 <- melt(cumulative_scores1, id.vars = "Strategy", 
                               variable.name = "Event", value.name = "Cumulative Score")

# Create the line plot for cumulative scores
ggplot(melted_cumulative_data1, aes(x = Event, y = `Cumulative Score`, color = Strategy, group = Strategy)) +
  geom_line(size = 1.2) +  # Line plot
  geom_point(size = 3) +  # Add points to the lines
  theme_minimal() +  # Clean theme
  labs(title = "", 
       x = "Event", y = "Accumulated Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
