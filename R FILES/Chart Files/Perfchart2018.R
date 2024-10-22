strategy_scores <- data.frame(
  Strategy = c("Optimal", "ML approach", "Previous Event", "2017 Score", "Reoccuring stats", "Current Standings", "Break Type", "Random Guess"),
  E1 = c(552.02, 360.31, 276.92, 262.73, 248.99, 276.92,269.42, 312.21),
  E2 = c(648.04, 240.42, 240.13, 355.72, 262.49, 326.51, 333.23,292.65),
  E3 = c(540.51, 332.35, 324.97, 272.96, 235.12, 353.31, 332.59,336.82),
  E4 = c(663.62, 337.52, 316.00, 387.94, 351.78, 420.12, 353.79,323.57),
  E5 = c(553.95, 358.67, 360.1, 310.37, 307.26, 325.16, 262.5,306.89),
  E6 = c(659.05, 480.84, 328.93, 420.47, 491.24, 435.1,550.98 ,358.23),
  E7 = c(578.41, 414.35, 343.95, 434.51, 262.43, 380.33, 368.16,315),
  E8 = c(256.92, 199.83, 194.81, 205.03, 216.54, 182.96,199.83 ,151.91),
  E9 = c(648.51, 336.15, 460.82, 365.33, 315.5, 330.77,436.19 ,372.33),
  E10 = c(528.1, 339.68, 208.04, 271.01, 285.98, 382.78, 226.42,280.03),
  E11 = c(566.91, 418.36, 327.12, 372.62, 402.36, 366.77,425.8 ,240.53)
)
#Summary
strategy_stats <- strategy_scores %>%
  rowwise() %>%
  mutate(
    Mean = round(mean(c_across(E1:E11)), 2),
    Variance = round(var(c_across(E1:E11)), 2),
    Std_Dev = round(sd(c_across(E1:E11)), 2),
    Min = round(min(c_across(E1:E11)), 2),
    Max = round(max(c_across(E1:E11)), 2),
    Total = round(sum(c_across(E1:E11)), 2)
  ) %>%
  ungroup()
# Extract mean and standard deviation for Optimal and Random Guess
optimal_stats <- strategy_stats %>%
  filter(Strategy == "Optimal") %>%
  select(Mean, Std_Dev)

random_guess_stats <- strategy_stats %>%
  filter(Strategy == "Random Guess") %>%
  select(Mean, Std_Dev)

# Add columns comparing with Optimal and Random Guess
strategy_comparison <- strategy_stats %>%
  filter(Strategy != "Optimal" & Strategy != "Random Guess") %>%
  mutate(
    Mean_vs_Optimal = round(Mean - optimal_stats$Mean, 2),
    Std_Dev_vs_Optimal = round(Std_Dev - optimal_stats$Std_Dev, 2),
    Mean_vs_RandomGuess = round(Mean - random_guess_stats$Mean, 2),
    Std_Dev_vs_RandomGuess = round(Std_Dev - random_guess_stats$Std_Dev, 2)
  ) %>%
  select(Strategy, Mean, Std_Dev, Mean_vs_Optimal, Std_Dev_vs_Optimal, Mean_vs_RandomGuess, Std_Dev_vs_RandomGuess)

# Write the final table to a text file
write.table(strategy_comparison, file = "strategy_comparison_excluding_optimal_random.txt", sep = "\t", row.names = FALSE, quote = FALSE)
strategy_summary <- strategy_stats[, c("Strategy", "Mean", "Variance", "Std_Dev", "Min", "Max","Total")]

# Write the data to a text file
write.table(strategy_summary, file = "strategy_statistics_2018.txt", sep = "\t", row.names = FALSE, quote = FALSE)

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
  labs(x = "Event", y = "Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  theme(legend.position = "bottom")  # Move the legend to the bottom



strategy_totals <- strategy_scores
strategy_totals$Total <- rowSums(strategy_scores[, 2:12])

strategy_totals_sorted <- strategy_totals[order(-strategy_totals$Total), ]

# Print the sorted results showing Strategy and Total
print(strategy_totals_sorted[, c("Strategy", "Total")])

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
  labs(x = "Event", y = "Accumulated Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  theme(legend.position = "bottom")  # Move the legend to the bottom


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
  labs(x = "Event", y = "Accumulated Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  theme(legend.position = "bottom")  # Move the legend to the bottom
