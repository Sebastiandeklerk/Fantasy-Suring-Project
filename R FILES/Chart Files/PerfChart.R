# strategy_scores <- data.frame(
#   Strategy = c("Optimal", "Our ML approach", "Previous Event", "Recent Form","2016 Score", "Reoccuring stats", "Current Standings", "Random Guess", "Break Type"),
#   E1 = c(695.94, 439.33, 459.89,459.89, 486.15, 520.63,459.89, 458.28, 472.69),
#   E2 = c(604.32, 444.9, 420.57, 385.4, 423.93, 406.89, 374.9,350.92, 511.76),
#   E3 = c(718.45, 560.59, 476.58, 550.34, 473.35, 612.97, 496.58,460.91, 610.06),
#   E4 = c(668.46, 529.5, 447.77, 319.4, 346.8, 352.07,395.36 ,435, 341.88),
#   E5 = c(709.03, 432.62, 346.85, 274.86, 299.81, 346.85, 341.79,208.5, 253.43),
#   E6 = c(838.28, 611.36, 425.4, 456.98, 555.03, 500.92,464.92 ,395.65, 517.5),
#   E7 = c(630.1, 550.86, 392.57, 446.41, 500.07, 495.94, 407.4,449.38, 512.05),
#   E8 = c(768.72, 545.66, 345.62, 349.02, 407.73, 474.69,512.34 ,450.9, 319.01),
#   E9 = c(734.21, 580.11, 254.13, 260.73, 452.97, 418.73,404.14 ,378.13, 490.61),
#   E10 = c(620.4, 419.93, 514.32, 468.94, 226.71, 471.5, 391.28,326.97, 446.99),
#   E11 = c(587.26, 422.55, 314.41, 416.01, 393.28, 414.44,423.5 ,306.99, 367.24)
# )
library(dplyr)
strategy_scores <- data.frame(
  Strategy = c("Optimal", "ML approach", "Previous Event", "2016 Score", "Reoccuring stats", "Current Standings", "Random Guess", "Break Type"),
  E1 = c(695.94, 439.33, 459.89, 486.15, 520.63, 459.89, 458.28, 472.69),
  E2 = c(604.32, 444.9, 420.57, 423.93, 406.89, 374.9, 350.92, 511.76),
  E3 = c(718.45, 560.59, 476.58, 473.35, 612.97, 496.58, 460.91, 610.06),
  E4 = c(668.46, 529.5, 447.77, 346.8, 352.07, 395.36, 435, 341.88),
  E5 = c(709.03, 432.62, 346.85, 299.81, 346.85, 341.79, 208.5, 253.43),
  E6 = c(838.28, 611.36, 425.4, 555.03, 500.92, 464.92, 395.65, 517.5),
  E7 = c(630.1, 550.86, 392.57, 500.07, 495.94, 407.4, 449.38, 512.05),
  E8 = c(768.72, 545.66, 345.62, 407.73, 474.69, 512.34, 450.9, 319.01),
  E9 = c(734.21, 580.11, 254.13, 452.97, 418.73, 404.14, 378.13, 490.61),
  E10 = c(620.4, 419.93, 514.32, 226.71, 471.5, 391.28, 326.97, 446.99),
  E11 = c(587.26, 422.55, 314.41, 393.28, 414.44, 423.5, 306.99, 367.24)
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
write.table(strategy_comparison, file = "strategy_comparison_excluding_optimal_random2017.txt", sep = "\t", row.names = FALSE, quote = FALSE)

strategy_summary <- strategy_stats[, c("Strategy", "Mean", "Variance", "Std_Dev", "Min", "Max","Total")]

# Write the data to a text file
write.table(strategy_summary, file = "strategy_statistics_2017.txt", sep = "\t", row.names = FALSE, quote = FALSE)

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "bottom")  # Move the legend to the bottom
