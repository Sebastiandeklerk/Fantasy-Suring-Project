data <- data.frame(
  Entity = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11"),
  `2016 Heats Surfed` = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  `2016 Heat Score` = c(1, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA),
  `Max Heat Score` = c(1, 1, NA, NA, 1, NA, NA, NA, 1, 1, 1),
  `Years Surfing` = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  `2016 Placement` = c(NA, 1, NA, NA, NA, NA, NA, NA, NA, 1, NA),
  `Previous Heat Score` = c(NA, 1, 1, 1, 1, NA, NA, NA, 1, 1, 1),
  `Average Heat Score` = c(NA, NA, 1, NA, 1, NA, NA, 1, 1, NA, 1),
  `Excellent Heats` = c(NA, NA, 1, NA, NA, 1, 1, NA, 1, NA, NA),
  `Previous Heat Average` = c(NA, NA, NA, 1, 1, 1, 1, NA, 1, 1, NA),
  `Previous Average Heat Average` = c(NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, NA),
  `2016 Score` = c(NA, NA, 1, 1, NA, 1, 1, NA, NA, NA, NA),
  `Standings` = c(NA, NA, NA, 1, NA, 1, NA, NA, NA, NA, NA),
  `Goofy Stance` = c(NA, NA, NA, 1, NA, NA, NA, NA, 1, NA, NA),
  `Negative Nationality Advantage` = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA),
  `Positive Nationality Advantage` = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, 1)
)

D2<-rowSums(!is.na(data[-1]))
unique_vars_per_entity <- apply(data, 1, function(row) names(row)[!is.na(row)])
library(ggplot2)

# Plot counts of non-NA values per variable
ggplot(data.frame(Variable = colnames(data[-1]), Count = colSums(!is.na(data[-1]))), aes(x = Variable, y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of Non-NA Variables Across Entities", x = "Variable", y = "Count")

library(ggplot2)
library(dplyr)

# Create a data frame with counts of non-NA values per variable
variable_counts <- data.frame(
  Variable = colnames(data[-1]),
  Count = colSums(!is.na(data[-1]))
)

# Arrange the data frame by Count in ascending order
variable_counts <- variable_counts %>%
  arrange(desc(Count))

# Plot the data in ascending order
ggplot(variable_counts, aes(x = reorder(Variable, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count Variables Across Entities (Descending Order)", x = "Variable", y = "Count")

