# Load the dataset
airline <- read.csv("Invistico_Airline.csv")

# Check the structure of the dataset
str(airline)   # 129880 rows, 23 columns

# Check the total number of missing values in the dataset
sum(is.na(airline))

# Load the necessary libraries for data manipulation
library(dplyr)

# Create a new column 'Age_Group' based on age categories
new_airline <- airline |> 
  mutate(
    Age_Group = case_when(
      Age >= 0 & Age <= 14 ~ "Children (0-14 years)",
      Age >= 15 & Age <= 24 ~ "Youth (15-24 years)",
      Age >= 25 & Age <= 64 ~ "Adults (25-64 years)",
      Age >= 65 ~ "Seniors (65 years and over)",
      TRUE ~ NA_character_  # NA for other cases
    )
  )

# Reorder columns using relocate()
new_airline <- new_airline |>
  relocate(Age_Group, .after = Age)

# Define the labels for the five-level ordinal scale
labels <- c("Very Poor", "Poor", "Below Average", "Average", "Above Average", "Excellent")

# Columns to categorize
cols_to_categorize <- c(
  "Seat.comfort",
  "Departure.Arrival.time.convenient",
  "Food.and.drink",
  "Inflight.wifi.service",
  "Inflight.entertainment",
  "Online.support",
  "Ease.of.Online.booking",
  "On.board.service",
  "Leg.room.service",
  "Baggage.handling",
  "Checkin.service",
  "Cleanliness",
  "Online.boarding"
)

# Loop through columns to categorize
for (col_name in cols_to_categorize) {
  if (is.numeric(new_airline[[col_name]]) &&
      all(new_airline[[col_name]] %in% 0:5)) {
    # Replace values with labels
    new_airline[[col_name]] <-
      factor(new_airline[[col_name]], levels = 0:5, labels = labels)
  }
}

# Display the resulting dataset with replaced labels
View(new_airline)




### Data Visualization ###

# Plot customer satisfaction by gender
library(dplyr)
library(ggplot2)
library(forcats)


airline |>
  group_by(Gender, satisfaction) |>
  summarise(n = n()) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(x = satisfaction, y = n, fill = satisfaction)) +  # Updated fill aesthetics to satisfaction
  geom_bar(
    stat = "identity",
    position = "stack",
    width = 0.35,
    color = "black",
    size = 1
  ) +
  geom_text(
    aes(label = paste0(round(perc), "%")),
    position = position_stack(vjust = 0.5),
    size = 2.65,
    # Adjust text size
    hjust = 0.5,
    # Center the text horizontally
    vjust = -0.5,
    # Center the text vertically
    color = "black"
  ) +
  labs(title = "Customer Satisfaction by Gender", x = "Satisfaction", y = "Count") +
  facet_grid(. ~ Gender,
             scales = "free_x",
             space = "free_x",
             switch = "x") +  # Added facet grid for Gender
  scale_x_discrete(labels = c("Dissatisfied", "Satisfied")) +
  scale_fill_discrete(name = "Satisfaction",
                      labels = c("Dissatisfied", "Satisfied")) +  # Updated fill labels
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# The plot shows that more customers are satisfied than dissatisfied.
# Among males, 56% are dissatisfied and 44% are satisfied.
# Among females, 65% are satisfied and 35% are dissatisfied.
# In conclusion, males tend to be dissatisfied with the airline service while females tend to be more satisfied.

# As for why this is the case,
# it is hard to say without more information about the airline service
# and its customers.
# However, it could be due to differences in expectations or
# experiences between male and female customers.

airline %>%
  group_by(Gender, satisfaction, Customer.Type) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = satisfaction, y = n, fill = Customer.Type)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    width = 0.35,
    color = "black",
    size = 1
  ) +
  geom_text(
    aes(label = paste0(round(perc), "%")),
    position = position_stack(vjust = 0.5),
    size = 2.75,
    # Adjust text size
    hjust = 0.5,
    # Center the text horizontally
    vjust = 0.5,
    # Center the text vertically
    color = "black"
  ) + # Add white color to text
  labs(title = "Customer Satisfaction by Gender and Customer Type", x = "Satisfaction", y = "Count") +
  facet_wrap( ~ Gender,
              nrow = 1,
              scales = "free_x",
              switch = "x") + # Use facet_wrap with nrow = 1 to create a single row of facets
  scale_x_discrete(labels = c("Dissatisfied", "Satisfied")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# A surprising fact is that loyal male customers actually make up 77% of those
# who are dissatisfied with the airline service.

airline %>%
  group_by(Gender, Customer.Type) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Gender, y = n, fill = Customer.Type)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = "black",
    size = 1
  ) +
  geom_text(
    aes(label = paste0(round(perc), "%")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) + # Add percentage text labels
  labs(title = "Customer Type Distribution by Gender", x = "Gender", y = "Count") +
  scale_fill_discrete(name = "Customer Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# I do not see anything special about this.


# Load required packages
library(dplyr)
library(ggplot2)

# Group by Age_Group and Customer.Type, calculate counts
age_group_counts <- new_airline %>%
  group_by(Age_Group, Customer.Type) %>%
  summarise(n = n()) %>%
  ungroup()

# Calculate percentage within each Age_Group and Customer.Type combination
age_group_counts <- age_group_counts %>%
  group_by(Age_Group) %>%
  mutate(perc = n / sum(n) * 100)

# Plot the distribution of Age_Group by Customer.Type
ggplot(age_group_counts,
       aes(x = Age_Group, y = perc, fill = Customer.Type)) +
  geom_bar(stat = "identity",
           position = "stack",
           width = 0.8) +
  labs(title = "Distribution of Age Group by Customer Type", x = "Age Group", y = "Percentage") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Customer Satisfaction by age group
new_airline |>
  group_by(Age_Group, satisfaction) |>
  summarise(n = n()) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(x = satisfaction, y = n, fill = satisfaction)) +  # Updated fill aesthetics to satisfaction
  geom_bar(
    stat = "identity",
    position = "stack",
    width = 0.35,
    color = "black",
    size = 1
  ) +
  geom_text(
    aes(label = paste0(round(perc), "%")),
    position = position_stack(vjust = 0.5),
    size = 2.65,
    # Adjust text size
    hjust = 0.5,
    # Center the text horizontally
    vjust = -0.5,
    # Center the text vertically
    color = "black"
  ) +
  labs(title = "Customer Satisfaction by Age Group", x = "Satisfaction", y = "Count") +
  facet_grid(. ~ Age_Group,
             scales = "free_x",
             space = "free_x",
             switch = "x") +  # Added facet grid for Gender
  scale_x_discrete(labels = c("Dissatisfied", "Satisfied")) +
  scale_fill_discrete(name = "Satisfaction",
                      labels = c("Dissatisfied", "Satisfied")) +  # Updated fill labels
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Based on the plot, it appears that children, youth, and senior citizens
# have higher proportions of dissatisfied levels with the airline service, which is unexpected.
# This suggests that there may be underlying issues with the service provided by the airline,
# as the proportion of dissatisfied customers is generally high across these age groups.

# Customer Satisfaction by age group and customer type
new_airline |>
  group_by(Age_Group, Customer.Type, satisfaction) |>
  summarise(n = n()) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(x = satisfaction, y = n, fill = satisfaction)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    width = 0.35,
    color = "black",
    size = 1
  ) +
  geom_text(
    aes(label = paste0(round(perc), "%")),
    position = position_stack(vjust = 0.5),
    size = 2.65,
    hjust = 0.5,
    vjust = -0.5,
    color = "black"
  ) +
  labs(title = "Customer Satisfaction by Age Group and Customer Type", x = "Satisfaction", y = "Count") +
  facet_grid(
    . ~ Age_Group + Customer.Type,
    # Updated facet grid to include Customer.Type
    scales = "free_x",
    space = "free_x",
    switch = "x"
  ) +
  scale_x_discrete(labels = c("Dissatisfied", "Satisfied")) +
  scale_fill_discrete(name = "Satisfaction",
                      labels = c("Dissatisfied", "Satisfied")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
