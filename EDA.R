# Load the dataset
airline <- read.csv("Invistico_Airline.csv")

# Load the necessary libraries for data manipulation
library(dplyr)

# Create a new column 'Age_Group' based on age categories using mutate()
new_airline <- airline %>%
  mutate(
    "Age Group" = case_when(
      Age >= 0 & Age <= 14 ~ "Children (0-14 years)",
      Age >= 15 & Age <= 24 ~ "Youth (15-24 years)",
      Age >= 25 & Age <= 64 ~ "Adults (25-64 years)",
      Age >= 65 ~ "Seniors (65 years and over)",
      TRUE ~ NA_character_  # NA for other cases
    )
  ) %>%
  relocate("Age Group", .after = Age) # Reorder columns using relocate()

# Define the labels for the five-level ordinal scale
labels <-
  c("Very Poor",
    "Poor",
    "Below Average",
    "Average",
    "Above Average",
    "Excellent")

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

# Apply factor labels to columns
new_airline <- new_airline %>%
  mutate(across(
    all_of(cols_to_categorize),
    factor,
    levels = 0:5,
    labels = labels
  ))

# Rename columns in the dataset
new_airline <- new_airline %>%
  rename(
    "Satisfaction" = satisfaction,
    "Customer Type" = Customer.Type,
    "Type of Travel" = Type.of.Travel,
    "Flight Distance" = Flight.Distance,
    "Seat Comfort" = Seat.comfort,
    "Departure/Arrival Time Convenient" = Departure.Arrival.time.convenient,
    "Food and Drink" = Food.and.drink,
    "Gate and Location" = Gate.location,
    "Inflight Wifi Service" = Inflight.wifi.service,
    "Inflight Entertainment" = Inflight.entertainment,
    "Online Support" = Online.support,
    "Ease of Online Booking" = Ease.of.Online.booking,
    "On-board Service" = On.board.service,
    "Leg Room Service" = Leg.room.service,
    "Baggage Handling" = Baggage.handling,
    "Check-in Service" = Checkin.service,
    "Online Boarding" = Online.boarding,
    "Departure Delay in Minutes" = Departure.Delay.in.Minutes,
    "Arrival Delay in Minutes" = Arrival.Delay.in.Minutes
  )

# Display the resulting dataset with replaced labels
View(new_airline)


# Q1.What is the distribution of customer satisfaction levels in the dataset?

# Load needed libraries
library(ggplot2)
library(forcats)

# Plot 1 :A bar plot showing satisfaction levels of customers for the airline.
new_airline %>%
  ggplot(aes(x = Satisfaction, fill = Satisfaction)) +
  geom_bar(width = 0.5) +
  geom_text(
    aes(label = ..count.., y = ..count..),
    stat = "count",
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x = "Satisfaction", y = "Count", title = "Customer Satisfaction Distribution") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# From the plot above ,
# we get to know that satisfied customers are more than dissatisfied customers.

new_airline |>
  group_by(Gender, Satisfaction) |>
  summarise(n = n()) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(x = Satisfaction, y = n, fill = Satisfaction)) +  # Updated fill aesthetics to satisfaction
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

# Among males, 56% are dissatisfied and 44% are satisfied.
# Among females, 65% are satisfied and 35% are dissatisfied.
# In conclusion, males tend to be dissatisfied with the airline service while females tend to be more satisfied.

# As for why this is the case,
# it is hard to say without more information about the airline service
# and its customers.
# However, it could be due to differences in expectations or
# experiences between male and female customers.

new_airline %>%
  group_by(Gender, Satisfaction, `Customer Type`) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Satisfaction, y = n, fill = `Customer Type`)) +
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
    hjust = 0.5,
    vjust = 0.5,
    color = "black"
  ) +
  labs(title = "Customer Satisfaction by Gender and Customer Type", x = "Satisfaction", y = "Count") +
  facet_wrap(~ Gender,
             nrow = 1,
             scales = "free_x",
             switch = "x") +
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
# Therefore, we can conclude that there's something
# about the service that fails to satisfy the loyal customers.

new_airline %>%
  group_by(Gender, `Customer Type`) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Gender, y = n, fill = `Customer Type`)) +
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



# Customer Satisfaction by Age Group
new_airline |>
  group_by(`Age Group`, Satisfaction) |>
  summarise(n = n()) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(x = Satisfaction, y = n, fill = Satisfaction)) +  # Updated fill aesthetics to satisfaction
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
  facet_grid(. ~ `Age Group`,
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

# Customer Satisfaction by Age Group and Customer Type
new_airline |>
  group_by(`Age Group`, `Customer Type`, Satisfaction) |>
  summarise(n = n()) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(x = Satisfaction, y = n, fill = Satisfaction)) +
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
    . ~ `Age Group` + `Customer Type`,
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

# Generally speaking , except for the loyal adults group ,
# every other group has high proportion of not liking the airline service.


# Check the distribution of customer satisfaction level and their class of seats
ggplot(new_airline, aes(x = Class, y = ..count.., fill = Satisfaction)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Distribution of Customers by Class of Seats and Satisfaction",
       x = "Class of Seats", y = "Count") +
  scale_fill_discrete(name = "Satisfaction") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# The plot fulfills my expectation for the outcome.


ggplot(new_airline, aes(x = Class, fill = Satisfaction)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ Gender) +
  labs(title = "Distribution of Customers by Class of Seats and Satisfaction",
       x = "Class of Seats", y = "Count") +
  scale_fill_discrete(name = "Satisfaction") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_x_discrete(limits = c("Business", "Eco", "Eco Plus"))

# We can see there's an exceptional high number of male who are unsatisfied with
# the economy class service of the airline , which is quite worth a probe.

# Question 2: What happened to those males who's taken economy class in the airline?

# Filter the rows to only males with economy class.
airline_male_economy <- new_airline %>%
  filter(Gender == "Male", Class == "Eco")

airline_male_economy |>
  group_by(`Age Group`, `Customer Type`, Satisfaction) |>
  summarise(n = n()) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(x = Satisfaction, y = n, fill = Satisfaction)) +
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
    . ~ `Age Group` + `Customer Type`,
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

# Every age group has a very large contrast of people 
# who do not like the airline service.

airline_male_economy %>%
  group_by(`Age Group`, `Customer Type`, `Type of Travel`, Satisfaction) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Satisfaction, y = perc, fill = Satisfaction)) +
  geom_bar(
    stat = "identity",
    position = "fill",
    color = "black",
    size = 1,
    width = 0.7 # set width to control the bar width
  ) +
  geom_text(
    aes(label = paste0(round(perc), "%")),
    position = position_fill(vjust = 0.5),
    size = 4,
    color = "black"
  ) +
  labs(title = "Customer Satisfaction by Age Group, Customer Type, and Type of Travel",
       x = "Satisfaction", y = "Percentage") +
  facet_grid(
    `Age Group` ~ `Customer Type` + `Type of Travel`,
    scales = "free",
    space = "free_x",
    switch = "y"
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
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines")
  )

airline_male_economy %>%
  group_by(`Type of Travel`, Satisfaction) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Satisfaction, y = perc, fill = `Type of Travel`)) +
  geom_bar(
    stat = "identity",
    position = "fill",
    color = "black",
    size = 1,
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(perc), "%")),
    position = position_fill(vjust = 0.5),
    size = 3,
    hjust = 0.5,
    color = "black"
  ) +
  labs(title = "Customer Satisfaction by Type of Travel", x = "Satisfaction", y = "Percentage") +
  scale_x_discrete(labels = c("Dissatisfied", "Satisfied")) +
  scale_fill_discrete(name = "Type of Travel") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
