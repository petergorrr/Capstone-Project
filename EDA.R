# Load the data set
airline <- read.csv("Invistico_Airline.csv")

# View the data set
View(airline)

# Check the structure of the data set
str(airline)   # 129880 rows , 23 columns

# Check the total number of missing values in the data set
sum(is.na(airline))

# Plot customer satisfaction by gender
library(dplyr)
library(ggplot2)
library(forcats)


airline |>
  group_by(Gender, satisfaction) |>
  summarise(n = n()) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(x = satisfaction, y = n, fill = satisfaction)) +  # Updated fill aesthetics to satisfaction
  geom_bar(stat = "identity",
           position = "stack",
           width = 0.35) +
  geom_text(aes(label = paste0(round(perc), "%")), position = position_stack(vjust = 0.5)) +
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
