# Load the required libraries
library(randomForest)

# Load the dataset
df <- read.csv("Invistico_Airline.csv") # Replace "flight_dataset.csv" with the actual file name and path

# Convert categorical variables to factor
df$Gender <- as.factor(df$Gender)
df$Customer.Type <- as.factor(df$Customer.Type)
df$Type.of.Travel <- as.factor(df$Type.of.Travel)
df$Class <- as.factor(df$Class)

# Convert satisfaction to a factor variable
df$satisfaction <- as.factor(df$satisfaction)

# Remove rows with missing values
df <- na.omit(df)

# Split the dataset into training and testing data
set.seed(123)
train_indices <- sample(1:nrow(df), nrow(df)*0.8) # 80% for training
train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]

# Build the random forest model
rf_model <- randomForest(satisfaction ~ ., data = train_df, ntree = 100)

# Make predictions on the testing data
predictions <- predict(rf_model, test_df)

# Evaluate model performance
confusion_matrix <- table(predictions, test_df$satisfaction)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")
cat("Confusion Matrix:\n")
print(confusion_matrix)
