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

# Perform k-fold cross-validation
set.seed(123)
k <- 5 # Number of folds
accuracy <- numeric(k) # Store accuracy for each fold

# Loop through each fold
for (i in 1:k) {
  # Split data into training and testing sets
  indices <- sample(1:nrow(df), nrow(df)*(k-1)/k) # (k-1)/k for training
  train_df <- df[indices, ]
  test_df <- df[-indices, ]
  
  # Build the random forest model
  rf_model <- randomForest(satisfaction ~ ., data = train_df, ntree = 100)
  
  # Make predictions on the testing data
  predictions <- predict(rf_model, test_df)
  
  # Evaluate model performance
  confusion_matrix <- table(predictions, test_df$satisfaction)
  accuracy[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

# Calculate mean accuracy across all folds
mean_accuracy <- mean(accuracy)
cat("Mean Accuracy:", mean_accuracy, "\n")
