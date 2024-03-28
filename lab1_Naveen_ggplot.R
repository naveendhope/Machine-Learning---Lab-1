# Load necessary libraries
library(caret)
library(dplyr)
library(ggplot2)

# Read data from the file path
file_path <- "C:/Users/navee/OneDrive/Desktop/Business Analytics/Machine Learning/Lab1/oulad-students.csv"
data <- read.csv(file_path)

# Describe the variables
summary(subset_data)

# Remove rows with missing values
data <- na.omit(data)

# Convert necessary variables to factors
data$code_module <- as.factor(data$code_module)
data$code_presentation <- as.factor(data$code_presentation)
data$gender <- as.factor(data$gender)
data$region <- as.factor(data$region)
data$highest_education <- as.factor(data$highest_education)
data$imd_band <- as.factor(data$imd_band)
data$age_band <- as.factor(data$age_band)
data$num_of_prev_attempts <- as.factor(data$num_of_prev_attempts)
data$disability <- as.factor(data$disability)
data$final_result <- as.factor(data$final_result)

# Visualization with ggplot2
ggplot(data, aes(x=highest_education, fill=final_result)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  labs(title="Final Result by Highest Education",
       x="Highest Education", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better readability

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(100) # For reproducibility
train_index <- createDataPartition(data$final_result, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the classification model (logistic regression)
model <- train(final_result ~ ., data = train_data, method = "glm", family = "binomial")

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
confusionMatrix(predictions, test_data$final_result)

