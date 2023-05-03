-------------------
-- #Assignment 2 --
-------------------
install.packages(caret)
install.packages("mltools")
library(mltools)
library(caret)
library(rpart)
  
# Question 1

--- 1.1 
data <- data.frame(train_Loan)
data[data == ""] <- NA

#Structure the data in order to find columns data types:
str(data)

# Count NA values:
na_values <- sapply(data, function(x) sum(is.na(x)))
na_values

# Update categorical values as factors with them unique values:
categorical_col_names <- c("Gender", "Married", "Dependents", "Education", "Self_Employed", 
               "Credit_History", "Property_Area", "Loan_Amount_Term")
numeric_col_names <- c("ApplicantIncome", "CoapplicantIncome", 
                       "LoanAmount")

data$Loan_Status <- factor(data$Loan_Status, levels = unique(data$Loan_Status))

for (col in categorical_col_names) {
  col_levels <- unique(data[[col]])
  data[[col]] <- factor(data[[col]], levels = col_levels)
}

--- 1.2
# Function to calculate mode value
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Mean value for numeric columns
for (col in numeric_col_names) {
  # Print values for report
  print(col)
  print(mean(data[[col]], na.rm = TRUE))
  
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}
# Mode value for categorical columns
for (col in categorical_col_names) {
  mode_value <- Mode(data[[col]][!is.na(data[[col]])])
  # Print values for report
  print(col)
  print(mode_value)
  
  data[[col]][is.na(data[[col]])] <- mode_value
}
# NA count check - There are no NA values in the data.
sapply(data, function(x) sum(is.na(x)))

--- 1.3
labels <- c("L", "LM", "M", "MH", "H")
labels_three <- c("L", "M", "H")

data[, "ApplicantIncome"] <- bin_data(data$ApplicantIncome, bins=5, binType = "quantile")
data$ApplicantIncome <- factor(data$ApplicantIncome, labels = labels)
summary(data$ApplicantIncome)

data[, "CoapplicantIncome"] <- bin_data(data$LoanAmount, bins=5, binType = "quantile")
data$CoapplicantIncome <- factor(data$CoapplicantIncome, labels = labels)
summary(data$CoapplicantIncome)

data[, "LoanAmount"] <- bin_data(data$LoanAmount, bins=3, binType = "quantile")
data$LoanAmount <- factor(data$LoanAmount, labels = labels_three)
summary(data$LoanAmount)

--- 1.4
# Attached to report

--- 1.5
# Split data into train and test sub-sets:
inTrain <- createDataPartition(y=data$Loan_Status, p=0.8, list=FALSE)
train_data <- data[inTrain, ]
test_data <- data[-inTrain, ]
nrow(data)
nrow(train_data)
nrow(test_data)

# Question 2

--- 2.1

tree_infogain <- rpart(Loan_Status ~ Gender + Married + Dependents + Education +
                         Self_Employed + ApplicantIncome + CoapplicantIncome + 
                         LoanAmount + Loan_Amount_Term + Credit_History +
                         Property_Area , data = data, parms 
                       = list(split = "information"), cp = 0.01)
print(tree_infogain)
predictions <- predict(tree_infogain, test_data, type = "class")
info_accuracy <- confusionMatrix(predictions, test_data$Loan_Status)$overall["Accuracy"]
cat("Accuracy:", info_accuracy, "\n")

tree_gini <- rpart(Loan_Status ~ Gender + Married + Dependents + Education +
                         Self_Employed + ApplicantIncome + CoapplicantIncome +
                         LoanAmount + Loan_Amount_Term + Credit_History +
                         Property_Area , data = data, parms 
                       = list(split = "gini"), cp = 0.01)
print(tree_gini)
predictions <- predict(tree_gini, test_data, type = "class")
gini_accuracy <- confusionMatrix(predictions, test_data$Loan_Status)$overall["Accuracy"]
cat("Accuracy:", gini_accuracy, "\n")

minsplit_2 <- rpart(Loan_Status ~ Gender + Married + Dependents + Education +
                         Self_Employed + ApplicantIncome + CoapplicantIncome + 
                         LoanAmount + Loan_Amount_Term + Credit_History +
                         Property_Area , data = data, parms 
                       = list(split = "information"), minsplit = 2, cp=0.01)
print(minsplit_2)
predictions <- predict(minsplit_2, test_data, type = "class")
minsplit_2_accuracy <- confusionMatrix(predictions, test_data$Loan_Status)$overall["Accuracy"]
cat("Accuracy:", minsplit_2_accuracy, "\n")

minsplit_5 <- rpart(Loan_Status ~ Gender + Married + Dependents + Education +
                      Self_Employed + ApplicantIncome + CoapplicantIncome + 
                      LoanAmount + Loan_Amount_Term + Credit_History +
                      Property_Area , data = data, parms 
                    = list(split = "information"), minsplit = 5, cp=0.01)
print(minsplit_5)
predictions <- predict(minsplit_5, test_data, type = "class")
minsplit_5_accuracy <- confusionMatrix(predictions, test_data$Loan_Status)$overall["Accuracy"]
cat("Accuracy:", minsplit_5_accuracy, "\n")

