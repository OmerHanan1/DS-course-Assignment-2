-------------------
-- #Assignment 2 --
-------------------
install.packages(caret)
library(caret)
  
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
# Discretize the ApplicantIncome, CoapplicantIncome, LoanAmount variables into 5 categories of equal width
data$ApplicantIncome <- cut(data$ApplicantIncome, breaks = 5, labels = c("low", "low-med", "med", "med-high", "high"))
data$CoapplicantIncome <- cut(data$CoapplicantIncome, breaks = 5, labels = c("low", "low-med", "med", "med-high", "high"))
data$LoanAmount <- cut(data$LoanAmount, breaks = 5, labels = c("low", "low-med", "med", "med-high", "high"))

data





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
