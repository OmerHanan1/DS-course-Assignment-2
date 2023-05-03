-------------------
-- #Assignment 2 --
-------------------
install.packages(caret)
install.packages("mltools")
library(mltools)
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

data[, "ApplicantIncome"] <- bin_data(data$LoanAmount, bins=5, binType = "quantile")
labels <- c("Low", "Low-Mid", "Mid", "Mid-High", "High")
data[, "ApplicantIncome"] <- cut(data$ApplicantIncome, 
                                 breaks = c(-Inf, quantile(data$ApplicantIncome, probs = seq(0, 1, 0.2)), Inf), 
                                 labels = labels, include.lowest = TRUE)

# Print the summary of the binned data with labels
summary(data$ApplicantIncome)







data[, "ApplicantIncome"] <- bin_data(data$LoanAmount, bins=5, binType = "quantile")
data[, "CoapplicantIncome"] <- bin_data(data$LoanAmount, bins=5, binType = "quantile")
data[, "LoanAmount"] <- bin_data(data$LoanAmount, bins=3, binType = "quantile")

summary(data$ApplicantIncome)
summary(data$CoapplicantIncome)
summary(data$LoanAmount)
str(data)

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


