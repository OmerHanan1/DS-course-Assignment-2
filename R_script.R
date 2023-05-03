-------------------
-- #Assignment 2 --
-------------------

# Question 1

--- 1.1 
data <- data.frame(train_Loan)
#Structure the data in order to find columns data types:
str(data)
# Count NA values:
sapply(data, function(x) sum(is.na(x)))
# Count empty strings values:
sapply(data, function(x) sum(nchar(as.character(data$x))))

# Update categorical values as factors with them unique values:
categorical_col_names <- c("Gender", "Married", "Dependents", "Education", "Self_Employed", 
               "Credit_History", "Property_Area", "Loan_Status")
numeric_col_names <- c("ApplicantIncome", "CoapplicantIncome", 
                       "LoanAmount", "Loan_Amount_Term")

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
  print(col)
  print(mean(data[[col]], na.rm = TRUE))
  
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}
# Mode value for categorical columns
for (col in categorical_col_names) {
  mode_value <- Mode(data[[col]][!is.na(data[[col]])])
  print(col)
  print(mode_value)
  
  data[[col]][is.na(data[[col]])] <- mode_value
}
# NA count check - There are no NA values in the data.
sapply(data, function(x) sum(is.na(x)))






