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
col_names <- c("Gender", "Married", "Dependents", "Education", "Self_Employed", 
               "Credit_History", "Property_Area", "Loan_Status")
for (col in col_names) {
  col_levels <- unique(data[[col]])
  data[[col]] <- factor(data[[col]], levels = col_levels)
}
