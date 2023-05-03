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



  