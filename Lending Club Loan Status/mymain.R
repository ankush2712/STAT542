#Install and download the libraries
installIfNeeded = function(cliblist){
  libsNeeded = cliblist
  libsNeeded = libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}
installIfNeeded(c("glmnet", "caret"))

library(caret)
library(glmnet)


# Read in the train and test data
train <- read.csv('train3.csv')
test <- read.csv('test3.csv')

# Cleaning function
clean_data <- function(data){
  # Removing rendundant variables
  remove_vars <- c("earliest_cr_line", "emp_title", "grade", "title", 
                   "zip_code", "fico_range_low")
  data <- data[,-which(names(data) %in% remove_vars)]
  
  # Important variable treatment
  data$emp_length_num <- as.numeric(factor(data$emp_length, levels=c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10+ years")))
  data$emp_length_num <- data$emp_length_num - 1
  data$term_num <- ifelse(data$term=='36 months', 36, 60)
  data$home_ownership[data$home_ownership %in% c('ANY', 'NONE')] <- 'OTHER'
  data$home_ownership <- factor(data$home_ownership, levels = unique(data$home_ownership), ordered = FALSE)
  data$loan_status <- ifelse(data$loan_status=="Fully Paid", 0, 1)
  
  # remove these vars
  data$term <- NULL
  data$emp_length <- NULL
  
  # removing all missing values
  data <- data[complete.cases(data), ]
  
  # One hot encoding for the factor variables
  dmy <- dummyVars(" ~ .", data = data)
  data <- data.frame(predict(dmy, newdata = data))
}

train <- clean_data(train)
test <- clean_data(test)

x_train <- as.matrix(train[,-which(names(train) %in% c('id', 'loan_status'))])
y_train <- as.matrix(train[,'loan_status'])
x_test <- as.matrix(test[,-which(names(test) %in% c('id', 'loan_status'))])
y_test <- test[,'loan_status']

cvfit = cv.glmnet(x_train, y_train, family = "binomial", type.measure = "class")
predictions <- predict(cvfit, newx = x_test, s = "lambda.min", type = "response")


mysubmission1 <- data.frame(id=test[,"id"], prob = predictions)
colnames(mysubmission1) <- c("id", "prob")
write.table(mysubmission1, file = "mysubmission1.txt", sep = ",", row.names = FALSE)



