# Read in the training and test dataset
train_data <- read.csv("train5.csv")
test_data <- read.csv("test5.csv")

# Create a Check if package is installed and install them
installIfNeeded = function(cliblist){
  libsNeeded = cliblist
  libsNeeded = libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}

installIfNeeded(c("glmnet", "tidyverse", "DescTools", "caret", "randomForest"))

#Load packages
library(glmnet)
library(DescTools)
library(caret)
library(randomForest)

# Removing the above factor variables
# Remove Low_Qual_Fin_SF, Kitchen_AbvGr, Three_season_porch, Pool_Area and Misc_Val
# as they are all 0
rm_vars <- c("Low_Qual_Fin_SF", "Kitchen_AbvGr", "Three_season_porch",
             "Pool_Area", "Misc_Val", "Condition_2", "Heating", 
             "Pool_QC", "Roof_Matl", "Street", "Utilities", "Longitude", 
             "Latitude", "Garage_Yr_Blt")
train_data <- train_data[ , -which(names(train_data) %in% rm_vars)]

# Adding in indicator if the house has been remodeled
train_data$remod_ind <- ifelse(train_data$Year_Built==train_data$Year_Remod_Add, 0, 1)
train_data$tot_area <- train_data$Mas_Vnr_Area + train_data$Bsmt_Unf_SF + 
  train_data$Total_Bsmt_SF + train_data$Gr_Liv_Area + train_data$Wood_Deck_SF + 
  train_data$Open_Porch_SF + train_data$Enclosed_Porch + train_data$Screen_Porch

dmy <- dummyVars(" ~ .", data = train_data)
train_data <- data.frame(predict(dmy, newdata = train_data))

# Quantile Distribution of the numeric variables
is.num <- sapply(train_data, is.numeric)

# Subset data for numeric variables
num_data <- train_data[, is.num]

percent_dist <- as.data.frame(sapply( num_data, function( y ) {
  quantile( y, probs = c(0.05,0.95))
}))

train_data$Lot_Frontage <- Winsorize(train_data$Lot_Frontage, minval = 0)
train_data$Lot_Area <- Winsorize(train_data$Lot_Area, minval = 0)
train_data$Mas_Vnr_Area <- Winsorize(train_data$Mas_Vnr_Area, minval = 0)
train_data$BsmtFin_SF_1 <- Winsorize(train_data$BsmtFin_SF_1, minval = 0)
train_data$BsmtFin_SF_2 <- Winsorize(train_data$BsmtFin_SF_2, minval = 0)
train_data$Bsmt_Unf_SF <- Winsorize(train_data$Bsmt_Unf_SF, minval = 0)
train_data$Total_Bsmt_SF <- Winsorize(train_data$Total_Bsmt_SF, minval = 0)
train_data$First_Flr_SF <- Winsorize(train_data$First_Flr_SF, minval = 0)
train_data$Second_Flr_SF <- Winsorize(train_data$Second_Flr_SF, minval = 0)
train_data$Gr_Liv_Area <- Winsorize(train_data$Gr_Liv_Area, minval = 0)
train_data$Bsmt_Full_Bath <- Winsorize(train_data$Bsmt_Full_Bath, minval = 0)
train_data$Bsmt_Half_Bath <- Winsorize(train_data$Bsmt_Half_Bath, minval = 0)
train_data$Full_Bath <- Winsorize(train_data$Full_Bath, minval = 0)
train_data$Half_Bath <- Winsorize(train_data$Half_Bath, minval = 0)
train_data$Bedroom_AbvGr <- Winsorize(train_data$Bedroom_AbvGr, minval = 0)
train_data$TotRms_AbvGrd <- Winsorize(train_data$TotRms_AbvGrd, minval = 0)
train_data$Fireplaces <- Winsorize(train_data$Fireplaces, minval = 0)
train_data$Garage_Cars <- Winsorize(train_data$Garage_Cars, minval = 0)
train_data$Garage_Area <- Winsorize(train_data$Garage_Area, minval = 0)
train_data$Wood_Deck_SF <- Winsorize(train_data$Wood_Deck_SF, minval = 0)
train_data$Open_Porch_SF <- Winsorize(train_data$Open_Porch_SF, minval = 0)
train_data$Enclosed_Porch <- Winsorize(train_data$Enclosed_Porch, minval = 0)
train_data$Screen_Porch <- Winsorize(train_data$Screen_Porch, minval = 0)

# Remove the 2 PIDs 
train_data <- train_data[!(train_data$PID %in% c(902207130,910251050)), ]

##################################################################
# Cleaning test data
test_data <- test_data[ , -which(names(test_data) %in% rm_vars)]

# Adding in indicator if the house has been remodeled
test_data$remod_ind <- ifelse(test_data$Year_Built==test_data$Year_Remod_Add, 0, 1)
test_data$tot_area <- test_data$Mas_Vnr_Area + test_data$Bsmt_Unf_SF + 
  test_data$Total_Bsmt_SF + test_data$Gr_Liv_Area + test_data$Wood_Deck_SF + 
  test_data$Open_Porch_SF + test_data$Enclosed_Porch + test_data$Screen_Porch

dmy2 <- dummyVars(" ~ .", data = test_data)
test_data <- data.frame(predict(dmy2, newdata = test_data))

cols_train <- colnames(train_data)
cols_test <- colnames(test_data)

common_vars <- intersect(cols_test, cols_train)
train_data <- train_data[,common_vars]
test_data <- test_data[,common_vars]


# Winzorising numerical values in test data
test_data$Lot_Frontage = Winsorize(test_data$Lot_Frontage, minval = 0,
                                   maxval=percent_dist$Lot_Frontage[2])
test_data$Lot_Area = Winsorize(test_data$Lot_Area, minval = 0,
                               maxval=percent_dist$Lot_Area[2])
test_data$Mas_Vnr_Area = Winsorize(test_data$Mas_Vnr_Area, minval = 0,
                                   maxval=percent_dist$Mas_Vnr_Area[2])
test_data$BsmtFin_SF_1 = Winsorize(test_data$BsmtFin_SF_1, minval = 0,
                                   maxval=percent_dist$BsmtFin_SF_1[2])
test_data$BsmtFin_SF_2 = Winsorize(test_data$BsmtFin_SF_2, minval = 0,
                                   maxval=percent_dist$BsmtFin_SF_2[2])
test_data$Bsmt_Unf_SF = Winsorize(test_data$Bsmt_Unf_SF, minval = 0,
                                  maxval=percent_dist$Bsmt_Unf_SF[2])
test_data$Total_Bsmt_SF = Winsorize(test_data$Total_Bsmt_SF, minval = 0,
                                    maxval=percent_dist$Total_Bsmt_SF[2])
test_data$First_Flr_SF = Winsorize(test_data$First_Flr_SF, minval = 0,
                                   maxval=percent_dist$First_Flr_SF[2])
test_data$Second_Flr_SF = Winsorize(test_data$Second_Flr_SF, minval = 0,
                                    maxval=percent_dist$Second_Flr_SF[2])
test_data$Gr_Liv_Area = Winsorize(test_data$Gr_Liv_Area, minval = 0,
                                  maxval=percent_dist$Gr_Liv_Area[2])
test_data$Bsmt_Full_Bath = Winsorize(test_data$Bsmt_Full_Bath, minval = 0,
                                     maxval=percent_dist$Bsmt_Full_Bath[2])
test_data$Bsmt_Half_Bath = Winsorize(test_data$Bsmt_Half_Bath, minval = 0,
                                     maxval=percent_dist$Bsmt_Half_Bath[2])
test_data$Full_Bath = Winsorize(test_data$Full_Bath, minval = 0,
                                maxval=percent_dist$Full_Bath[2])
test_data$Half_Bath = Winsorize(test_data$Half_Bath, minval = 0,
                                maxval=percent_dist$Half_Bath[2])
test_data$Bedroom_AbvGr = Winsorize(test_data$Bedroom_AbvGr, minval = 0,
                                    maxval=percent_dist$Bedroom_AbvGr[2])
test_data$TotRms_AbvGrd = Winsorize(test_data$TotRms_AbvGrd, minval = 0,
                                    maxval=percent_dist$TotRms_AbvGrd[2])
test_data$Fireplaces = Winsorize(test_data$Fireplaces, minval = 0,
                                 maxval=percent_dist$Fireplaces[2])
test_data$Garage_Cars = Winsorize(test_data$Garage_Cars, minval = 0,
                                  maxval=percent_dist$Garage_Cars[2])
test_data$Garage_Area = Winsorize(test_data$Garage_Area, minval = 0,
                                  maxval=percent_dist$Garage_Area[2])
test_data$Wood_Deck_SF = Winsorize(test_data$Wood_Deck_SF, minval = 0,
                                   maxval=percent_dist$Wood_Deck_SF[2])
test_data$Open_Porch_SF = Winsorize(test_data$Open_Porch_SF, minval = 0,
                                    maxval=percent_dist$Open_Porch_SF[2])
test_data$Enclosed_Porch = Winsorize(test_data$Enclosed_Porch, minval = 0,
                                     maxval=percent_dist$Enclosed_Porch[2])
test_data$Screen_Porch = Winsorize(test_data$Screen_Porch, minval = 0,
                                   maxval=percent_dist$Screen_Porch[2])
test_data$Sale_Price = Winsorize(test_data$Sale_Price, minval = 0,
                                 maxval=percent_dist$Sale_Price[2])


# Remove the 2 PIDs 
test_data <- test_data[!(test_data$PID %in% c(902207130,910251050)), ]


# Remove the 2 PIDs
X_train <- train_data[,!(colnames(train_data) %in% c("PID", "Sale_Price",
                                                     "Functional.Sal"))]
Y_train <- train_data[,"Sale_Price"]
X_test <- test_data[,!(colnames(test_data) %in% c("PID", "Sale_Price", 
                                                  "Functional.Sal"))]
Y_test <- test_data[,"Sale_Price"]
X_train <- as.matrix(X_train)
Y_train <- as.matrix(Y_train)
X_test <- as.matrix(X_test)
Y_test <- as.matrix(Y_test)

columns <- c("PID", "Sale_Price")
# testID = read.table(file="Project1_test_id.txt", sep="")

# for (i in 1:10) {
  # X_train <- raw_data_4[-which(raw_data_4$PID  %in% testID[,i]),
  #                       -which(names(raw_data_4) %in% c("PID","Sale_Price", 
  #                                                       "Functional.Sal"))]
  # Y_train <- raw_data_4[-which(raw_data_4$PID %in% testID[,i]),
  #                       which(names(raw_data_4) %in% c("Sale_Price"))]
  # X_test <- raw_data_4[which(raw_data_4$PID %in% testID[,i]),
  #                      -which(names(raw_data_4) %in% c("PID","Sale_Price", 
  #                                                      "Functional.Sal"))]
  # Y_test <- raw_data_4[which(raw_data_4$PID %in% testID[,i]),
  #                      which(names(raw_data_4) %in% c("Sale_Price"))]
  # X_train <- as.matrix(X_train)
  # Y_train <- as.matrix(Y_train)
  # X_test <- as.matrix(X_test)
  # Y_test <- as.matrix(Y_test)
  
  cv.out = cv.glmnet(X_train, log(Y_train), alpha = 1)
  Ytest.pred = exp(predict(cv.out, s = cv.out$lambda.min, newx = X_test))
  mysubmission1 <- data.frame(PID=test_data[,"PID"], Sale_Price = Ytest.pred)
  colnames(mysubmission1) <- columns
  write.table(mysubmission1, file = "mysubmission1.txt", sep = ",", row.names = FALSE)
  # rmse = sqrt(mean((log(Ytest.pred) - log(Y_test))^2))
  # print(rmse)
# }


train_model2 <- train_data[, !(colnames(train_data) %in% c("PID", "Functional.Sal"))]
test_model2 <- test_data[, !(colnames(test_data) %in% c("PID", "Functional.Sal"))]
rfModel = randomForest(Sale_Price ~ .,
                       data = train_model2,
                       importance = T, ntree=400);
Sale_Price = predict(rfModel, test_model2)

# rmse2 = sqrt(mean((log(Sale_Price) - log(test_model2$Sale_Price))^2))
# print(rmse2)
mysubmission2 <- data.frame(PID=test_data[,"PID"], Sale_Price = Sale_Price)
colnames(mysubmission2) <- columns
write.table(mysubmission2, file = "mysubmission2.txt", sep = ",", row.names = FALSE)




