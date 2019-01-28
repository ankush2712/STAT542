Analyzed the housing data collected on residential properties sold in Ames, Iowa between 2006 and 2010.

 The dataset has 2930 rows (i.e., houses) and 83 columns. Column 1 is "PID", the Parcel identification number, the last column is the response variable, "Sale_Price", and the remaining 81 columns are explanatory variables describing (almost) every aspect of residential homes.

 The goal is to predict the final price of a home with those explanatory variables.

The project includes 2 prediction models (Lasso regression with CV.GLMNET and Random Forest) and my own implementation of Lasso Regression without using any packages.

My best RMSE is in the range of 0.11 using cross validated glmnet (lasso regression). Kindly refer the R code and report for deep dive. 
