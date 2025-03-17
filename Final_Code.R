# Install and load the required packages
library(forecast)
library(glmnet)
library(tidyverse)
library(stats)
library(dplyr)   # For data manipulation
library(ggplot2) # For data visualization


# Create matrix from data for CPI
X <- as.matrix(McCracken_Cleaned_CPI[, 2:123])

# Create time series from matrix for CPI
your_ts <- ts(X, start = c(1960, 03), frequency = 12)

# Standardize data for CPI
standardized_ts <- scale(your_ts)

# Define outcome variable CPI
CPI <- standardized_ts[, 103]




####### RANDOM WALK COMPONENT

set.seed(123)

# Define number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 131 - 12

# Create a matrix to store residuals for each iteration
rw_residual <- numeric(num_iterations)

# Loop to iterate over each 10 year window
for(i in 1:num_iterations) {
  
  # Calculate the drift
  drift <- mean(diff(CPI[(i+12):(i+131)]))
  
  # Take the last observation
  last_observation <- CPI[(i+131)]
  
  # Start forecast with last observation
  rw_predicted_value <- last_observation
  
  # Loop to add random error component for each month along with drift
  for(j in 1:12) {
    rw_predicted_value <- rw_predicted_value + drift + rnorm(1, mean = 0, sd = sqrt(var(diff(CPI[(i+12):(i+131)]))))
  }
  
  # Define actual y value for the 12 months after subset
  actual_y_value <- CPI[i+143]
  
  # Calculate and store the sum of squared residual
  rw_residual[i] <- (actual_y_value - rw_predicted_value)^2
}

# Printing mean of rw_residual
mean_rw_residual <- (mean(rw_residual))







####### RIDGE REGRESSION

# Define lambda values to iterate over
ridge_lambda_values <- c(0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16, 0.18, 0.2, 0.22, 0.24, 0.26, 0.28, 0.3, 0.32, 0.34, 0.36, 0.38, 0.4, 0.42, 0.44, 0.46, 0.48, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)

# Define number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 131 - 12

# Create a matrix to store residuals for each iteration and each lambda value
ridge_MSE <- matrix(nrow=num_iterations, ncol=length(ridge_lambda_values), dimnames=list(NULL, ridge_lambda_values))

# Loop to iterate over lambda values
for(j in 1:length(ridge_lambda_values)) {
  
  # Loop to iterate over each 10 year window
  for(i in 1:num_iterations) {
    
    # Define subset for the regression
    subset_df <- standardized_ts[i:(i+119), ]
    
    # Define response and predictors from the subset
    y <- standardized_ts[(i+12):(i+131), 103]
    ridge_regressors <- subset_df[, c(1:102, 104:122)]
    test_ridge_regressors <- standardized_ts[(i+131), c(1:102, 104:122)]
    test_y <- standardized_ts[(i+143), 103]
    
    # Fit Ridge on the subset with the current lambda iteration
    fit <- glmnet(ridge_regressors, y, alpha=0, lambda=ridge_lambda_values[j], family="gaussian")
    
    # Coefficients for prediction
    coefficients <- coef(fit)
    
    # Predict values for the test data
    predicted_value <- coefficients[1] + sum(test_ridge_regressors * coefficients[-1])
    
    # Compute residual
    residual <- test_y - predicted_value
    
    # Store the sum of squared residuals for current iteration
    ridge_MSE[i, j] <- (residual^2)
  }
}

# Computing mean of residuals for each lambda value
mean_MSE <- apply(ridge_MSE, MARGIN=2, FUN=mean)

# Computing ratio of Ridge MSE to RW
ridge_ratio <- mean_MSE / mean_rw_residual



# Creating plot for ratio
data <- data.frame(
  lambda = ridge_lambda_values,
  ratio = ridge_ratio
)

ggplot(data, aes(x=lambda, y=ridge_ratio)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="Ridge MSE / RW - CPI", 
       x="Lambda", 
       y="MSE / Random Walk") + 
  theme_minimal()


ggsave("Ridge_CPI_Plot_ratio.png", width=8, height=5)




####### LASSO REGRESSION

# Define lambda values to iterate over
lasso_lambda_values <- c(0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.011, 0.012, 0.013, 0.014, 0.015, 0.016, 0.017, 0.018, 0.019, 0.02, 0.021, 0.022, 0.023, 0.024, 0.025, 0.026, 0.027, 0.028, 0.029, 0.03, 0.031, 0.032, 0.033, 0.034, 0.035, 0.036, 0.037, 0.038, 0.039, 0.04, 0.045, 0.05, 0.055, 0.06, 0.065, 0.07, 0.075, 0.08, 0.085, 0.09, 0.095, 0.1, 0.12, 0.14, 0.16, 0.18, 0.2, 0.22, 0.24, 0.26, 0.28, 0.3)

# Define number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 131 - 12

# Create a matrix to store residuals for each iteration and each lambda value
Lasso_MSE <- matrix(nrow=num_iterations, ncol=length(lasso_lambda_values), dimnames=list(NULL, lasso_lambda_values))

# Loop to iterate over lambda values
for(j in 1:length(lasso_lambda_values)) {
  
  # Loop to iterate over each 10 year window
  for(i in 1:num_iterations) {
    
    # Define subset for the regression
    subset_df <- standardized_ts[i:(i+119), ]
    
    # Define response and predictors from the subset
    y <- standardized_ts[(i+12):(i+131), 103]
    lasso_regressors <- subset_df[, c(1:102, 104:122)]
    test_lasso_regressors <- standardized_ts[(i+131), c(1:102, 104:122)]
    test_y <- standardized_ts[(i+143), 103]
    
    # Fit Lasso on the subset with the current lambda iteration
    fit <- glmnet(lasso_regressors, y, alpha=1, lambda=lasso_lambda_values[j], family="gaussian")
    
    # Extract coefficients for prediction
    coefficients <- coef(fit)
 
    # Predict value for the test data
    predicted_value <- coefficients[1] + sum(test_lasso_regressors * coefficients[-1])
                                                                                
    # Compute residual
    residual <- test_y - predicted_value
    
    # Store the sum of squared residuals for current iteration
    Lasso_MSE[i, j] <- residual^2
  }
}

# Computing mean of residuals for each lambda value
lasso_mean_MSE <- apply(Lasso_MSE, MARGIN=2, FUN=mean)

# Computing ratio of Lasso MSE to RW
lasso_ratio <- lasso_mean_MSE / mean_rw_residual

# Creating plot for ratio
data <- data.frame(
  lambda = lasso_lambda_values,
  ratio = lasso_ratio
)

ggplot(data, aes(x=lambda, y=lasso_ratio)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="Lasso MSE / RW - CPI", 
       x="Lambda", 
       y="MSE / Random Walk") + 
  theme_minimal()


ggsave("Lasso_CPI_Plot_ratio.png", width=8, height=5)








####### PCR WITH ITERATION THROUGH DATA

# Define component values to iterate over
num_components <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 78, 80, 82, 84, 86, 88, 90)  # Adjust this vector as per your choices

# Define number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 131 - 12

# Create a matrix to store residuals for each iteration and each component value
PCR_MSE <- matrix(nrow=num_iterations, ncol=length(num_components), dimnames=list(NULL, num_components))

# Loop to iterate over number of components values
for(j in 1:length(num_components)) {

  # Loop to iterate over each 10 year window  
  for(i in 1:num_iterations) {
  
    # Define subset for the regression
    subset_df <- standardized_ts[i:(i+119), ]
    
    # Define response and predictors from the subset
    y <- standardized_ts[(i+12):(i+131), 103]
    predictors <- subset_df[, c(1:102, 104:122)]
    test_predictors <- standardized_ts[(i+131), c(1:102, 104:122)]
    test_y <- standardized_ts[(i+143), 103]
    
    # Fit PCA
    pca_result <- prcomp(predictors, center = TRUE, scale. = TRUE)
    
    # Use PCA-transformed data as regressors - Note this is Z
    pc_regressors <- pca_result$x[,1:num_components[j]]
    
    # Assume pca_result is your PCA result from prcomp - Note this is V
    rotation_matrix <- pca_result$rotation
    
    # Select the first number of components column - Note this is V adjusted
    selected_eigenvectors <- rotation_matrix[, 1:num_components[j]]
    
    # Multiply new data by selected eigenvectors - Note this is Z_2
    projected_data <- test_predictors %*% selected_eigenvectors
    
    # Fit regression model using principal components
    fit <- lm(y ~ pc_regressors)
    
    # Extract coefficients for prediction
    coefficients <- coef(fit)
    
    # Predict value for the test data
    predicted_value <- cbind(1, projected_data) %*% coefficients
  
    # Compute residual
    residual <- test_y - predicted_value
    
    # Store the sum of squared residuals for current iteration
    PCR_MSE[i, j] <- (residual^2)
    }
  print("One component done")
}

# Computing mean of residuals for each component value
PCR_mean_MSE <- apply(PCR_MSE, MARGIN=2, FUN=mean)

# Computing the ratio of PCR MSE to RW
PCR_ratio <- PCR_mean_MSE / mean_rw_residual

# Creating plot for ratio
data <- data.frame(
  components = num_components,
  ratio = PCR_ratio
)

ggplot(data, aes(x=components, y=PCR_ratio)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="PCR MSE / RW - CPI", 
       x="Number of Components", 
       y="MSE / Random Walk") + 
  theme_minimal()


ggsave("PCR_CPI_Plot_ratio.png", width=8, height=5)







####### RANDOM WALK COMPONENT 10 YEAR WINDOW USED FOR 3PRF RATIO

set.seed(123)

# Define number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 264

# Create a matrix to store residuals for each iteration
rw_residual <- numeric(num_iterations)

# Loop to iterate over each 10 year window
for(i in 1:num_iterations) {
  
  # Calculate the drift
  drift <- mean(diff(CPI[(i+12):(i+131)]))
  
  # Take the last observation
  last_observation <- CPI[(i+131)]
  
  # Start forecast with last observation
  rw_predicted_value <- last_observation
  
  # Loop to add random error component for each month along with drift
  for(j in 1:120) {
    rw_predicted_value <- rw_predicted_value + drift + rnorm(1, mean = 0, sd = sqrt(var(diff(CPI[(i+12):(i+131)]))))
  }
  
  # Define actual y value for the 12 months after subset
  actual_y_value <- CPI[i+143]
  
  # Calculate and store the sum of squared residual
  rw_residual[i] <- (actual_y_value - rw_predicted_value)^2
}

# Now, 'rw_residuals' will contain the squared residuals for the Random Walk model for each iteration
mean_rw_residual_10_year <- (mean(rw_residual))







####### 3PRF

# Define number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 264

set.seed(123)

# Defining starting points for windows
starting_points <- c(1, 50, 100, 150, 200, 250, 300, 350, 400, 450)

# M values
M = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 45, 50, 55, 60, 65, 70, 75, 80)

# Create a matrix to store MSE for each iteration and each value of M
three_PRF_MSE <- matrix(nrow=length(starting_points), ncol=length(M), dimnames=list(NULL, M))

# Loop to iterate over M values
for(m in 1:length(M)) {
  
  # Reset row index for MSE matrix for next value of M
  row_index <- 1
  
  # Loop to iterate over each 10 year window
  for(i in starting_points) {
    
    # Define response and predictors from the subset
    y_shifted <- standardized_ts[(i+12):(i+131), 103]
    PRF_regressors <- standardized_ts[i:(i+119), c(1:102, 104:122)]
    test_PRF_regressors <- standardized_ts[(i+120):(i+239), c(1:102, 104:122)]
    test_y_shifted <- standardized_ts[(i+132):(i+251), 103]
  
    # Loop to iterate over each integer of M
    for (k in 1:M[m]) {
      
      # Defining initial Z
      Z_initial_train <- t(matrix(y_shifted, nrow = 1))
      
      # Defining which Z to use
      if (k == 1) {Z_train <- Z_initial_train}
      else {Z_train <- Z_appended_train}
      
      ### STEP 1 TRAIN
      
      # Matrix to store the coefficients in step 1
      coefficients_df_train <- matrix(NA, ncol = (k+1), nrow = 121)
      
      # Loop to fit
      for (n in 1:ncol(PRF_regressors)) {
        fit <- lm(PRF_regressors[, n] ~ Z_train)
        coefficients_df_train[n, ] <- coef(fit)
      }
      
      # This is big Phi
      coefficients_df_train <- coefficients_df_train[, -1] # Drop the intercept column
      
      ### STEP 2 TRAIN
      
      # Matrix to store the coefficients in step 2
      step2_coefficients_df_train <- matrix(NA, ncol = (k+1), nrow = 120)
      
      # Loop to fit
      for (n in 1:nrow(step2_coefficients_df_train)) {
        fit <- lm(PRF_regressors[n, ] ~ coefficients_df_train)
        step2_coefficients_df_train[n, ] <- coef(fit)
      }
      
      # This is big F
      step2_coefficients_df_train <- step2_coefficients_df_train[, -1]
      
      ### STEP 3 TRAIN
      
      fit <- lm(y_shifted ~ step2_coefficients_df_train)
      step3_residuals <- residuals(fit)
      
      # Beta_0 and Beta_1 for test
      coefficients_train <- coef(fit)
      
      # Update regressor_matrix for the next iteration
      Z_appended_train <- cbind(Z_train, step3_residuals)
      cat("one k done: ", k)
    }
    
    ### STEP 1 TEST
    
    # Matrix to store the coefficients in step 1 test
    coefficients_df_test <- matrix(NA, ncol = (k+1), nrow = 121) 
    
    # Z to use for test
    Z_test <- Z_appended_train[,-ncol(Z_appended_train)] # Excluding the first 108 rows and the last column to make the dimensions match
    
    # For loop to fit
    for (n in 1:ncol(test_PRF_regressors)) {
      fit <- lm(test_PRF_regressors[, n] ~ Z_test) 
      coefficients_df_test[n, ] <- coef(fit)
    }
    
    # This is big Phi - test
    coefficients_df_test <- coefficients_df_test[, -1] # Drop the intercept column
    
    # STEP 2 TEST
    
    # Matrix to store the coefficients in step 2 test
    step2_coefficients_df_test <- matrix(NA, ncol = (k+1), nrow = 120)
    
    for (n in 1:nrow(step2_coefficients_df_test)) {
      fit <- lm(test_PRF_regressors[n, ] ~ coefficients_df_test)
      step2_coefficients_df_test[n, ] <- coef(fit)
    }
    
    # This is big F - test
    step2_coefficients_df_test <- step2_coefficients_df_test[, -1]
    
    
    # No Step 3, just prediction for current M and current window
    predicted_value <- coefficients_train[1] + step2_coefficients_df_test %*% coefficients_train[-1]
    
    # Compute residual
    residual <- test_y_shifted - predicted_value
    
    residual_squared <- residual^2
    
    # Store the sum of squared residuals for current iteration
    three_PRF_MSE[row_index, m] <- (mean(residual_squared))
    
    # Moving row index plus 1 to store the MSE of the next iteration
    row_index <- row_index + 1
  }
  
}

# Computing the mean MSE of each M
PRF_mean_MSE <- apply(three_PRF_MSE, MARGIN=2, FUN=mean)

# Computing the ratio of 3PRF MSE to 10 year random walk
PRF_ratio <- PRF_mean_MSE / mean_rw_residual_10_year

# Creating plot for ratio
data <- data.frame(
  M_values = M,
  ratio = PRF_ratio
)

ggplot(data, aes(x=M_values, y=PRF_ratio)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="PRF MSE / RW - CPI", 
       x="Number of Factors", 
       y="MSE / Random Walk") + 
  theme_minimal()


ggsave("3PRF_CPI_Plot_ratio.png", width=8, height=5)





####################### REGRESSIONS FOR IP #####################################




# Create matrix from data for IP
Z <- as.matrix(McCracken_Cleaned_IP[,2:123])

# Create time series from matrix for IP
your_ts <- ts(Z, start = c(1960, 01), frequency = 12)

# Standardize data for IP
standardized_ts <- scale(your_ts)

# Define outcome variable IPI
IP <- standardized_ts[, 6]




####### RANDOM WALK COMPONENT

set.seed(123)

# Define number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 131 - 12

# Create a matrix to store residuals for each iteration
rw_residual <- numeric(num_iterations)

# Loop to iterate over each 10 year window
for(i in 1:num_iterations) {
  
  # Calculate the drift
  drift <- mean(diff(IP[(i+12):(i+131)]))
  
  # Take the last observation
  last_observation <- IP[(i+131)]
  
  # Start forecast with last observation
  rw_predicted_value <- last_observation
  
  # Loop to add random error component for each month along with drift
  for(j in 1:12) {
    rw_predicted_value <- rw_predicted_value + drift + rnorm(1, mean = 0, sd = sqrt(var(diff(IP[(i+12):(i+131)]))))
    print(rw_predicted_value)
  }
  
  # Define actual y value for the 12 months after subset
  actual_y_value <- IP[i+143]
  
  # Calculate and store the sum of squared residual
  rw_residual[i] <- (actual_y_value - rw_predicted_value)^2
}


# Computing mean of RW MSE to use for ratios
mean_rw_residual_IP <- mean(rw_residual)




 
####### RIDGE REGRESSION

# Lambda values to iterate over
ridge_lambda_values <- c(0.002, 0.005, 0.05, 0.1, 0.5, 1, 5, 6, 7, 8, 9, 10, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100)

# Number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 131 - 12

# Matrix to store residuals for each iteration and each lambda value
ridge_MSE <- matrix(nrow=num_iterations, ncol=length(ridge_lambda_values), dimnames=list(NULL, ridge_lambda_values))

# Loop to iterate over lambda values
for(j in 1:length(ridge_lambda_values)) {
  
  # Loop to iterate over each 10 year window
  for(i in 1:num_iterations) {
    
    # Subset for the regression
    subset_df <- standardized_ts[i:(i+119), ]
    
    # Response and predictors from the subset
    y <- standardized_ts[(i+12):(i+131), 6]
    ridge_regressors <- subset_df[, c(1:5, 7:122)]
    test_ridge_regressors <- standardized_ts[(i+131), c(1:5, 7:122)]
    test_y <- standardized_ts[(i+143), 6]
    
    # Fit Ridge on the subset with the current lambda iteration
    fit <- glmnet(ridge_regressors, y, alpha=0, lambda=ridge_lambda_values[j], family="gaussian")
    
    # Coefficients for prediction
    coefficients <- coef(fit)
    
    # Predict values for the test data
    predicted_value <- coefficients[1] + sum(test_ridge_regressors * coefficients[-1])
    
    # Compute residual
    residual <- test_y - predicted_value
    
    # Store the sum of squared residuals for current iteration
    ridge_MSE[i, j] <- (residual^2)
  }
}

# Computing mean of residuals for each lambda value
ridge_mean_MSE <- apply(ridge_MSE, MARGIN=2, FUN=mean)

# Computing ratio of Ridge MSE to RW
ridge_ratio <- ridge_mean_MSE / mean_rw_residual_IP

# Creating plot for ratio
data <- data.frame(
  lambda = ridge_lambda_values,
  ratio = ridge_ratio
)

ggplot(data, aes(x=lambda, y=ridge_ratio)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="Ridge MSE / RW - IP", 
       x="Lambda", 
       y="MSE / Random Walk") + 
  theme_minimal()


ggsave("Ridge_IP_Plot_ratio.png", width=8, height=5)





####### LASSO REGRESSION

# Lambda values to iterate over
lasso_lambda_values <- c(0.0004, 0.0006, 0.0008, 0.001, 0.00125, 0.0015, 0.002, 0.003, 0.005, 0.01, 0.02, 0.035, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.5)

# Number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 131 - 12

# Matrix to store residuals for each iteration and each lambda value
Lasso_MSE <- matrix(nrow=num_iterations, ncol=length(lasso_lambda_values), dimnames=list(NULL, lasso_lambda_values))

# Loop to iterate over lambda values
for(j in 1:length(lasso_lambda_values)) {
  
  # Loop to iterate over each 10 year window
  for(i in 1:num_iterations) {
    
    # Subset for the regression
    subset_df <- standardized_ts[i:(i+119), ]
    
    # Response and predictors from the subset
    y <- standardized_ts[(i+12):(i+131), 6]
    lasso_regressors <- subset_df[, c(1:5, 7:122)]
    test_lasso_regressors <- standardized_ts[(i+131), c(1:5, 7:122)]
    test_y <- standardized_ts[(i+143), 6]
    
    # Fit Lasso on the subset with the current lambda iteration
    fit <- glmnet(lasso_regressors, y, alpha=1, lambda=lasso_lambda_values[j], family="gaussian")
    
    # Extract coefficients for prediction
    coefficients <- coef(fit)
    
    # Predict value for the test data
    predicted_value <- coefficients[1] + sum(test_lasso_regressors * coefficients[-1])
    
    # Compute residual
    residual <- test_y - predicted_value
    
    # Store the sum of squared residuals for current iteration
    Lasso_MSE[i, j] <- residual^2
  }
}

# Computing mean of residuals for each lambda value
Lasso_mean_MSE <- apply(Lasso_MSE, MARGIN=2, FUN=mean)

# Computing ratio of Lasso MSE to RW
lasso_ratio <- Lasso_mean_MSE / mean_rw_residual_IP

# Creating plot for ratio
data <- data.frame(
  lambda = lasso_lambda_values,
  ratio = lasso_ratio
)

ggplot(data, aes(x=lambda, y=lasso_ratio)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="Lasso MSE / RW - IP", 
       x="Lambda", 
       y="MSE / Random Walk") + 
  theme_minimal()


ggsave("Lasso_IP_Plot_ratio.png", width=8, height=5)







####### PCR WITH ITERATION THROUGH DATA

# Component values to iterate over
num_components <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 78, 80, 82, 84, 86, 88, 90)  # Adjust this vector as per your choices

# Number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 131 - 12

# Matrix to store residuals for each iteration and each component value
PCR_MSE <- matrix(nrow=num_iterations, ncol=length(num_components), dimnames=list(NULL, num_components))

# Loop to iterate over number of components values
for(j in 1:length(num_components)) {
  
  # Loop to iterate over each 10 year window  
  for(i in 1:num_iterations) {
    
    # Subset for the regression
    subset_df <- standardized_ts[i:(i+119), ]
    
    # Response and predictors from the subset
    y <- standardized_ts[(i+12):(i+131), 6]
    predictors <- subset_df[, c(1:5, 7:122)]
    test_predictors <- standardized_ts[(i+131), c(1:5, 7:122)]
    test_y <- standardized_ts[(i+143), 6]
    
    # Fit PCA
    pca_result <- prcomp(predictors, center = TRUE, scale. = TRUE)
    
    # Use PCA-transformed data as regressors - Note this is Z
    pc_regressors <- pca_result$x[,1:num_components[j]]
    
    # Assume pca_result is your PCA result from prcomp - Note this is V
    rotation_matrix <- pca_result$rotation
    
    # Select the first number of components column - Note this is V adjusted
    selected_eigenvectors <- rotation_matrix[, 1:num_components[j]]
    
    # Multiply new data by selected eigenvectors - Note this is Z_2
    projected_data <- test_predictors %*% selected_eigenvectors
    
    # Fit regression model using principal components
    fit <- lm(y ~ pc_regressors)
    
    # Extract coefficients for prediction
    coefficients <- coef(fit)
    
    # Predict value for the test data
    predicted_value <- cbind(1, projected_data) %*% coefficients
    
    # Compute residual
    residual <- test_y - predicted_value
    
    # Store the sum of squared residuals for current iteration
    PCR_MSE[i, j] <- (residual^2)
  }
  print("One component done")
}

# Computing mean of residuals for each component value
PCR_mean_MSE <- apply(PCR_MSE, MARGIN=2, FUN=mean)

# Computing ratio of PCR MSE to RW
PCR_ratio <- PCR_mean_MSE / mean_rw_residual_IP

# Creating plot for ratio
data <- data.frame(
  components = num_components,
  ratio = PCR_ratio
)

ggplot(data, aes(x=components, y=PCR_ratio)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="PCR MSE / RW - IP", 
       x="Number of Components", 
       y="MSE / Random Walk") + 
  theme_minimal()


ggsave("PCR_IP_Plot_ratio.png", width=8, height=5)





####### RANDOM WALK COMPONENT 10 YEAR WINDOW

set.seed(123)

# Mumber of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 264

# Matrix to store residuals for each iteration
rw_residual <- numeric(num_iterations)

# Loop to iterate over each 10 year window
for(i in 1:num_iterations) {
  
  # Calculate the drift
  drift <- mean(diff(IP[(i+12):(i+131)]))
  
  # Take the last observation
  last_observation <- IP[(i+131)]
  
  # Start forecast with last observation
  rw_predicted_value <- last_observation
  
  # Loop to add random error component for each month along with drift
  for(j in 1:120) {
    rw_predicted_value <- rw_predicted_value + drift + rnorm(1, mean = 0, sd = sqrt(var(diff(IP[(i+12):(i+131)]))))
  }
  
  # Define actual y value for the 12 months after subset
  actual_y_value <- IP[i+143]
  
  # Calculate and store the sum of squared residual
  rw_residual[i] <- (actual_y_value - rw_predicted_value)^2
}

# Printing mean of 10 year rw_residual
mean_rw_residual_10_year_IP <- (mean(rw_residual))






####### 3PRF

# Number of iterations for loop subtracting 143 due to 10 year window and shifted y value
num_iterations <- nrow(standardized_ts) - 264

set.seed(123)

# Starting points of windows
starting_points <- c(1, 50, 100, 150, 200, 250, 300, 350, 400, 450)

# M values
M = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 45, 50, 55, 60, 65, 70, 75, 80)

# Matrix to store MSE for each iteration and each value of M
three_PRF_MSE <- matrix(nrow=length(starting_points), ncol=length(M), dimnames=list(NULL, M))

# Loop to iterate over M values
for(m in 1:length(M)) {
  
  # Reset row index for MSE matrix for next value of M
  row_index <- 1
  
  # Loop to iterate over each 10 year window
  for(i in starting_points) {
    
    # Response and predictors from the subset
    y_shifted <- standardized_ts[(i+12):(i+131), 6]
    PRF_regressors <- standardized_ts[i:(i+119), c(1:5, 7:122)]
    test_PRF_regressors <- standardized_ts[(i+120):(i+239), c(1:5, 7:122)]
    test_y_shifted <- standardized_ts[(i+132):(i+251), 6]
    
    # Loop to iterate over each integer of M
    for (k in 1:M[m]) {
      
      # Defining initial Z
      Z_initial_train <- t(matrix(y_shifted, nrow = 1))
      
      # Defining which Z to use
      if (k == 1) {Z_train <- Z_initial_train}
      else {Z_train <- Z_appended_train}
      
      ### STEP 1 TRAIN
      
      # Matrix to store the coefficients in step 1
      coefficients_df_train <- matrix(NA, ncol = (k+1), nrow = 121)
      
      # Loop to fit
      for (n in 1:ncol(PRF_regressors)) {
        fit <- lm(PRF_regressors[, n] ~ Z_train)
        coefficients_df_train[n, ] <- coef(fit)
      }
      
      # This is big Phi
      coefficients_df_train <- coefficients_df_train[, -1] # Drop the intercept column
      
      ### STEP 2 TRAIN
      
      # Matrix to store the coefficients in step 2
      step2_coefficients_df_train <- matrix(NA, ncol = (k+1), nrow = 120)
      
      # Loop to fit
      for (n in 1:nrow(step2_coefficients_df_train)) {
        fit <- lm(PRF_regressors[n, ] ~ coefficients_df_train)
        step2_coefficients_df_train[n, ] <- coef(fit)
      }
      
      # This is big F
      step2_coefficients_df_train <- step2_coefficients_df_train[, -1]
      
      ### STEP 3 TRAIN
      
      fit <- lm(y_shifted ~ step2_coefficients_df_train)
      step3_residuals <- residuals(fit)
      
      # Beta_0 and Beta_1 for test
      coefficients_train <- coef(fit)
      
      # Update regressor_matrix for the next iteration
      Z_appended_train <- cbind(Z_train, step3_residuals)
      cat("one k done: ", k)
    }
    
    ### STEP 1 TEST
    
    # Matrix to store the coefficients in step 1 test
    coefficients_df_test <- matrix(NA, ncol = (k+1), nrow = 121) 
    
    # Z to use for test
    Z_test <- Z_appended_train[,-ncol(Z_appended_train)] # Excluding the first 108 rows and the last column to make the dimensions match
    
    # Loop to fit
    for (n in 1:ncol(test_PRF_regressors)) {
      fit <- lm(test_PRF_regressors[, n] ~ Z_test) 
      coefficients_df_test[n, ] <- coef(fit)
    }
    
    # This is big Phi - test
    coefficients_df_test <- coefficients_df_test[, -1] # Drop the intercept column
    
    # STEP 2 TEST
    
    # Matrix to store the coefficients in step 2 test
    step2_coefficients_df_test <- matrix(NA, ncol = (k+1), nrow = 120)
    
    # Loop to fit
    for (n in 1:nrow(step2_coefficients_df_test)) {
      fit <- lm(test_PRF_regressors[n, ] ~ coefficients_df_test)
      step2_coefficients_df_test[n, ] <- coef(fit)
    }
    
    # This is big F - test
    step2_coefficients_df_test <- step2_coefficients_df_test[, -1]
    
    # No Step 3, just prediction for current M and current window
    predicted_value <- coefficients_train[1] + step2_coefficients_df_test %*% coefficients_train[-1]
    
    # Compute residual
    residual <- test_y_shifted - predicted_value
    
    residual_squared <- residual^2
    
    # Store the sum of squared residuals for current iteration
    three_PRF_MSE[row_index, m] <- (mean(residual_squared))
    
    # Moving index up one to store MSE of the next iteration
    row_index <- row_index + 1
  }
  
}

# Computing the 3PRF MSE
PRF_mean_MSE <- apply(three_PRF_MSE, MARGIN=2, FUN=mean)

# Computing the ratio of the 3PRF MSE to 10 year RW
PRF_ratio <- PRF_mean_MSE / mean_rw_residual_10_year_IP

# Creating plot for ratio
data <- data.frame(
  M_values = M,
  ratio = PRF_ratio
)

ggplot(data, aes(x=M_values, y=PRF_ratio)) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title="PRF MSE / RW - IP", 
       x="Number of Factors", 
       y="MSE / Random Walk") + 
  theme_minimal()


ggsave("3PRF_IP_Plot_ratio.png", width=8, height=5)

