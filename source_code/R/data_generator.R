# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the “Software”), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:

# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
# OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
# THE USE OR OTHER DEALINGS IN THE SOFTWARE.
######################################################################
#
# Purpose: Data Generating Process for Sequential Parallel Comparison Design
#         
# Author:  Xiaoxia Champon
# Date: 09/23/2024
#
##############################################################

# Load required libraries
library(dplyr)

# Set random seed for reproducibility
#set.seed(123)

# Mapping function for continuous response
mapping_function <- function(Y_cont) {
  return(log(Y_cont + 1))
}

mapping_function_2 <- function(Y_cont) {
  return(-exp(Y_cont))
}


spcd_data <- function(n, n_groups, diff_stage1, diff_stage2){
  # Define parameters
  # n <- 300  # Total number of subjects
  # n_groups <- 3  # Two treatments and one placebo
  n_subjects_per_group <- n / n_groups  # Subjects per group
  p_bin <- 0.5  # Probability for binary covariates
  covar1_mean <- 10  # Mean for continuous covariate 1
  covar2_mean <- 35  # Mean for continuous covariate 2
  covar_sd <- 10  # Standard deviation for continuous covariates
  
  # Simulate binary and continuous covariates
  covariates <- data.frame(
    binary_cov1 = rbinom(n, 1, p_bin),
    #binary_cov2 = sample(1:3, size=n, replace = TRUE),
    binary_cov2 = rbinom(n, 1, p_bin),
    continuous_cov1 = rnorm(n, covar1_mean, covar_sd),
    continuous_cov2 = rnorm(n, covar2_mean, covar_sd)
  )
  
  # Assign subjects equally to two treatment arms and a placebo group in Stage 1
  covariates$treatment_stage1 <- rep(c(0, 1, 2), each = n_subjects_per_group)  # 0 = placebo, 1 = treatment 1, 2 = treatment 2
  
  # Define treatment effects for Stage 1 (binary and continuous responses)
  treatment_effect_bin_stage1 <- c(0, 4, 7)  # Binary response effects
  treatment_effect_cont_stage1 <- c(0, 1, 1+diff_stage1)   # Continuous response effects
  
  
  # Simulate continuous response in Stage 1
  beta_cont_stage1 <- c(2, 1.5, -0.5, 0.8, 1.2)  # Coefficients for continuous response
  covariates$continuous_response_stage1 <- beta_cont_stage1[1] +
    beta_cont_stage1[2] * covariates$binary_cov1 +
    beta_cont_stage1[3] * covariates$binary_cov2 +
    beta_cont_stage1[4] * covariates$continuous_cov1 +
    beta_cont_stage1[5] * covariates$continuous_cov2 +
    treatment_effect_cont_stage1[covariates$treatment_stage1 + 1] +  # Adding treatment effect
    rnorm(n, 0, 1)  # Random noise
  
  # Simulate binary response in Stage 1
  beta_bin_stage1 <- c(-1, 1, -0.5, 0.7, 1.1)  # Coefficients for binary response
  lin_pred_stage1 <- beta_bin_stage1[1]-45 +
    beta_bin_stage1[2] * covariates$binary_cov1 +
    beta_bin_stage1[3] * covariates$binary_cov2 +
    beta_bin_stage1[4] * covariates$continuous_cov1 +
    beta_bin_stage1[5] * covariates$continuous_cov2 +
    treatment_effect_bin_stage1[covariates$treatment_stage1 + 1]  # Treatment effect
  covariates$binary_response_stage1 <- rbinom(n, 1, plogis(lin_pred_stage1))  # Logistic transformation
  
  #sum(covariates$binary_response_stage1)
  # Identify non-responders in Stage 1 (placebo group, no response)
  non_responders <- covariates %>%
    filter(treatment_stage1 == 0 & binary_response_stage1 == 0)  # Non-responders from placebo group
  
  # Stage 2: Re-randomize non-responders to two treatments and placebo
  n_non_responders <- nrow(non_responders)
  non_responders$treatment_stage2 <- rep(c(0, 1, 2), length.out = n_non_responders)  # Re-randomize equally # 0 = placebo, 1 = treatment 1, 2 = treatment 2
  
  # Define treatment effects for Stage 2
  treatment_effect_bin_stage2 <- c(0, 6, 10)  # Binary response effects in Stage 2
  treatment_effect_cont_stage2 <- c(0, 3, 3+diff_stage2)  # Continuous response effects in Stage 2
  
  # Simulate continuous response in Stage 2 for non-responders
  non_responders$continuous_response_stage2 <- beta_cont_stage1[1] +
    beta_cont_stage1[2] * non_responders$binary_cov1 +
    beta_cont_stage1[3] * non_responders$binary_cov2 +
    beta_cont_stage1[4] * non_responders$continuous_cov1 +
    beta_cont_stage1[5] * non_responders$continuous_cov2 +
    treatment_effect_cont_stage2[non_responders$treatment_stage2 + 1] +  # Treatment effect
    rnorm(n_non_responders, 0, 1)  # Random noise
  
  # Simulate binary response in Stage 2 for non-responders
  lin_pred_stage2 <- beta_bin_stage1[1]-40 +
    beta_bin_stage1[2] * non_responders$binary_cov1 +
    beta_bin_stage1[3] * non_responders$binary_cov2 +
    beta_bin_stage1[4] * non_responders$continuous_cov1 +
    beta_bin_stage1[5] * non_responders$continuous_cov2 +
    treatment_effect_bin_stage2[non_responders$treatment_stage2 + 1]  # Treatment effect
  non_responders$binary_response_stage2 <- rbinom(n_non_responders, 1, plogis(lin_pred_stage2))  # Logistic transformation
  
  
  #sum(non_responders$binary_response_stage2)
  
  #hist(lin_pred_stage2)
  
  # ------------------- Binary Response Comparison in Stage 2 -------------------
  # Pooling method with shared internal control arm (from placebo group in Stage 1)
  
  # Combine non-responders' binary responses and historical data from Stage 1 placebo group
  historical_control <- covariates %>%
    filter(treatment_stage1 == 0) %>%
    select(binary_response_stage1)
  
  # Pooling: Create a pooled dataset from Stage 2 and historical controls
  pooled_binary_data <- rbind(
    non_responders %>% select(binary_response_stage2)#,
    #historical_control %>% rename(binary_response_stage1 = binary_response_stage1)
  )
  
  # Perform logistic regression on pooled data
  
  
  
  # ------------------- Continuous Response Comparison in Stage 2 -------------------
  # Use mapping function for continuous response analysis (e.g., log transformation)
  

  
  # Apply mapping function
  non_responders$mapped_continuous_response_stage2 <- mapping_function(non_responders$continuous_response_stage2)
  non_responders$mapped_continuous_response_stage22 <- mapping_function_2(non_responders$continuous_response_stage2)
  
  return("spcd_data" <- non_responders)
}
