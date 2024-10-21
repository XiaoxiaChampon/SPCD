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
#random non-responders
set.seed(123)  # For reproducibility

# Number of participants
n <- 300  # Total number of participants

# Stage 1 Treatment assignment: 1/3 to A, 1/3 to B, 1/3 to Placebo
#treatment_stage1 <- factor(sample(c("A", "B", "Placebo"), n, replace = TRUE, prob = c(1/3, 1/3, 1/3)))
treatment_stage1 <- factor(sample(c("Placebo", "B", "A"), n, replace = TRUE, prob = c(1/3, 1/3, 1/3)))

# Define the effects of the treatments in Stage 1 for continuous response
#beta_0 <- 50  # Intercept
beta_0 <- 2  # Intercept
#effect_A <- 10
#effect_B <- 8
# effect_Placebo <- 2

effect_Placebo <- 10
effect_B <- 8
effect_A <- 2

# Continuous response for Stage 1
# response_stage1_continuous <- beta_0 +
#   ifelse(treatment_stage1 == "A", effect_A, ifelse(treatment_stage1 == "B", effect_B, effect_Placebo)) +
#   rnorm(n, 0, 5)

response_stage1_continuous <- beta_0 +
  ifelse(treatment_stage1 == "A", effect_A, ifelse(treatment_stage1 == "B", effect_B, effect_Placebo)) * rnorm(n, 0, 5)

# Binary response probabilities in Stage 1 (responder = 1, non-responder = 0)
#p_stage1 <- ifelse(treatment_stage1 == "A", 0.8, ifelse(treatment_stage1 == "B", 0.7, 0.5))
p_stage1 <- exp(response_stage1_continuous)/(1+exp(response_stage1_continuous))
response_stage1_binary <- rbinom(n, 1, p_stage1)

# Identify non-responders in the Placebo group in Stage 1
placebo_nonresponders <- (treatment_stage1 == "Placebo") & (response_stage1_binary == 0)

# Number of non-responders from Placebo group
n_nonresponders <- sum(placebo_nonresponders)

# Stage 2 Treatment assignment for non-responders in Placebo group: 1/3 to A, 1/3 to B, 1/3 to Placebo
treatment_stage2 <- rep(NA, n)
treatment_stage2[placebo_nonresponders] <- factor(sample(c("A", "B", "Placebo"), n_nonresponders, replace = TRUE, prob = c(1/3, 1/3, 1/3)))

# Generate continuous response in Stage 2 for non-responders
response_stage2_continuous <- rep(NA, n)
# response_stage2_continuous[placebo_nonresponders] <- beta_0 +
#   ifelse(treatment_stage2[placebo_nonresponders] == "A", effect_A, ifelse(treatment_stage2[placebo_nonresponders] == "B", effect_B, effect_Placebo)) +
#   rnorm(n_nonresponders, 0, 5)

response_stage2_continuous[placebo_nonresponders] <- beta_0 +
  ifelse(treatment_stage2[placebo_nonresponders] == "A", effect_A, ifelse(treatment_stage2[placebo_nonresponders] == "B", effect_B, effect_Placebo)) *
  rnorm(n_nonresponders, 0, 5)

# Binary response probabilities in Stage 2 for non-responders
p_stage2 <- rep(NA, n)
#p_stage2[placebo_nonresponders] <- ifelse(treatment_stage2[placebo_nonresponders] == "A", 0.8, ifelse(treatment_stage2[placebo_nonresponders] == "B", 0.7, 0.5))

p_stage2[placebo_nonresponders] <- exp(response_stage2_continuous[placebo_nonresponders])/(1+exp(response_stage2_continuous[placebo_nonresponders]))
response_stage2_binary <- rep(NA, n)
response_stage2_binary[placebo_nonresponders] <- rbinom(n_nonresponders, 1, p_stage2[placebo_nonresponders])

# Combine data into a data frame
data <- data.frame(
  Subject_ID = 1:n,
  Treatment_Stage1 = treatment_stage1,
  Response_Stage1_Continuous = response_stage1_continuous,
  Response_Stage1_Binary = response_stage1_binary,
  Placebo_NonResponder = placebo_nonresponders,
  Treatment_Stage2 = treatment_stage2,
  Response_Stage2_Continuous = response_stage2_continuous,
  Response_Stage2_Binary = response_stage2_binary
)

# Preview the first few rows of the data
head(data)

par(mfrow=c(2,2))
hist(data[data$Treatment_Stage1=="A","Response_Stage1_Continuous"],xlab="Value",,main="Response Stage 1 Trt A")
hist(data[data$Treatment_Stage1=="B","Response_Stage1_Continuous"],xlab="Value",,main="Response Stage 1 Trt B")
hist(data[data$Treatment_Stage1=="Placebo","Response_Stage1_Continuous"],xlab="Value",,main="Response Stage 1 Placebo")
hist(data$Response_Stage1_Continuous,xlab="Value",,main="Response Stage 1 ")
####################################
########binary outcome
#Response rate for A
resp_rate_A <- sum(data[data$Treatment_Stage1=="A","Response_Stage1_Binary"])/length(data[data$Treatment_Stage1=="A","Response_Stage1_Binary"])
resp_rate_A
# 0.552381
#105, 58

## Obtain the propensity scores (predicted probabilities)

index_A <- which(data$Treatment_Stage1=="A")

library(nnet)
# Fit the multinomial regression model using the nnet package
data_prop <- data.frame(treatment_stage1,beta_0,(response_stage1_continuous-beta_0) )
colnames(data_prop) <- c("trt","intercept_const","X1")
prop_model <- multinom(trt ~ intercept_const + X1, data = data_prop)
predicted_probabilities <- prop_model$fitted.values
prop_score_A <- predicted_probabilities[index_A]
hist(prop_score_A)
 mean(prop_score_A)
# [1] 0.3519459


resp_rate_B <- sum(data[data$Treatment_Stage1=="B","Response_Stage1_Binary"])/length(data[data$Treatment_Stage1=="B","Response_Stage1_Binary"])
resp_rate_B
#0.5959596
#59, 99

index_B <- which(data$Treatment_Stage1=="B")
prop_score_B <- predicted_probabilities[index_B]
hist(prop_score_B)
mean(prop_score_B)
#0.3480523

resp_rate_Placebo <- sum(data[data$Treatment_Stage1=="Placebo","Response_Stage1_Binary"])/length(data[data$Treatment_Stage1=="Placebo","Response_Stage1_Binary"])
resp_rate_Placebo
#0.4583333
#44, 96

index_P <- which(data$Treatment_Stage1=="Placebo")
prop_score_P <- predicted_probabilities[index_P]
hist(prop_score_P)
mean(prop_score_P)
#0.3498803


######
par(mfrow=c(1,3))
hist(prop_score_A,xlab="Value",,main="Propensity Score Stage 1 Trt A")
hist(prop_score_B,xlab="Value",,main="Propensity Score Stage 1 Trt B")
hist(prop_score_P,xlab="Value",,main="Propensity Score Stage 1 Placebo")

#############


########continuous outcome
trt_A <- mean(data[data$Treatment_Stage1=="A","Response_Stage1_Continuous"])
trt_A
# 1.054524

trt_B <-  mean(data[data$Treatment_Stage1=="B","Response_Stage1_Continuous"])
trt_B
#6.751512

trt_Placebo <-  mean(data[data$Treatment_Stage1=="Placebo","Response_Stage1_Continuous"])
trt_Placebo

#4.735334
###############################

final_data <- data[!is.na(data$Treatment_Stage2),]

hist(final_data$Response_Stage2_Continuous)
table(final_data$Treatment_Stage2)
# 1  2  3 
# 19 16 15 
length(final_data$Response_Stage2_Continuous)
#50
table(final_data$Response_Stage2_Binary)
# 0  1 
#21 29 

resp_rate_A_final <- sum(final_data[final_data$Treatment_Stage2=="1","Response_Stage2_Binary"])/length(final_data[final_data$Treatment_Stage2=="1","Response_Stage2_Binary"])
resp_rate_A_final
# 0.5263158
#10, 19

index_A_final <- which(data$Treatment_Stage2=="1")
data_prop_final <- data.frame(treatment_stage2,beta_0,(response_stage2_continuous-beta_0) )
colnames(data_prop_final) <- c("trt","intercept_const","X1")
prop_model_final <- multinom(trt ~ intercept_const + X1, data = data_prop_final)
predicted_probabilities_final <- prop_model_final$fitted.values
prop_score_A_final <- predicted_probabilities[index_A_final]
hist(prop_score_A_final)
mean(prop_score_A_final)
#0.3805412

resp_rate_B_final <- sum(final_data[final_data$Treatment_Stage2=="2","Response_Stage2_Binary"])/length(final_data[final_data$Treatment_Stage2=="2","Response_Stage2_Binary"])
resp_rate_B_final
#0.75
#12, 16


index_B_final <- which(data$Treatment_Stage2=="2")
prop_score_B_final <- predicted_probabilities[index_B_final]
hist(prop_score_B_final)
mean(prop_score_B_final)
#0.3841446

resp_rate_Placebo_final <- sum(final_data[final_data$Treatment_Stage2=="3","Response_Stage2_Binary"])/length(final_data[final_data$Treatment_Stage2=="3","Response_Stage2_Binary"])
resp_rate_Placebo_final
#0.4666667
#7, 15



index_P_final <- which(data$Treatment_Stage2=="3")
prop_score_P_final <- predicted_probabilities[index_P_final]
hist(prop_score_P_final)
mean(prop_score_P_final)
#0.3708361
###############
trt_A_final <- mean(na.omit(data[data$Treatment_Stage2=="1","Response_Stage2_Continuous"]))
trt_A_final
# 2.576261

trt_B_final <-  mean(na.omit(data[data$Treatment_Stage2=="2","Response_Stage2_Continuous"]))
trt_B_final 
#10.11395

trt_Placebo_final <- mean(na.omit(data[data$Treatment_Stage2=="3","Response_Stage2_Continuous"]))
trt_Placebo_final
#6.810049


par(mfrow=c(2,2))
hist(data[data$Treatment_Stage2=="1","Response_Stage2_Continuous"],xlab="Value",,main="Response Stage 2 Trt A")
hist(data[data$Treatment_Stage2=="2","Response_Stage2_Continuous"],xlab="Value",,main="Response Stage 2 Trt B")
hist(data[data$Treatment_Stage2=="3","Response_Stage2_Continuous"],xlab="Value",,main="Response Stage 2 Placebo")
hist(data$Response_Stage2_Continuous,xlab="Value",,main="Response Stage 2 ")

par(mfrow=c(1,3))
hist(prop_score_A_final,xlab="Value",,main="Propensity Score Stage 2 Trt A")
hist(prop_score_B_final,xlab="Value",,main="Propensity Score Stage 2 Trt B")
hist(prop_score_P_final,xlab="Value",,main="Propensity Score Stage 2 Placebo")

#############################################################################################
##############################################################################################
# Load required libraries
library(dplyr)

# Set random seed for reproducibility
set.seed(123)

# Define parameters
n <- 300  # Total number of subjects
n_groups <- 3  # Two treatments and one placebo
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
treatment_effect_cont_stage1 <- c(0, 1, 1.5)   # Continuous response effects


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

sum(covariates$binary_response_stage1)
# Identify non-responders in Stage 1 (placebo group, no response)
non_responders <- covariates %>%
  filter(treatment_stage1 == 0 & binary_response_stage1 == 0)  # Non-responders from placebo group



# Stage 2: Re-randomize non-responders to two treatments and placebo
n_non_responders <- nrow(non_responders)
non_responders$treatment_stage2 <- rep(c(0, 1, 2), length.out = n_non_responders)  # Re-randomize equally

# Define treatment effects for Stage 2
treatment_effect_bin_stage2 <- c(0, 6, 10)  # Binary response effects in Stage 2
treatment_effect_cont_stage2 <- c(0, 3, 4.5)  # Continuous response effects in Stage 2

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

sum(non_responders$binary_response_stage2)

hist(lin_pred_stage2)

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

# Set "1" as the reference level
non_responders$treatment_stage2 <- relevel(as.factor(non_responders$treatment_stage2), ref = "0")
pooled_binary_model <- glm(binary_response_stage2 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2, 
                           family = binomial(link = "logit"), data = non_responders)
summary(pooled_binary_model)
# Call:
#   glm(formula = binary_response_stage2 ~ binary_cov1 + binary_cov2 + 
#         continuous_cov1 + continuous_cov2 + treatment_stage2, family = binomial(link = "logit"), 
#       data = non_responders)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)        -2425.58  509002.18  -0.005    0.996
# binary_cov1          -28.20   98801.67   0.000    1.000
# binary_cov2         -211.30   45356.02  -0.005    0.996
# continuous_cov1       36.10    7376.40   0.005    0.996
# continuous_cov2       67.76   13812.85   0.005    0.996
# treatment_stage21    356.23   74106.56   0.005    0.996
# treatment_stage22    747.62  153188.31   0.005    0.996
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 6.7417e+01  on 48  degrees of freedom
# Residual deviance: 4.8135e-08  on 42  degrees of freedom
# AIC: 14
# 
# Number of Fisher Scoring iterations: 25

predicted_prob <- predict(pooled_binary_model, type = "response")

index_0 <- which(non_responders$treatment_stage2=="0")
index_1 <- which(non_responders$treatment_stage2=="1")
index_2 <- which(non_responders$treatment_stage2=="2")

# control_p <- sum(predicted_prob[index_0])/length(index_0)
# trt1_p <- sum(predicted_prob[index_1])/length(index_1)
# trt2_p <- sum(predicted_prob[index_2])/length(index_2)
# control_p
# trt1_p
# trt2_p

successes_1 <- c(sum(predicted_prob[index_0]), sum(predicted_prob[index_1]))
# Total number of trials in each group
totals_1 <- c(length(index_0), length(index_1))
test_result_1 <- prop.test(successes_1, totals_1)
# Perform the two-proportion z-test
#test_result <- prop.test(control_p, trt1_p )
test_result_1$p.value 


successes_2 <- c(sum(predicted_prob[index_0]), sum(predicted_prob[index_2]))
# Total number of trials in each group
totals_2 <- c(length(index_0), length(index_2))
test_result_2 <- prop.test(successes_2, totals_2)
# Perform the two-proportion z-test
#test_result <- prop.test(control_p, trt1_p )
test_result_2$p.value 

# sum(predicted_prob)/dim(non_responders)[1]
# 
# sum(non_responders$binary_response_stage2)/dim(non_responders)[1]



# ------------------- Continuous Response Comparison in Stage 2 -------------------
# Use mapping function for continuous response analysis (e.g., log transformation)

# Mapping function for continuous response
mapping_function <- function(Y_cont) {
  return(log(Y_cont + 1))
}

mapping_function_2 <- function(Y_cont) {
  return(-exp(Y_cont))
}


# Apply mapping function
non_responders$mapped_continuous_response_stage2 <- mapping_function(non_responders$continuous_response_stage2)
non_responders$mapped_continuous_response_stage22 <- mapping_function_2(non_responders$continuous_response_stage2)

#hist(non_responders$mapped_continuous_response_stage2)

# Analyze mapped continuous response with linear regression
mapped_cont_model <- lm(mapped_continuous_response_stage2 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2,
                        data = non_responders)
#summary(mapped_cont_model)

group_0 <- mapped_cont_model$fitted.values[index_0]
group_1 <- mapped_cont_model$fitted.values[index_1]
group_2 <- mapped_cont_model$fitted.values[index_2]
# Perform the two-sample t-test
t_test_result_1 <- t.test(group_0, group_1)
t_test_result_1$p.value

t_test_result_2 <- t.test(group_0, group_2)
t_test_result_2$p.value

# Call:
#   lm(formula = mapped_continuous_response_stage2 ~ binary_cov1 + 
#        binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2, 
#      data = non_responders)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.060705 -0.021937  0.001227  0.018481  0.070810 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        2.780528   0.024118 115.290  < 2e-16 ***
#   binary_cov1        0.028530   0.009292   3.070  0.00374 ** 
#   binary_cov2       -0.027057   0.009315  -2.905  0.00584 ** 
#   continuous_cov1    0.018721   0.000565  33.135  < 2e-16 ***
#   continuous_cov2    0.029255   0.000633  46.218  < 2e-16 ***
#   treatment_stage21  0.072070   0.011586   6.221 1.91e-07 ***
#   treatment_stage22  0.089937   0.011538   7.795 1.09e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.03142 on 42 degrees of freedom
# Multiple R-squared:  0.9838,	Adjusted R-squared:  0.9814 
# F-statistic: 424.1 on 6 and 42 DF,  p-value: < 2.2e-16

# ------------------- Interpretation -------------------
# Compare the pooled logistic regression model for binary response and linear model for continuous response

#######################################
#install.packages("RBesT")
library(RBesT)

# Example: setting up priors for treatment and control arms

control_prior <- mixnorm(c(1, 0, 1), sigma = 1)
treatment_prior <- mixnorm(c(1, 0, 1), sigma = 1)

# Data for the control and treatment arms (e.g., success counts)
data_control <- c(15, 100)  # 15 successes out of 100 trials
data_treatment <- c(20, 100)  # 20 successes out of 100 trials

# Posterior distributions for each arm
posterior_control <- postmix(control_prior, data_control)
posterior_treatment <- postmix(treatment_prior, data_treatment)

# Compare the two posteriors
summary(posterior_control)
summary(posterior_treatment)

summary(posterior_control)
# mean         sd       2.5%      50.0%      97.5% 
# 0.0318163  0.9997233 -1.9276054  0.0318163  1.9912380 
# > summary(posterior_treatment)
# mean          sd        2.5%       50.0%       97.5% 
# 0.03747658  0.99968765 -1.92187521  0.03747658  1.99682836 


# Load RBesT package
library(RBesT)

#binary
# Define prior distributions for the two groups (Treatment A and Treatment B)
prior_A <- mixnorm(c(1, 0, 1), sigma = 1)   # Normal prior for Treatment A
prior_B <- mixnorm(c(1, 0, 1), sigma = 1)   # Normal prior for Treatment B

# Sample data for the two groups: (successes, trials)
data_A <- c(18, 50)   # 18 successes out of 50 for Treatment A
data_B <- c(12, 50)   # 12 successes out of 50 for Treatment B

# Compute posteriors for both treatments
posterior_A <- postmix(prior_A, data_A)
posterior_B <- postmix(prior_B, data_B)

# Hypothesis Test: Probability that Treatment A is better than Treatment B
# Compare posteriors using a threshold
diff_posterior <- postmix(prior_A, data_A) - postmix(prior_B, data_B)

# Compute the probability that Treatment A is better than Treatment B
prob_A_better <- pmix(diff_posterior, 0)

# Print the probability
print(prob_A_better)

# Decision rule: if prob_A_better > 0.95, reject null hypothesis
if (prob_A_better > 0.95) {
  print("Reject the null hypothesis: Treatment A is better than Treatment B")
} else {
  print("Fail to reject the null hypothesis: No strong evidence that Treatment A is better")
}

#continuous
# Set up priors for the two groups
# Using normal priors with mean and standard deviation
# Assume data for group 1 and group 2 are normally distributed

prior1 <- mixnorm(c(1, 0, 1), sigma = 1)
prior2 <- mixnorm(c(1, 0, 1), sigma = 1)

# Define the data from two groups (assuming we have two vectors of continuous responses)
group1 <- c(4.2, 5.1, 6.3, 5.9, 6.1)
group2 <- c(3.5, 4.0, 4.8, 5.0, 5.3)

# Perform the Bayesian analysis for group 1 and group 2 using the defined priors
post1 <- postmix(prior1, m = mean(group1), se = sd(group1)/sqrt(length(group1)))
post2 <- postmix(prior2, m = mean(group2), se = sd(group2)/sqrt(length(group2)))

# Hypothesis testing: Compare the posterior distributions of the two groups
# Compute the probability that group 1 mean > group 2 mean
prob_diff <- 1 - pmixdiff(post1, post2, 0)

# Output the result
print(paste("Probability that Group 1 has a higher mean than Group 2:", round(prob_diff, 4)))


