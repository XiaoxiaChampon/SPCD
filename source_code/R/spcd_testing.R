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
# Purpose: Hypothesis Test for Sequential Parallel Comparison Design
#         
# Author:  Xiaoxia Champon
# Date: 09/23/2024
#
##############################################################
# names(non_responders)
# 
# [1] "binary_cov1"                       "binary_cov2"                       "continuous_cov1"                  
# [4] "continuous_cov2"                   "treatment_stage1"                  "continuous_response_stage1"       
# [7] "binary_response_stage1"            "treatment_stage2"                  "continuous_response_stage2"       
# [10] "binary_response_stage2"            "mapped_continuous_response_stage2"


#Z_function <- function ( diff_stage1_trt, diff_stage2_trt, w_weight = 0.5) {
Z_function <- function (diff_stage1, diff_stage2, diff_stage1_trt, diff_stage2_trt, w_weight) {
  
  # Z_value <- (w_weight * mean (diff_stage1_trt) + (1 - w_weight) * mean(diff_stage2_trt))/sqrt (w_weight^2 * (sd(diff_stage1_trt)/sqrt(length(diff_stage1_trt)))^2 +
  #                                                                                                 #2 * w_weight * (1 - w_weight) *cov(diff_stage1_trt, diff_stage2_trt)+
  #                                                                                                 (1 - w_weight)^2*(sd(diff_stage2_trt)/sqrt(length(diff_stage2_trt)))^2
  #                                                                                         )
  Z_value <- (w_weight * diff_stage1 + (1 - w_weight) * diff_stage2)/sqrt (w_weight^2 * (sd(diff_stage1_trt)/sqrt(length(diff_stage1_trt)))^2 +
                                                                                                  #2 * w_weight * (1 - w_weight) *cov(diff_stage1_trt, diff_stage2_trt)+
                                                                                                  (1 - w_weight)^2*(sd(diff_stage2_trt)/sqrt(length(diff_stage2_trt)))^2
  )
  return(Z_value)
}

#add the bias from the data

hypothesis_testing <- function(non_responders, covariates, trtA_effect_stage1, trtA_effect_stage2, diff_stage1, diff_stage2 , num_replicas, w_weight){
  # Perform logistic regression on pooled data
  #non_responders <- example_data$spcd_data_yes
  # Set "0" as the reference level
  non_responders$treatment_stage2 <- relevel(as.factor(non_responders$treatment_stage2), ref = "0")
  pooled_binary_model <- glm(binary_response_stage2 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2,
                             family = binomial(link = "logit"), data = non_responders)

  predicted_prob <- predict(pooled_binary_model, type = "response")

  index_0 <- which(non_responders$treatment_stage2=="0")
  index_1 <- which(non_responders$treatment_stage2=="1")
  index_2 <- which(non_responders$treatment_stage2=="2")
  # 
  # 
  successes_1 <- c(sum(predicted_prob[index_0]), sum(predicted_prob[index_1]))
  # Total number of trials in each group
  totals_1 <- c(length(index_0), length(index_1))
  test_result_1 <- prop.test(successes_1, totals_1)
  # Perform the two-proportion z-test
  #test_result_1$p.value


  successes_2 <- c(sum(predicted_prob[index_0]), sum(predicted_prob[index_2]))
  # Total number of trials in each group
  totals_2 <- c(length(index_0), length(index_2))
  test_result_2 <- prop.test(successes_2, totals_2)
  # # Perform the two-proportion z-test
  # #test_result_2$p.value
  # 
   binary_result <- c(test_result_1$p.value , test_result_2$p.value)
 
  # 
  #################bayesian binary
  #binary
  # Define prior distributions for the two groups (Treatment A and Treatment B)
  # prior_A <- mixnorm(c(1, 0, 1), sigma = 1)   # Normal prior for Treatment A
  # prior_B <- mixnorm(c(1, 0, 1), sigma = 1)   # Normal prior for Treatment B
  # 
  # # Sample data for the two groups: (successes, trials)
  # data_A <- c(18, 50)   # 18 successes out of 50 for Treatment A
  # data_B <- c(12, 50)   # 12 successes out of 50 for Treatment B
  # 
  # # Compute posteriors for both treatments
  # posterior_A <- postmix(prior_A, data_A)
  # posterior_B <- postmix(prior_B, data_B)
  # 
  # # Hypothesis Test: Probability that Treatment A is better than Treatment B
  # # Compare posteriors using a threshold
  # diff_posterior <- postmix(prior_A, data_A) - postmix(prior_B, data_B)
  # 
  # # Compute the probability that Treatment A is better than Treatment B
  # prob_A_better <- pmix(diff_posterior, 0)
  ########################################################################
  
  

  # ------------------- Continuous Response Comparison in Stage 2 -------------------
  # Use mapping function for continuous response analysis (e.g., log transformation)
 
  
  # Analyze mapped continuous response with linear regression
cont_model <- lm(continuous_response_stage2 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2,
                          data = non_responders)
  
   cont_model_stage1 <- lm(continuous_response_stage1 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage1,
                           data = covariates)
  #summary(mapped_cont_model)
  
  group_0 <- cont_model$fitted.values[index_0]
  group_1 <- cont_model$fitted.values[index_1]
  group_2 <- cont_model$fitted.values[index_2]
  # Perform the two-sample t-test
  #t_test_result_1 <- t.test(group_0, group_1)
  #Z_function <- function (diff_stage1_trt, diff_stage2_trt, w_weight = 0.5)
  
  bias_trtA <- w_weight*cont_model$coefficients[6]-(1-w_weight)*cont_model_stage1$coefficients[6]
  bias_trtB <- w_weight*cont_model$coefficients[7]-(1-w_weight)*cont_model_stage1$coefficients[7]
  bias_comb <- c( bias_trtA,  bias_trtB)
  if (num_replicas == 1000){
    t_test_result_1 <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                                non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                group_1 - group_0,
                                                w_weight))
    #t_test_result_1$p.value
    
    #t_test_result_2 <- t.test(group_0, group_2)
    
    t_test_result_2 <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                                non_responders$continuous_response_stage1[index_2]-non_responders$continuous_response_stage1[index_0] ,
                                                group_2 - group_0,
                                                w_weight))
  }
  
  if (num_replicas == 5000){
    t_test_result_1 <- pnorm( Z_function(mean(non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                         mean(group_1 - group_0 ),
                                                non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                group_1 - group_0,
                                         w_weight))
    #t_test_result_1$p.value
    
    #t_test_result_2 <- t.test(group_0, group_2)
    
    t_test_result_2 <- pnorm( Z_function(mean(non_responders$continuous_response_stage1[index_2]-non_responders$continuous_response_stage1[index_0]), 
                                         mean(group_2 - group_0),
                                                non_responders$continuous_response_stage1[index_2]-non_responders$continuous_response_stage1[index_0] ,
                                                group_2 - group_0,
                                         w_weight))
  }
  
  #t_test_result_2$p.value
  
  continuous_result <- c(t_test_result_1, t_test_result_2)
  
  #####################add bayesian
  # Define prior distributions for the two groups (Treatment A and Treatment B)
  # Set up priors for the two groups
  # Using normal priors with mean and standard deviation
  # Assume data for group 1 and group 2 are normally distributed
  prior_placebo <- mixnorm(c(1, 0, 1), sigma = 1)   # Normal prior for Treatment A
  # prior_A <- mixnorm(c(trtA_effect, 0, trtA_effect), sigma = 1)   # Normal prior for Treatment A
  # prior_B <- mixnorm(c(trtA_effect+diff_stage2, 0, trtA_effect+diff_stage2), sigma = 1)   # Normal prior for Treatment B
  prior_A <- mixnorm(c(1, 0, 1), sigma = 1)   # Normal prior for Treatment A
  prior_B <- mixnorm(c(1, 0, 1), sigma = 1)   # Normal prior for Treatment B
  

  # Define the data from two groups (assuming we have two vectors of continuous responses)
  #group_0,group_1, group_2

  # Perform the Bayesian analysis for group 1 and group 2 using the defined priors
  post_placebo <- postmix(prior_placebo, m = mean( group_0), se = sd( group_0)/sqrt(length( group_0)))
  post_trtA <- postmix(prior_A, m = mean( group_1), se = sd( group_1)/sqrt(length( group_1)))
  post_trtB <- postmix(prior_B, m = mean( group_2), se = sd( group_2)/sqrt(length( group_2)))

  # Hypothesis testing: Compare the posterior distributions of the two groups
  # Compute the probability that group 1 mean > group 2 mean
  # prob_diff_A <- 1 - pmixdiff(post_placebo, post_trtA, 0)
  # prob_diff_B <- 1 - pmixdiff(post_placebo, post_trtB, 0)
  # # Output the result
  # print(paste("Probability that Group 1 has a higher mean than Group 2:", round(prob_diff, 4)))

  # prob_diff_A <-  pmixdiff(post_placebo, post_trtA, 0)
  # prob_diff_B <-  pmixdiff(post_placebo, post_trtB, 0)
  if (num_replicas == 1000){
  prob_diff_A <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                          non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                          post_trtA - post_placebo,
                                          w_weight))
  prob_diff_B <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                          non_responders$continuous_response_stage1[index_2]-non_responders$continuous_response_stage1[index_0] ,
                                          post_trtB - post_placebo,
                                          w_weight))
  }
  
  if (num_replicas == 5000){
    prob_diff_A <- pnorm(Z_function(mean(non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]),
                                    mean(post_trtA - post_placebo),
                                            non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                            post_trtA - post_placebo,
                                    w_weight))
    prob_diff_B <- pnorm(Z_function(mean(non_responders$continuous_response_stage1[index_2]-non_responders$continuous_response_stage1[index_0]), 
                                    mean( post_trtB - post_placebo),
                                            non_responders$continuous_response_stage1[index_2]-non_responders$continuous_response_stage1[index_0] ,
                                            post_trtB - post_placebo,
                                    w_weight))
  }
  continuous_result_bayesian <- c(prob_diff_A, prob_diff_B)
  # 
  ####################################################################
  
  
  
  # Analyze mapped continuous response with linear regression
  mapped_cont_model <- lm(mapped_continuous_response_stage2 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2,
                          data = non_responders)
  #summary(mapped_cont_model)
  
  group_0 <- mapped_cont_model$fitted.values[index_0]
  group_1 <- mapped_cont_model$fitted.values[index_1]
  group_2 <- mapped_cont_model$fitted.values[index_2]
  # Perform the two-sample t-test
  # t_test_result_21 <- t.test(group_0, group_1)
  # #t_test_result_1$p.value
  # 
  # t_test_result_22 <- t.test(group_0, group_2)
  if (num_replicas == 1000){
  t_test_result_21 <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                               non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                               group_1 - group_0,
                                               w_weight))
  #t_test_result_1$p.value
  
  t_test_result_22 <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                               non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                               group_2 - group_0,
                                               w_weight))
  }
  
  if (num_replicas == 5000){
    
    t_test_result_21 <- pnorm(Z_function(       mean(non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                                mean(group_1 - group_0),
                                                 non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                 group_1 - group_0,
                                                w_weight))
    #t_test_result_1$p.value
    
    t_test_result_22 <- pnorm(Z_function(mean(non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                         mean(group_2 - group_0),
                                                 non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                 group_2 - group_0,
                                         w_weight))
  }
  
  
  #continous_map1 <- c (t_test_result_21$p.value, t_test_result_22$p.value)
  
  continous_map1 <- c (t_test_result_21, t_test_result_22)
  
  ##################add bayesian log
  sd_log <- ifelse(mapping_function(1)<0,1,mapping_function(1))
  # prior_placebo_log <- mixnorm(mapping_function(c(1,0,1)), sigma = sd_log)   # Normal prior for Treatment A
  # prior_A_log <- mixnorm(mapping_function(c(trtA_effect, 0, trtA_effect)), sigma = sd_log)   # Normal prior for Treatment A
  # prior_B_log <- mixnorm(mapping_function(c(trtA_effect+diff_stage2, 0, trtA_effect+diff_stage2)), sigma = sd_log)   # Normal prior for Treatment B

  prior_placebo_log <- mixnorm(c(1,0,1), sigma = 1)   # Normal prior for Treatment A
  prior_A_log <- mixnorm(c(1,0,1), sigma = 1)   # Normal prior for Treatment A
  prior_B_log <- mixnorm(c(1,0,1), sigma = 1)   # Normal prior for Treatment B
  

  # Define the data from two groups (assuming we have two vectors of continuous responses)
  #group_0,group_1, group_2

  # Perform the Bayesian analysis for group 1 and group 2 using the defined priors
  post_placebo_log <- postmix(prior_placebo_log, m = mean( mapping_function(group_0)),
                              se = sd( mapping_function(group_0))/sqrt(length( group_0)))
  post_trtA_log <- postmix(prior_A_log, m = mean( mapping_function(group_1)),
                           se = sd( mapping_function(group_1))/sqrt(length( group_1)))
  post_trtB_log <- postmix(prior_B_log, m = mean( mapping_function(group_2)),
                           se = sd( mapping_function(group_2))/sqrt(length( group_2)))

  # prob_diff_A_log <-  pmixdiff(post_placebo_log, post_trtA_log, 0)
  # prob_diff_B_log <-  pmixdiff(post_placebo_log, post_trtB_log, 0)
  if (num_replicas == 1000){
  prob_diff_A_log <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                              non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                           post_trtA_log - post_placebo_log,
                           w_weight))
  prob_diff_B_log <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                              non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                              post_trtB_log - post_placebo_log,
                                              w_weight))
  }
  
  if (num_replicas == 5000){
    prob_diff_A_log <- pnorm(Z_function(mean( non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                        mean(post_trtA_log - post_placebo_log),
                                                non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                post_trtA_log - post_placebo_log,
                                        w_weight))
    prob_diff_B_log <- pnorm(Z_function(mean( non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                        mean(post_trtB_log - post_placebo_log),
                                                non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                post_trtB_log - post_placebo_log,
                                        w_weight))
  }
  continuous_result_bayesian_log <- c(prob_diff_A_log, prob_diff_B_log)
  ####################################################
  
  
  mapped_cont_model_2 <- lm(mapped_continuous_response_stage22 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2,
                          data = non_responders)
  #summary(mapped_cont_model)
  
  group_0_2 <- mapped_cont_model_2$fitted.values[index_0]
  group_1_2 <- mapped_cont_model_2$fitted.values[index_1]
  group_2_2 <- mapped_cont_model_2$fitted.values[index_2]
  # Perform the two-sample t-test
  #t_test_result_221 <- t.test(group_0_2, group_1_2)
  #t_test_result_1$p.value
  if (num_replicas == 1000){
  t_test_result_221 <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                                non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                           group_1_2 - group_0_2,
                           w_weight))
  
  #t_test_result_222 <- t.test(group_0_2, group_2_2)
  t_test_result_222 <- pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                                non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                group_2_2 - group_0_2,
                                                w_weight))
  }
  
  if (num_replicas == 5000){
    t_test_result_221 <- pnorm( Z_function(mean(non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                           mean( group_1_2 - group_0_2),
                                                  non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                  group_1_2 - group_0_2,
                                           w_weight))
    
    #t_test_result_222 <- t.test(group_0_2, group_2_2)
    t_test_result_222 <- pnorm( Z_function(mean(non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                           mean( group_2_2 - group_0_2),
                                                  non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                  group_2_2 - group_0_2,
                                           w_weight))
  }
  continous_map2 <- c (t_test_result_221, t_test_result_222)
  
  ##############add bayesian negative expo
  # sd_exp <- ifelse(mapping_function_2(1)<0,1,mapping_function_2(1))
  # prior_placebo_exp <- mixnorm(mapping_function_2(c(1,0,1)), sigma = sd_exp)   # Normal prior for Treatment A
  # prior_A_exp <- mixnorm(mapping_function_2(c(trtA_effect, 0, trtA_effect)), sigma =sd_exp)   # Normal prior for Treatment A
  # prior_B_exp <- mixnorm(mapping_function_2(c(trtA_effect+diff_stage2, 0, trtA_effect+diff_stage2)), sigma = sd_exp)   # Normal prior for Treatment B

  prior_placebo_exp <- mixnorm(c(1,0,1), sigma = 1)   # Normal prior for Treatment A
  prior_A_exp <- mixnorm(c(1,0,1), sigma =1)   # Normal prior for Treatment A
  prior_B_exp <- mixnorm(c(1,0,1), sigma = 1)   # Normal prior for Treatment B
  
  # Define the data from two groups (assuming we have two vectors of continuous responses)
  #group_0,group_1, group_2

  # Perform the Bayesian analysis for group 1 and group 2 using the defined priors
  post_placebo_exp <- postmix(prior_placebo_exp, m = mean( mapping_function_2(group_0)),
                              se = sd( mapping_function_2(group_0))/sqrt(length( group_0)))
  post_trtA_exp <- postmix(prior_A_exp, m = mean( mapping_function_2(group_1)),
                           se = sd( mapping_function_2(group_1))/sqrt(length( group_1)))
  post_trtB_exp <- postmix(prior_B_exp, m = mean( mapping_function_2(group_2)),
                           se = sd( mapping_function_2(group_2))/sqrt(length( group_2)))

  # prob_diff_A_exp <-  pmixdiff(post_placebo_exp, post_trtA_exp, 0)
  # prob_diff_B_exp <-  pmixdiff(post_placebo_exp, post_trtB_exp, 0)
  if (num_replicas == 1000){
  prob_diff_A_exp <-  pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                               non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                               post_trtA_exp - post_placebo_exp,
                                               w_weight))
  prob_diff_B_exp <-  pnorm(-1.96 - Z_function(diff_stage1, diff_stage2,
                                               non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                               post_trtB_exp - post_placebo_exp,
                                               w_weight))
  }
  
  if (num_replicas == 5000){
    prob_diff_A_exp <-  pnorm( Z_function(mean(non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                          mean(post_trtA_exp - post_placebo_exp),
                                                 non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                 post_trtA_exp - post_placebo_exp,
                                          w_weight))
    prob_diff_B_exp <-  pnorm( Z_function(mean(non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0]), 
                                          mean( post_trtB_exp - post_placebo_exp),
                                                 non_responders$continuous_response_stage1[index_1]-non_responders$continuous_response_stage1[index_0] ,
                                                 post_trtB_exp - post_placebo_exp,
                                          w_weight))
  }
  continuous_result_bayesian_exp <- c(prob_diff_A_exp, prob_diff_B_exp)
  #######################################################################
  
  return(list("binary_result" = binary_result, "continuous_result" = continuous_result,
              "continous_map1" = continous_map1 , "continous_map2" = continous_map2,
              "continuous_result_bayesian" = continuous_result_bayesian,
              "continuous_result_bayesian_log" = continuous_result_bayesian_log,
              "continuous_result_bayesian_exp" = continuous_result_bayesian_exp, "bias_comb" = bias_comb))
  
  # return(list("binary_result" = binary_result, "continuous_result" = continuous_result, 
  #             "continous_map1" = continous_map1 , "continous_map2" = continous_map2))
  # 
  
  # return(list("binary_result" = binary_result, "continuous_result" = continuous_result,
  #             "continous_map1" = continous_map1 , "continous_map2" = continous_map2,
  #             "continuous_result_bayesian" = continuous_result_bayesian,
  #             "continuous_result_bayesian_log" = continuous_result_bayesian_log))
}

