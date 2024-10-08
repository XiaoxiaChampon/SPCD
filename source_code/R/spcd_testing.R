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

hypothesis_testing <- function(non_responders){
  # Perform logistic regression on pooled data
  
  # Set "0" as the reference level
  non_responders$treatment_stage2 <- relevel(as.factor(non_responders$treatment_stage2), ref = "0")
  pooled_binary_model <- glm(binary_response_stage2 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2, 
                             family = binomial(link = "logit"), data = non_responders)
  
  predicted_prob <- predict(pooled_binary_model, type = "response")
  
  index_0 <- which(non_responders$treatment_stage2=="0")
  index_1 <- which(non_responders$treatment_stage2=="1")
  index_2 <- which(non_responders$treatment_stage2=="2")
  
  
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
  # Perform the two-proportion z-test
  #test_result_2$p.value 
  
  binary_result <- c(test_result_1$p.value , test_result_2$p.value)
  # ------------------- Continuous Response Comparison in Stage 2 -------------------
  # Use mapping function for continuous response analysis (e.g., log transformation)
 
  
  # Analyze mapped continuous response with linear regression
cont_model <- lm(continuous_response_stage2 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2,
                          data = non_responders)
  #summary(mapped_cont_model)
  
  group_0 <- cont_model$fitted.values[index_0]
  group_1 <- cont_model$fitted.values[index_1]
  group_2 <- cont_model$fitted.values[index_2]
  # Perform the two-sample t-test
  t_test_result_1 <- t.test(group_0, group_1)
  #t_test_result_1$p.value
  
  t_test_result_2 <- t.test(group_0, group_2)
  #t_test_result_2$p.value
  
  continuous_result <- c(t_test_result_1$p.value, t_test_result_2$p.value)
  # Analyze mapped continuous response with linear regression
  mapped_cont_model <- lm(mapped_continuous_response_stage2 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2,
                          data = non_responders)
  #summary(mapped_cont_model)
  
  group_0 <- mapped_cont_model$fitted.values[index_0]
  group_1 <- mapped_cont_model$fitted.values[index_1]
  group_2 <- mapped_cont_model$fitted.values[index_2]
  # Perform the two-sample t-test
  t_test_result_21 <- t.test(group_0, group_1)
  #t_test_result_1$p.value
  
  t_test_result_22 <- t.test(group_0, group_2)
  continous_map1 <- c (t_test_result_21$p.value, t_test_result_22$p.value)
  
  
  mapped_cont_model_2 <- lm(mapped_continuous_response_stage22 ~ binary_cov1 + binary_cov2 + continuous_cov1 + continuous_cov2 + treatment_stage2,
                          data = non_responders)
  #summary(mapped_cont_model)
  
  group_0_2 <- mapped_cont_model_2$fitted.values[index_0]
  group_1_2 <- mapped_cont_model_2$fitted.values[index_1]
  group_2_2 <- mapped_cont_model_2$fitted.values[index_2]
  # Perform the two-sample t-test
  t_test_result_221 <- t.test(group_0_2, group_1_2)
  #t_test_result_1$p.value
  
  t_test_result_222 <- t.test(group_0_2, group_2_2)
  continous_map2 <- c (t_test_result_221$p.value, t_test_result_222$p.value)
  
  return(list("binary_result" = binary_result, "continuous_result" = continuous_result, "continous_map1" = continous_map1 , "continous_map2" = continous_map2))
  
}

