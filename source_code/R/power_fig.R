
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
# Purpose: Power Curves for SPCD Hypothesis Testing
#         
# Author:  Xiaoxia Champon
# Date: 10/08/2024
#
##############################################################
###
#load("./output_spcd_test/final_table.RData")
load("/Users/xzhao17/Documents/GitHub/SPCD/output_spcd_test_typeI_bayesian_power/spcd_power__1000_10_1.RData")

#type I
# final_table[,1:6]
#                           num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2 noise_sd
# experiment_output            200                  2                  1           0           0        1
# experiment_output.1          400                  2                  1           0           0        1
# experiment_output.2          600                  2                  1           0           0        1
# experiment_output.3          200                  2                  1           0           0        4
# experiment_output.4          400                  2                  1           0           0        4
# experiment_output.5          600                  2                  1           0           0        4
power_data <- function(final_table, power_col, power01_col, noise_sd, w_weight, option_01){
  final_table_sub <- final_table [(final_table$noise_sd == noise_sd & final_table$w_weight == w_weight) & final_table$option_01 == option_01,]
  #2 : trtA_effect_stage1 3:trtA_effect_stage2, 4 "diff_stage1" , 5:diff_stage2,  6: "noise_sd" , 7: w_weight
  #final_table <- final_table [,-c(3,5)]
  final_table_sub <- final_table_sub [,-c(6, 7)]
  #binary_result <- final_table[,1:3]
  binary_result <- final_table_sub[,1:7]
  binary_result <- rbind(binary_result,binary_result)
  binary_result$power <- c(0)
  binary_result$power01 <- c(0)
  for ( i in 1:(dim(binary_result)[1]/2)){
    binary_result$power[i] <- final_table_sub$power[[i]][power_col]
    binary_result$power[dim(binary_result)[1]/2+i] <- final_table_sub$power[[i]][power01_col]
    binary_result$power01[i] <- final_table_sub$power01[[i]][power_col]
    binary_result$power01[dim(binary_result)[1]/2+i] <- final_table_sub$power01[[i]][power01_col]
    binary_result$trt <- c(rep("A",dim(binary_result)[1]/2),rep("B",dim(binary_result)[1]/2))
  }
  return(binary_result)
}

binary_power_data <- power_data(final_table, 1, 2 ,1, 0.4)
continuous_power_data <- power_data(final_table, 3, 4 ,1 ,0.4)
continuous_power_logmap <- power_data(final_table, 5, 6 ,1, 0.4)
continuous_power_expmap <- power_data(final_table, 7, 8 ,1, 0.4)


binary_power_data <- power_data(final_table, 1, 2 ,1, 0.5)

########
continuous_power_data <- power_data(final_table, 3, 4 ,1 ,0.5, 1)
continuous_power_data2 <- power_data(final_table, 3, 4 ,1 ,0.5, 2)
continuous_power_data 
continuous_power_data2
#########
continuous_power_logmap <- power_data(final_table, 5, 6 ,1, 0.5)
continuous_power_expmap <- power_data(final_table, 7, 8 ,1, 0.5)


binary_power_data <- power_data(final_table, 1, 2 ,1, 0.6)
continuous_power_data <- power_data(final_table, 3, 4 ,1 ,0.6)
continuous_power_logmap <- power_data(final_table, 5, 6 ,1, 0.6)
continuous_power_expmap <- power_data(final_table, 7, 8 ,1, 0.6)


binary_power_data <- power_data(final_table, 1, 2 ,1, 0.6)
continuous_power_data <- power_data(final_table, 3, 4 ,1 ,0.8)
continuous_power_data06 <- power_data(final_table, 3, 4 ,1 ,0.6)
continuous_power_logmap <- power_data(final_table, 5, 6 ,1, 0.8)
continuous_power_logmap06 <- power_data(final_table, 5, 6 ,1, 0.6)
continuous_power_expmap <- power_data(final_table, 7, 8 ,1, 0.6)

# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.01398601 0.05594406   A
# experiment_output.7           400                  2                  1           0           0 0.02517483 0.05314685   A
# experiment_output.8           600                  2                  1           0           0 0.03636364 0.07272727   A
# experiment_output.61          200                  2                  1           0           0 0.01818182 0.04055944   B
# experiment_output.71          400                  2                  1           0           0 0.02517483 0.05454545   B
# experiment_output.81          600                  2                  1           0           0 0.03216783 0.05594406   B
##############################################################
#noise sd <- 2 
# binary_power_data2 <- power_data(final_table, 1, 2 ,2)
# continuous_power_data2 <- power_data(final_table, 3, 4 ,2 )
# continuous_power_logmap2 <- power_data(final_table, 5, 6 ,2)
# continuous_power_expmap2 <- power_data(final_table, 7, 8 ,2)
###############################################################
#continuous_power_logmap
#                       num_subjects diff_stage2  power power01 trt
# experiment_output             600           0 0.0504  0.1074   A
# experiment_output.1           900           0 0.0632  0.1154   A
# experiment_output1            600           0 0.0532  0.1104   B
# experiment_output.11          900           0 0.0586  0.1108   B

# continuous_power_expmap 
# num_subjects diff_stage2  power power01 trt
# experiment_output             600           0 0.5864  0.6370   A
# experiment_output.1           900           0 0.6172  0.6652   A
# experiment_output1            600           0 0.5798  0.6300   B
# experiment_output.11          900           0 0.6254  0.6730   B

### add different treatment effect
# num_subjects trtA_effect diff_stage2 power power01 trt
# experiment_output            200         3.0         1.5 0.110   0.184   A
# experiment_output.1          400         3.0         1.5 0.166   0.265   A
# experiment_output.2          600         3.0         1.5 0.217   0.303   A
# experiment_output.3          200         3.5         1.5 0.161   0.242   A
# experiment_output.4          400         3.5         1.5 0.205   0.310   A
# experiment_output.5          600         3.5         1.5 0.283   0.404   A

binary_power_data_map <- power_data(final_table, 9, 10 ,1)
####################
continuous_power_data_map <- power_data(final_table, 11, 12,1, 0.5, 1)
continuous_power_data_map2 <- power_data(final_table, 11, 12,1, 0.5, 2)
continuous_power_data_map
continuous_power_data_map2
########################
continuous_power_data_map <- power_data(final_table, 11, 12,1, 0.8)
continuous_power_data_map06 <- power_data(final_table, 11, 12,1, 0.6)
continuous_power_logmap_map <- power_data(final_table, 13, 14,1 , 0.8)
continuous_power_logmap_map06 <- power_data(final_table, 13, 14,1, 0.6)
continuous_power_expmap_map <- power_data(final_table, 15, 16,1)


##############################results summary
get_typeI_weight <- function(weight_w){
  continuous_power_data <- power_data(final_table, 3, 4 ,1 , weight_w)
  continuous_power_logmap <- power_data(final_table, 5, 6 ,1, weight_w)
  continuous_power_expmap <- power_data(final_table, 7, 8 ,1, weight_w)
  print("\n continuous \t")
  print(continuous_power_data)
  print("\n continuous log \t")
  print(continuous_power_logmap)
  print("\n continuous exp \t")
  print(continuous_power_expmap)
  
  continuous_power_data_map <- power_data(final_table, 11, 12,1, weight_w)
  print("\ n continuous map \t")
  print(continuous_power_data_map)
  continuous_power_logmap_map <- power_data(final_table, 13, 14,1, weight_w)
  print("\n continuous log map \t")
  print(continuous_power_logmap_map)
  continuous_power_expmap_map <- power_data(final_table, 15, 16,1, weight_w)
  print("\n continuous exp map \t")
  print(continuous_power_expmap_map)
  
  continuous_power_data_map_bay <- power_data(final_table, 23, 24,1, weight_w)
  print("\ n continuous map bay \t")
  print(continuous_power_data_map_bay)
  continuous_power_logmap_map_bay <- power_data(final_table, 25, 26,1, weight_w)
  print("\ n continuous map log bay \t") 
  print(continuous_power_logmap_map_bay)
  continuous_power_expmap_map_bay <- power_data(final_table, 27, 28,1, weight_w)
  print("\ n continuous map exp bay \t") 
  print(continuous_power_expmap_map_bay)
  
}

get_typeI_weight(0.6)
################################################################################
# [1] "\n continuous \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output             200                  2                  1           0           0 0.01538462 0.03776224   A
# experiment_output.1           400                  2                  1           0           0 0.03076923 0.06013986   A
# experiment_output.2           600                  2                  1           0           0 0.02797203 0.06713287   A
# experiment_output1            200                  2                  1           0           0 0.00979021 0.03216783   B
# experiment_output.11          400                  2                  1           0           0 0.02657343 0.05454545   B
# experiment_output.21          600                  2                  1           0           0 0.02517483 0.05454545   B
# [1] "\n continuous log \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2       power    power01 trt
# experiment_output             200                  2                  1           0           0 0.008391608 0.03636364   A
# experiment_output.1           400                  2                  1           0           0 0.016783217 0.04055944   A
# experiment_output.2           600                  2                  1           0           0 0.027972028 0.06013986   A
# experiment_output1            200                  2                  1           0           0 0.012587413 0.03496503   B
# experiment_output.11          400                  2                  1           0           0 0.022377622 0.05454545   B
# experiment_output.21          600                  2                  1           0           0 0.029370629 0.05874126   B
# [1] "\n continuous exp \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output             200                  2                  1           0           0 0.01958042 0.04615385   A
# experiment_output.1           400                  2                  1           0           0 0.03076923 0.05594406   A
# experiment_output.2           600                  2                  1           0           0 0.02237762 0.05314685   A
# experiment_output1            200                  2                  1           0           0 0.01398601 0.04475524   B
# experiment_output.11          400                  2                  1           0           0 0.01958042 0.05034965   B
# experiment_output.21          600                  2                  1           0           0 0.02937063 0.05734266   B
# [1] " n continuous map \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output             200                  2                  1           0           0 0.01398601 0.04475524   A
# experiment_output.1           400                  2                  1           0           0 0.02797203 0.07132867   A
# experiment_output.2           600                  2                  1           0           0 0.03076923 0.07412587   A
# experiment_output1            200                  2                  1           0           0 0.01818182 0.03916084   B
# experiment_output.11          400                  2                  1           0           0 0.03076923 0.05594406   B
# experiment_output.21          600                  2                  1           0           0 0.02657343 0.05314685   B
# [1] "\n continuous log map \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2       power    power01 trt
# experiment_output             200                  2                  1           0           0 0.006993007 0.03356643   A
# experiment_output.1           400                  2                  1           0           0 0.022377622 0.05034965   A
# experiment_output.2           600                  2                  1           0           0 0.030769231 0.06153846   A
# experiment_output1            200                  2                  1           0           0 0.011188811 0.03636364   B
# experiment_output.11          400                  2                  1           0           0 0.023776224 0.05034965   B
# experiment_output.21          600                  2                  1           0           0 0.029370629 0.06993007   B
# [1] "\n continuous exp map \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output             200                  2                  1           0           0 0.02517483 0.05174825   A
# experiment_output.1           400                  2                  1           0           0 0.02237762 0.05734266   A
# experiment_output.2           600                  2                  1           0           0 0.02937063 0.06293706   A
# experiment_output1            200                  2                  1           0           0 0.01258741 0.03636364   B
# experiment_output.11          400                  2                  1           0           0 0.04195804 0.06573427   B
# experiment_output.21          600                  2                  1           0           0 0.02797203 0.07132867   B
# [1] " n continuous map bay \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output             200                  2                  1           0           0 0.03076923 0.05594406   A
# experiment_output.1           400                  2                  1           0           0 0.01538462 0.04335664   A
# experiment_output.2           600                  2                  1           0           0 0.01958042 0.05174825   A
# experiment_output1            200                  2                  1           0           0 0.02377622 0.04895105   B
# experiment_output.11          400                  2                  1           0           0 0.02797203 0.06713287   B
# experiment_output.21          600                  2                  1           0           0 0.03496503 0.05314685   B
# [1] " n continuous map log bay \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output             200                  2                  1           0           0 0.01118881 0.03216783   A
# experiment_output.1           400                  2                  1           0           0 0.01958042 0.05454545   A
# experiment_output.2           600                  2                  1           0           0 0.02797203 0.05454545   A
# experiment_output1            200                  2                  1           0           0 0.01958042 0.03496503   B
# experiment_output.11          400                  2                  1           0           0 0.02937063 0.05594406   B
# experiment_output.21          600                  2                  1           0           0 0.03496503 0.06433566   B
# [1] " n continuous map exp bay \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2       power    power01 trt
# experiment_output             200                  2                  1           0           0 0.005594406 0.03356643   A
# experiment_output.1           400                  2                  1           0           0 0.029370629 0.07272727   A
# experiment_output.2           600                  2                  1           0           0 0.043356643 0.08251748   A
# experiment_output1            200                  2                  1           0           0 0.016783217 0.03636364   B
# experiment_output.11          400                  2                  1           0           0 0.029370629 0.05314685   B
# experiment_output.21          600                  2                  1           0           0 0.033566434 0.06153846   B
######################################################################################################################




get_typeI_weight(0.8)
#################################################
# [1] "\n continuous \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.01398601 0.05594406   A
# experiment_output.7           400                  2                  1           0           0 0.02517483 0.05314685   A
# experiment_output.8           600                  2                  1           0           0 0.03636364 0.07272727   A
# experiment_output.61          200                  2                  1           0           0 0.01818182 0.04055944   B
# experiment_output.71          400                  2                  1           0           0 0.02517483 0.05454545   B
# experiment_output.81          600                  2                  1           0           0 0.03216783 0.05594406   B
# [1] "\n continuous log \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.01678322 0.04055944   A
# experiment_output.7           400                  2                  1           0           0 0.02797203 0.05314685   A
# experiment_output.8           600                  2                  1           0           0 0.02937063 0.07412587   A
# experiment_output.61          200                  2                  1           0           0 0.01538462 0.04755245   B
# experiment_output.71          400                  2                  1           0           0 0.01398601 0.03216783   B
# experiment_output.81          600                  2                  1           0           0 0.02237762 0.05314685   B
# [1] "\n continuous exp \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.01118881 0.02797203   A
# experiment_output.7           400                  2                  1           0           0 0.01818182 0.05174825   A
# experiment_output.8           600                  2                  1           0           0 0.02377622 0.04475524   A
# experiment_output.61          200                  2                  1           0           0 0.01538462 0.04055944   B
# experiment_output.71          400                  2                  1           0           0 0.03076923 0.06013986   B
# experiment_output.81          600                  2                  1           0           0 0.02937063 0.06433566   B
# [1] " n continuous map \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.02657343 0.05034965   A
# experiment_output.7           400                  2                  1           0           0 0.03216783 0.06433566   A
# experiment_output.8           600                  2                  1           0           0 0.03076923 0.06713287   A
# experiment_output.61          200                  2                  1           0           0 0.01118881 0.04055944   B
# experiment_output.71          400                  2                  1           0           0 0.02517483 0.05034965   B
# experiment_output.81          600                  2                  1           0           0 0.03496503 0.06993007   B
# [1] "\n continuous log map \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.01118881 0.04055944   A
# experiment_output.7           400                  2                  1           0           0 0.03216783 0.05314685   A
# experiment_output.8           600                  2                  1           0           0 0.01818182 0.05314685   A
# experiment_output.61          200                  2                  1           0           0 0.01818182 0.04055944   B
# experiment_output.71          400                  2                  1           0           0 0.03216783 0.06573427   B
# experiment_output.81          600                  2                  1           0           0 0.02377622 0.04895105   B
# [1] "\n continuous exp map \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2       power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.008391608 0.04195804   A
# experiment_output.7           400                  2                  1           0           0 0.036363636 0.06433566   A
# experiment_output.8           600                  2                  1           0           0 0.037762238 0.06853147   A
# experiment_output.61          200                  2                  1           0           0 0.020979021 0.04475524   B
# experiment_output.71          400                  2                  1           0           0 0.029370629 0.06573427   B
# experiment_output.81          600                  2                  1           0           0 0.030769231 0.06573427   B
# [1] " n continuous map bay \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.03356643 0.06853147   A
# experiment_output.7           400                  2                  1           0           0 0.02237762 0.06573427   A
# experiment_output.8           600                  2                  1           0           0 0.03216783 0.06853147   A
# experiment_output.61          200                  2                  1           0           0 0.02377622 0.05034965   B
# experiment_output.71          400                  2                  1           0           0 0.02517483 0.04475524   B
# experiment_output.81          600                  2                  1           0           0 0.03076923 0.06433566   B
# [1] " n continuous map log bay \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.01678322 0.04475524   A
# experiment_output.7           400                  2                  1           0           0 0.02657343 0.06153846   A
# experiment_output.8           600                  2                  1           0           0 0.02657343 0.05174825   A
# experiment_output.61          200                  2                  1           0           0 0.01678322 0.04475524   B
# experiment_output.71          400                  2                  1           0           0 0.03356643 0.07552448   B
# experiment_output.81          600                  2                  1           0           0 0.02937063 0.06293706   B
# [1] " n continuous map exp bay \t"
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.01398601 0.04195804   A
# experiment_output.7           400                  2                  1           0           0 0.02657343 0.06013986   A
# experiment_output.8           600                  2                  1           0           0 0.02937063 0.05874126   A
# experiment_output.61          200                  2                  1           0           0 0.03636364 0.05874126   B
# experiment_output.71          400                  2                  1           0           0 0.02797203 0.05734266   B
# experiment_output.81          600                  2                  1           0           0 0.02517483 0.05874126   B



#####################################################
# 
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.02657343 0.05034965   A
# experiment_output.7           400                  2                  1           0           0 0.03216783 0.06433566   A
# experiment_output.8           600                  2                  1           0           0 0.03076923 0.06713287   A
# experiment_output.61          200                  2                  1           0           0 0.01118881 0.04055944   B
# experiment_output.71          400                  2                  1           0           0 0.02517483 0.05034965   B
# experiment_output.81          600                  2                  1           0           0 0.03496503 0.06993007   B
#################################################################
#noise sd <- 2
# binary_power_data_map2 <- power_data(final_table, 9, 10 ,2)
# continuous_power_data_map2 <- power_data(final_table, 11, 12,2)
# continuous_power_logmap_map2 <- power_data(final_table, 13, 14,2)
# continuous_power_expmap_map2 <- power_data(final_table, 15, 16,2)
###################################################################

###bayesian results
#non map
continuous_power_data_bay <- power_data(final_table, 17, 18 ,1, 0.8 )
continuous_power_data_bay06 <- power_data(final_table, 17, 18 ,1, 0.6 )
continuous_power_logmap_bay <- power_data(final_table, 19, 20 ,1, 0.8)
continuous_power_logmap_bay06 <- power_data(final_table, 19, 20 ,1, 0.6)
continuous_power_expmap_bay <- power_data(final_table, 21, 22 ,1)

#map
continuous_power_data_map_bay <- power_data(final_table, 23, 24,1, 0.8)
continuous_power_data_map_bay_06 <- power_data(final_table, 23, 24,1, 0.6)
continuous_power_logmap_map_bay <- power_data(final_table, 25, 26,1, 0.8)
continuous_power_logmap_map_bay06 <- power_data(final_table, 25, 26,1, 0.6)
continuous_power_expmap_map_bay <- power_data(final_table, 27, 28,1)


save(binary_power_data, continuous_power_data, continuous_power_logmap, continuous_power_expmap,
     binary_power_data2, continuous_power_data2, continuous_power_logmap2, continuous_power_expmap2,
     file = "TypeI_error.RData")

# continuous_power_logmap2 
# num_subjects diff_stage2  power power01 trt
# experiment_output             600           0 0.0586  0.1114   A
# experiment_output.1           900           0 0.0570  0.1126   A
# experiment_output1            600           0 0.0552  0.1098   B
# experiment_output.11          900           0 0.0578  0.1150   B


# continuous_power_expmap2
# num_subjects diff_stage2  power power01 trt
# experiment_output             600           0 0.6406  0.6592   A
# experiment_output.1           900           0 0.6612  0.6788   A
# experiment_output1            600           0 0.6258  0.6456   B
# experiment_output.11          900           0 0.6386  0.6538   B



# continuous_power_data 
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.01398601 0.05594406   A
# experiment_output.7           400                  2                  1           0           0 0.02517483 0.05314685   A
# experiment_output.8           600                  2                  1           0           0 0.03636364 0.07272727   A
# experiment_output.61          200                  2                  1           0           0 0.01818182 0.04055944   B
# experiment_output.71          400                  2                  1           0           0 0.02517483 0.05454545   B
# experiment_output.81          600                  2                  1           0           0 0.03216783 0.05594406   B
# > continuous_power_data_map <- power_data(final_table, 11, 12,1, 0.8)
# > continuous_power_data_map
# num_subjects trtA_effect_stage1 trtA_effect_stage2 diff_stage1 diff_stage2      power    power01 trt
# experiment_output.6           200                  2                  1           0           0 0.02657343 0.05034965   A
# experiment_output.7           400                  2                  1           0           0 0.03216783 0.06433566   A
# experiment_output.8           600                  2                  1           0           0 0.03076923 0.06713287   A
# experiment_output.61          200                  2                  1           0           0 0.01118881 0.04055944   B
# experiment_output.71          400                  2                  1           0           0 0.02517483 0.05034965   B
# experiment_output.81          600                  2                  1           0           0 0.03496503 0.06993007   B




####################################################
library(ggplot2)
library(data.table)
library(gridExtra)
power_fig_fun <- function (binary_power_data, diff_stage2, trtA_effect_stage1, trtA_effect_stage2){
  #delete colun diff_stage1
  power_by_time_new_long <- melt(setDT(binary_power_data[,-c(4)]), id.vars = c("num_subjects", "trtA_effect_stage1","trtA_effect_stage2",  "diff_stage2", "trt"),
                                 variable.name = "power_both")
  ########################################################
  ########################################################
  #diff_stage2 <- 1.5
  # trtA_effect <- 3.0
  power_by_time_new_long$trt_new <- c('"Treat A"', '"Treat B"')[as.factor(power_by_time_new_long$trt)]
  power_by_time_new_long$power_both_new <- c('alpha*" = 0.05"', 'alpha*" = 0.10"')[power_by_time_new_long$power_both]
  
  power_by_time_plot_new=ggplot(power_by_time_new_long[power_by_time_new_long$trt == "A" & power_by_time_new_long$diff_stage2==1.5 & power_by_time_new_long$trtA_effect_stage1==5,],
                                aes(x=trtA_effect_stage2,y=unlist(value),color=as.factor(num_subjects),shape=as.factor(num_subjects)))+
    geom_line() +
    facet_grid(as.factor(trt_new)~power_both_new,
               labeller = label_parsed)+
    ylab("Power")+
    xlab(expression(~delta))+
    guides(color = guide_legend(title = "Subjects")) +
    theme(text = element_text(size = 20))  +
    theme(
      legend.position = c(0.02, .98),
      legend.justification = c("left", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )#+
  
  power_by_time_plot_new_B=ggplot(power_by_time_new_long[power_by_time_new_long$trt == "B" & power_by_time_new_long$trtA_effect_stage1== 3.0 &power_by_time_new_long$trtA_effect_stage2== 3.0,],
                                aes(x=diff_stage2,y=unlist(value),color=as.factor(num_subjects),shape=as.factor(num_subjects)))+
    geom_line() +
    facet_grid(as.factor(trt_new)~power_both_new,
               labeller = label_parsed)+
    ylab("Power")+
    xlab(expression(~delta))+
    guides(color = guide_legend(title = "Subjects")) +
    theme(text = element_text(size = 20))  +
    theme(legend.position = "none")
  #theme(strip.text.x = element_blank())
  #print(power_by_time_plot_new)
  print(grid.arrange(power_by_time_plot_new, power_by_time_plot_new_B, nrow = 2))
}


# continuous_power_data <- power_data(final_table, 3, 4 ,1 ,0.8)
# continuous_power_data06 <- power_data(final_table, 3, 4 ,1 ,0.6)
# continuous_power_logmap <- power_data(final_table, 5, 6 ,1, 0.8)
# continuous_power_logmap06 <- power_data(final_table, 5, 6 ,1, 0.6)
#continuous_power_data_map_bay <- power_data(final_table, 23, 24,1, 0.8)
#continuous_power_data_map_bay_06 <- power_data(final_table, 23, 24,1, 0.6)

#######################################################
#weight 0.6
power_fig_fun  (continuous_power_data06 , 1.5, 3)
power_fig_fun  (continuous_power_data_map06 , 1.5, 3)



power_fig_fun  (continuous_power_logmap06, 1.5, 3)
power_fig_fun  (continuous_power_logmap_map06, 1.5, 3)

power_fig_fun(continuous_power_data_bay06, 1.5, 3)
power_fig_fun  (continuous_power_data_map_bay_06 , 1.5, 3)


power_fig_fun(continuous_power_logmap_bay06, 1.5, 3)
power_fig_fun(continuous_power_logmap_map_bay06, 1.5, 3)

#weight 0.8
power_fig_fun  (continuous_power_data , 1.5, 3)
power_fig_fun  (continuous_power_data_map , 1.5, 3)



power_fig_fun  (continuous_power_logmap, 1.5, 3)
power_fig_fun  (continuous_power_logmap_map, 1.5, 3)

power_fig_fun(continuous_power_data_bay, 1.5, 3)
power_fig_fun  (continuous_power_data_map_bay , 1.5, 3)


power_fig_fun(continuous_power_logmap_bay, 1.5, 3)
power_fig_fun(continuous_power_logmap_map_bay, 1.5, 3)

####################################################

power_fig_fun  (continuous_power_data , 1.5, 3)
power_fig_fun  (continuous_power_logmap , 1.5, 3)
power_fig_fun  (continuous_power_data_map_bay , 1.5, 3)




par(mfrow=c(2,3))
#power_fig_fun  (binary_power_data)
power_fig_fun  (continuous_power_data , 1.5, 3)
power_fig_fun  (continuous_power_logmap, 1.5, 3)
power_fig_fun  (continuous_power_expmap, 1.5, 3)

#sd <- 2
#power_fig_fun  (binary_power_data2)
# power_fig_fun  (continuous_power_data2)
# power_fig_fun  (continuous_power_logmap2)
# power_fig_fun  (continuous_power_expmap2)

par(mfrow=c(2,3))
#power_fig_fun  (binary_power_data_map)
power_fig_fun  (continuous_power_data_map, 1.5, 3)
power_fig_fun  (continuous_power_logmap_map, 1.5, 3)
power_fig_fun  (continuous_power_expmap_map, 1.5, 3)

#sd <- 2
#power_fig_fun  (binary_power_data_map2)
# power_fig_fun  (continuous_power_data_map2)
# power_fig_fun  (continuous_power_logmap_map2)
# power_fig_fun  (continuous_power_expmap_map2)


###bay
power_fig_fun  (continuous_power_data_bay, 1.5, 3)
power_fig_fun  (continuous_power_logmap_bay, 1.5, 3)
power_fig_fun  (continuous_power_expmap_bay, 1.5, 3)


power_fig_fun  (continuous_power_data_map_bay, 1.5, 3)
power_fig_fun  (continuous_power_logmap_map_bay, 1.5, 3)
power_fig_fun  (continuous_power_expmap_map_bay, 1.5, 3)
