
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
power_data <- function(final_table, power_col, power01_col, noise_sd){
  final_table <- final_table [final_table$noise_sd==noise_sd,]
  #2 :  3: "diff_stage1" , 5: "noise_sd" 
  final_table <- final_table [,-c(3,5)]
  binary_result <- final_table[,1:3]
  binary_result <- rbind(binary_result,binary_result)
  binary_result$power <- c(0)
  binary_result$power01 <- c(0)
  for ( i in 1:(dim(binary_result)[1]/2)){
    binary_result$power[i] <- final_table$power[[i]][power_col]
    binary_result$power[dim(binary_result)[1]/2+i] <- final_table$power[[i]][power01_col]
    binary_result$power01[i] <- final_table$power01[[i]][power_col]
    binary_result$power01[dim(binary_result)[1]/2+i] <- final_table$power01[[i]][power01_col]
    binary_result$trt <- c(rep("A",dim(binary_result)[1]/2),rep("B",dim(binary_result)[1]/2))
  }
  return(binary_result)
}

binary_power_data <- power_data(final_table, 1, 2 ,1)
continuous_power_data <- power_data(final_table, 3, 4 ,1 )
continuous_power_logmap <- power_data(final_table, 5, 6 ,1)
continuous_power_expmap <- power_data(final_table, 7, 8 ,1)
##############################################################
#noise sd <- 2 
# binary_power_data2 <- power_data(final_table, 1, 2 ,2)
# continuous_power_data2 <- power_data(final_table, 3, 4 ,2 )
# continuous_power_logmap2 <- power_data(final_table, 5, 6 ,2)
# continuous_power_expmap2 <- power_data(final_table, 7, 8 ,2)
###############################################################
#continuous_power_logmap
# num_subjects diff_stage2  power power01 trt
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
continuous_power_data_map <- power_data(final_table, 11, 12,1)
continuous_power_logmap_map <- power_data(final_table, 13, 14,1)
continuous_power_expmap_map <- power_data(final_table, 15, 16,1)

#################################################################
#noise sd <- 2
# binary_power_data_map2 <- power_data(final_table, 9, 10 ,2)
# continuous_power_data_map2 <- power_data(final_table, 11, 12,2)
# continuous_power_logmap_map2 <- power_data(final_table, 13, 14,2)
# continuous_power_expmap_map2 <- power_data(final_table, 15, 16,2)
###################################################################

###bayesian results
#non map
continuous_power_data_bay <- power_data(final_table, 17, 18 ,1 )
continuous_power_logmap_bay <- power_data(final_table, 19, 20 ,1)
continuous_power_expmap_bay <- power_data(final_table, 21, 22 ,1)

#map
continuous_power_data_map_bay <- power_data(final_table, 23, 24,1)
continuous_power_logmap_map_bay <- power_data(final_table, 25, 26,1)
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
####################################################
library(ggplot2)
library(data.table)
power_fig_fun <- function (binary_power_data, diff_stage2, trtA_effect){
  power_by_time_new_long <- melt(setDT(binary_power_data), id.vars = c("num_subjects", "trtA_effect", "diff_stage2", "trt"),
                                 variable.name = "power_both")
  ########################################################
  ########################################################
  #diff_stage2 <- 1.5
  # trtA_effect <- 3.0
  power_by_time_new_long$trt_new <- c('"Treat A"', '"Treat B"')[as.factor(power_by_time_new_long$trt)]
  power_by_time_new_long$power_both_new <- c('alpha*" = 0.05"', 'alpha*" = 0.10"')[power_by_time_new_long$power_both]
  
  power_by_time_plot_new=ggplot(power_by_time_new_long[power_by_time_new_long$trt == "A" & power_by_time_new_long$diff_stage2==1.5,],
                                aes(x=trtA_effect,y=unlist(value),color=as.factor(num_subjects),shape=as.factor(num_subjects)))+
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
  
  power_by_time_plot_new_B=ggplot(power_by_time_new_long[power_by_time_new_long$trt == "B" & power_by_time_new_long$trtA_effect== 3.0,],
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
