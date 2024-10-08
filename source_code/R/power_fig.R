
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
power_data <- function(final_table, power_col, power01_col){
  binary_result <- final_table[,1:2]
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

binary_power_data <- power_data(final_table, 1, 2)
continuous_power_data <- power_data(final_table, 3, 4)
continuous_power_logmap <- power_data(final_table, 5, 6)
continuous_power_expmap <- power_data(final_table, 7, 8)


####################################################
library(ggplot2)
library(data.table)
power_fig_fun <- function (binary_power_data){
  power_by_time_new_long <- melt(setDT(binary_power_data), id.vars = c("num_subjects", "diff_stage2", "trt"),
                                 variable.name = "power_both")
  ########################################################
  ########################################################
  power_by_time_new_long$num_timepoints_new <- c('"Treat A"', '"Treat B"')[as.factor(power_by_time_new_long$trt)]
  power_by_time_new_long$power_both_new <- c('alpha*" = 0.05"', 'alpha*" = 0.10"')[power_by_time_new_long$power_both]
  
  power_by_time_plot_new=ggplot(power_by_time_new_long,
                                aes(x=diff_stage2,y=unlist(value),color=as.factor(num_subjects),shape=as.factor(num_subjects)))+
    geom_line() +
    facet_grid(as.factor(num_timepoints_new)~power_both_new,
               labeller = label_parsed)+
    ylab("Power")+
    xlab(expression(~delta))+
    guides(color = guide_legend(title = "Subjects")) +
    theme(text = element_text(size = 20))  +
    theme(
      legend.position = c(.02, .98),
      legend.justification = c("left", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )#+
  #theme(strip.text.x = element_blank())
  print(power_by_time_plot_new)
}

power_fig_fun  (binary_power_data)
power_fig_fun  (continuous_power_data)
power_fig_fun  (continuous_power_logmap)
power_fig_fun  (continuous_power_expmap)
