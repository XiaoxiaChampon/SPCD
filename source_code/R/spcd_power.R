
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
# Purpose: Simulations for Hypothesis testing SPCD
#         
# Author:  Xiaoxia Champon
# Date: 09/27/2024
#
##############################################################

library(parallel)
library(stats)
library(RBesT)

#library(pracma)


###########
library(optparse)

# Define options
option_list <- list(
  make_option(c("-j", "--jobid"), type="integer", default=123,
              help="Job Index", metavar="JOBID"),
  make_option(c("-n", "--numcpus"), type="integer", default=32,
              help="Num CPUs", metavar="NUMCPUS"),
  make_option(c("-r", "--replicas"), type="integer", default=100,
              help="Num Replicas", metavar="NUMREPLICAS")
)

#####need for hazel
# Create parser and parse options
parser <- OptionParser(option_list=option_list)
options <- parse_args(parser)

# options_jobid <- options$jobid
# options_numcpus <- options$numcpus
# options_replicas <- options$replicas
# #options_subjects <- options$subjects

#####################

options_jobid <- 1
options_numcpus <- 10
options_replicas <- 5000
#options_replicas <- 1000
# Use the options
cat("Job Idx:", options_jobid, "\n")
cat("Num CPUs:", options_numcpus, "\n")
cat("Num Replicas:", options_replicas, "\n")
#cat("Num Subjects:", options_subjects, "\n")

###########
# ---- For: parallelization ----
# For: foreach loop
library(foreach)

run_parallel <- TRUE
time_elapsed <- list()
if(run_parallel)
{
  print("RUNNING PARALLEL")
  
  # For: makeCluster
  library(doParallel)
  
  # For: %dorng% or registerDoRNG for reproducable parallel random number generation
  library(doRNG)
  
  if(exists("initialized_parallel") && initialized_parallel == TRUE)
  {
    parallel::stopCluster(cl = my.cluster)
  }
  # n.cores <- parallel::detectCores()
  n.cores <- options_numcpus
  my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
  doParallel::registerDoParallel(cl = my.cluster)
  cat("Parellel Registered: ", foreach::getDoParRegistered(), " (num cores=", n.cores, ")\n")
  initialized_parallel <- TRUE
  
  # registerDoRNG(123) # ///<<<< THIS CREATES THE ERROR FOR FADPClust !!!
}

ensure_dir_exist <- function(directory_path){
  # Check if the directory exists
  if(!dir.exists(directory_path)) {
    # Directory doesn't exist, so create it
    dir.create(directory_path, recursive = TRUE)
    cat("Directory created:", directory_path, "\n")
  } else {
    cat("Directory already exists:", directory_path, "\n")
  }
}

# scenario_folder = "spcd_test"
# ensure_dir_exist(scenario_folder)
# 
# final_table_folder = paste0("output_spcd_test")
# ensure_dir_exist(final_table_folder)

scenario_folder = "spcd_test_typeI_weight"
#scenario_folder = "spcd_test_power_iter_trt12_new"
ensure_dir_exist(scenario_folder)

final_table_folder = paste0("output_spcd_test_typeI_weight")
#final_table_folder = paste0("output_spcd_test_power_iter_trt12_new")
ensure_dir_exist(final_table_folder)


# spcd_testing_simulation <- function (num_replicas, 
#                                      num_indvs,
#                                      diff_stage2 ,
#                                      n_groups = 3,
#                                      diff_stage1 = 0.5){
  spcd_testing_simulation <- function (num_replicas, 
                                       num_indvs,
                                       trtA_effect_stage1,
                                       trtA_effect_stage2,
                                       diff_stage1 ,
                                       diff_stage2 ,
                                       noise_sd,
                                       w_weight,
                                       n_groups = 3){
  cat("SPCD Hypothesis Testing Simulation \nNum Replicas:\t", num_replicas)
  #num_replicas=2
  result_all <- foreach (number_simulation = 1:num_replicas, .combine = cbind, .init = NULL,
                         .packages=c("RBesT", "stats")) %dorng% {
                           
                           source("./source_code/R/data_generator.R")
                           source("./source_code/R/spcd_testing.R")
                           #                       
                           #######################################################################
                           #spcd_data_test <- spcd_data(n, n_groups, diff_stage1, diff_stage2)
                           # num_indvs <- 200
                           # n_groups <- 3
                           # diff_stage1 <- 0
                           # diff_stage2 <- 0
                           # trtA_effect_stage1 <- 2
                           # trtA_effect_stage2 <- 1
                           # num_replicas <- 5000
                           # w_weight <- 0.4
                           #noise_sd <- 1
                           example_data <- spcd_data(num_indvs, n_groups, trtA_effect_stage1, trtA_effect_stage2, diff_stage1, diff_stage2, noise_sd)
                           
                           #####add while loop to avoid error from data generation process
                           iter_count  <- 0
                           while(min(table(example_data$spcd_data_yes$treatment_stage2)) < 3 && iter_count < 1000){
                             example_data <- spcd_data(num_indvs, n_groups, trtA_effect_stage1, trtA_effect_stage2, diff_stage1, diff_stage2, noise_sd)
                             iter_count <<- iter_count + 1
                           }
                           ######
                           non_responders <- example_data$spcd_data
                           result <- hypothesis_testing (non_responders, trtA_effect_stage1, trtA_effect_stage2, diff_stage1, diff_stage2, num_replicas, w_weight)
                           
                           non_responders2 <- example_data$spcd_data_yes
                           result2 <- hypothesis_testing (non_responders2, trtA_effect_stage1, trtA_effect_stage2, diff_stage1, diff_stage2, num_replicas, w_weight)
                           
                           # return(list("binary_result"=result$binary_result,"continuous_result"=result$continuous_result,
                           #             "continous_map1"=result$continous_map1,"continous_map2"=result$continous_map2))
                           
                           return(list("binary_result"=result$binary_result,"continuous_result"=result$continuous_result,
                                       "continous_map1"=result$continous_map1,"continous_map2"=result$continous_map2,
                                       "binary_result2"=result2$binary_result,"continuous_result2"=result2$continuous_result,
                                       "continous_map12"=result2$continous_map1,"continous_map22"=result2$continous_map2,
                                       "continuous_result_bayesian"=result$continuous_result_bayesian,
                                       "continuous_result_bayesian_log"=result$continuous_result_bayesian_log,
                                       "continuous_result_bayesian_exp"=result$continuous_result_bayesian_exp,
                                       "continuous_result_bayesian2"=result2$continuous_result_bayesian,
                                       "continuous_result_bayesian_log2"=result2$continuous_result_bayesian_log,
                                       "continuous_result_bayesian_exp2"=result2$continuous_result_bayesian_exp
                                       ))

                           # return(list("binary_result"=result$binary_result,"continuous_result"=result$continuous_result,
                           #             "continous_map1"=result$continous_map1,"continous_map2"=result$continous_map2,
                           #             "binary_result2"=result2$binary_result,"continuous_result2"=result2$continuous_result,
                           #             "continous_map12"=result2$continous_map1,"continous_map22"=result2$continous_map2
                           #            
                           # ))
                           
                           # return(list("binary_result"=result$binary_result,"continuous_result"=result$continuous_result,
                           #             "continous_map1"=result$continous_map1,"continous_map2"=result$continous_map2,
                           #             "binary_result2"=result2$binary_result,"continuous_result2"=result2$continuous_result,
                           #             "continous_map12"=result2$continous_map1,"continous_map22"=result2$continous_map2,
                           #             "continuous_result_bayesian"=result$continuous_result_bayesian,
                           #             "continuous_result_bayesian_log"=result$continuous_result_bayesian_log,
                           #             "continuous_result_bayesian2"=result2$continuous_result_bayesian,
                           #             "continuous_result_bayesian_log2"=result2$continuous_result_bayesian_log
                           #             ))
                         } 
  #T_rep <- do.call(rbind, T_rep)
  return(result_all)
  
}

#set.seed(123456 + 10 * options_jobid)
  #set.seed(123456 + 10 * 2)
 #  result_all <- spcd_testing_simulation  (num_replicas = 6,num_indvs= 400, trtA_effect = 2,  diff_stage1 = 0,
 #   diff_stage2 = 0,1)
 
  # num_replicas, 
  # num_indvs,
  # trtA_effect_stage1,
  # trtA_effect_stage2,
  # diff_stage1 ,
  # diff_stage2 ,
  # noise_sd,
  # w_weight,
  # n_groups = 3
   # result_all <- spcd_testing_simulation  (num_replicas = 6,num_indvs= 200, trtA_effect_stage1 =2, trtA_effect_stage2 =1,  diff_stage1 = 0,
   #                                        diff_stage2 = 0, noise_sd = 1,
   #                                         w_weight = 0.4, n_groups = 3)
# simulation_pvalues <- matrix(unlist(result_all), nrow=8)
# power <- apply(simulation_pvalues, 1, function(x){sum(x<=0.05)/length(simulation_pvalues[1,])})
# power01 <- apply(simulation_pvalues, 1, function(x){sum(x<=0.1)/length(simulation_pvalues[1,])})


source("./source_code/R/time_track_function.R")
run_experiment_hypothesis <- function(exp_idx,
                                      num_indvs,
                                      trtA_effect_stage1,
                                      trtA_effect_stage2,
                                      diff_stage1,
                                      diff_stage2 ,
                                      noise_sd, 
                                      num_replicas = options_replicas,
                                      w_weight,
                                      alpha = 0.05,
                                      alpha2 = 0.1){
  
  exp_str <- paste("Track time for \nNum Subjects:\t", num_indvs,
                   "\n trtA_effect stage 1:\t",trtA_effect_stage1,
                   "\n trtA_effect stage 2:\t",trtA_effect_stage2,
                   "\n diff_stage1:\t",diff_stage1,
                   "\n diff_stage2:\t",diff_stage2,
                   "\n w_weight:\t",w_weight,
                   "\n noise_sd:\t",noise_sd)
  writeLines(exp_str)
  timeKeeperStart(exp_str)
  simulation_scenarios <- spcd_testing_simulation(num_replicas = num_replicas, 
                                                 num_indvs = num_indvs, 
                                                 trtA_effect_stage1 = trtA_effect_stage1,
                                                 trtA_effect_stage2 = trtA_effect_stage2,
                                                 diff_stage1 = diff_stage1,
                                                 diff_stage2 =  diff_stage2,
                                                 noise_sd = noise_sd,
                                                 w_weight = w_weight 
                                                )
  
  #simulation_pvalues <- matrix(unlist(simulation_scenarios), nrow=8)
  #simulation_pvalues <- matrix(unlist(simulation_scenarios), nrow=16)
  #binary 2 (A and B), continuous 2, log 2, exp 2, bayesian 2, bayesian log 2, bayesian exp 2  7*2=14 elements, 14*2=28 map
  
  simulation_pvalues <- matrix(unlist(simulation_scenarios), nrow=28)
  save(simulation_pvalues, file = paste0("./", scenario_folder, "/",
                                         
                                       
                                         "_n", num_indvs,
                                         "_trtA_effect_stage1",  trtA_effect_stage1,
                                         "_trtA_effect_stage2",  trtA_effect_stage2,
                                         "_diff_stage1",  diff_stage1,
                                         "_diff_stage2",  diff_stage2,
                                         "_w_weight",  w_weight,
                                         "_",options_numcpus,
                                         "_",options_jobid,
                                         ".RData"))
 
  #power <- simulation_pvalues[1,] 
 # if (num_replicas == 5000){
    power <- apply(simulation_pvalues, 1, function(x){sum(x<=alpha)/length(simulation_pvalues[1,])})
    power01 <- apply(simulation_pvalues, 1, function(x){sum(x<=alpha2)/length(simulation_pvalues[1,])})
    
 # }
  
  # if (num_replicas == 1000){
  #   power <- apply(simulation_pvalues, 1, function(x){sum(x>=(1-alpha/2))/length(simulation_pvalues[1,])})
  #   power01 <- apply(simulation_pvalues, 1, function(x){sum(x>=(1-alpha2/2))/length(simulation_pvalues[1,])})
  #   
  # }
  
  timeKeeperNext()
  
  
  return(list("power" = power, "power01" =  power01))
}
# 
# run_experiment_hypothesis <- function(exp_idx,
#                                       num_indvs,
#                                       trtA_effect,
#                                       diff_stage1,
#                                       diff_stage2 ,
#                                       noise_sd,
#                                       num_replicas = options_replicas,
#                                       alpha = 0.05,
#                                       alpha2 = 0.1)
# run_experiment_hypothesis (1,
#                                       400,
#                            3,
#                                       0.75,
#                            1.5,
#                            2,
#                                       num_replicas = 5,
#                                       alpha = 0.05,
#                                       alpha2 = 0.1)

begin_exp_time <- Sys.time()

set.seed(123456 + 10 * options_jobid)

#set.seed(123456 + 6 * options_jobid)
generate_ed_table <- function(subjects_vector,
                              diff_stage1,
                              trtA_effect_stage1_vector,
                              trtA_effect_stage2_vector,
                              diff_stage2_vector,
                              noise_sd_vector,
                              w_weight_vector
                              ){
  ed_table_ret <- expand.grid(subjects_vector,  trtA_effect_stage1_vector, trtA_effect_stage2_vector, diff_stage1, diff_stage2_vector, noise_sd_vector, w_weight_vector)
  return(ed_table_ret)
}



###################
#power

if (options_replicas == 1000){
  #diff_stage1 <- 0.75
  diff_stage1 <- 0.5
  #trtA_effect <- 3
  ed_table1 <- generate_ed_table(subjects_vector = c( 200, 400, 600),
                                 diff_stage1,
                                 # trtA_effect_vector = c(3, 3.5, 4, 4.5, 5, 5.5),
                                 # diff_stage2_vector = c(1.5, 2.5, 3.5, 4.5, 5.5,  6.5),
                                 trtA_effect_stage1_vector = c(3, 3.5, 4, 4.5, 5, 5.5),
                                 trtA_effect_stage2_vector = c(3, 3.5, 4, 4.5, 5, 5.5),
                                 diff_stage2_vector = c(1.5, 2.5, 3.5, 4.5, 5.5,  6.5),
                                 #noise_sd_vector = c(1, 2)
                                 noise_sd_vector = c(1),
                                 w_weight_vector = c (0.4, 0.5, 0.6)
                                 )
}

if (options_replicas == 5000){
  diff_stage1 <- 0
  #trtA_effect <- 2
  # ed_table1 <- generate_ed_table(subjects_vector = c(300,600),
  #                                diff_stage2_vector = c(0))
  ed_table1 <- generate_ed_table(subjects_vector = c(200, 400,600),
                                 diff_stage1,
                                 #trtA_effect,
                                 trtA_effect_stage1_vector = c(2),
                                 trtA_effect_stage2_vector = c(1),
                                 diff_stage2_vector = c(0),
                                 #noise_sd_vector = c(1, 4, 8, 16))
                                 #noise_sd_vector = c(1, 4),
                                 noise_sd_vector = c(1, 4),
                                 #w_weight_vector = c (0.4, 0.5, 0.6)
                                 w_weight_vector = c (0.6, 0.7, 0.8))
}

# ed_table1 <- generate_ed_table(subjects_vector = c(300,600),
#                                diff_stage2_vector = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5))

ed_table <- ed_table1
###################

colnames(ed_table) <- c("num_subjects","trtA_effect_stage1", "trtA_effect_stage2", "diff_stage1", "diff_stage2", "noise_sd", " w_weight")

##########################
all_experiment_outputs <- list()
for (row_index in 1:dim(ed_table)[1]){
  num_indvs <- ed_table[row_index,]$num_subjects
  trtA_effect_stage1 <- ed_table[row_index,]$trtA_effect_stage1
  trtA_effect_stage2 <- ed_table[row_index,]$trtA_effect_stage2
  diff_stage2 <- ed_table[row_index,]$diff_stage2
  diff_stage1 <- ed_table[row_index,]$diff_stage1
  noise_sd <- ed_table[row_index,]$noise_sd
  w_weight <- ed_table[row_index,]$w_weight
  experiment_output <- run_experiment_hypothesis( row_index,
                                                  num_indvs , 
                                                 trtA_effect_stage1,
                                                 trtA_effect_stage2,
                                                  diff_stage1,
                                                  diff_stage2,
                                                  noise_sd,
                                                 num_replicas = options_replicas,
                                                  w_weight)
  save(experiment_output, file = paste0("./", scenario_folder, "/",
                                       
                                        
                                        "_n", num_indvs, 
                                        "_trtA_effect_stage1",  trtA_effect_stage1,
                                        "_trtA_effect_stage2",  trtA_effect_stage2,
                                        "_diff_stage1",  diff_stage1,
                                        "_diff_stage2",  diff_stage2,
                                        "_w_weight",  w_weight,
                                        "_noisesd", noise_sd,
                                        "_", options_numcpus,
                                        "_", options_jobid,
                                        ".RData"))
  all_experiment_outputs <- rbind(all_experiment_outputs, experiment_output)
}

final_table <- cbind(ed_table, all_experiment_outputs)

save(final_table,file =paste0("./", final_table_folder, "/spcd_power_",
                              
                              "_", options_replicas,
                              "_", options_numcpus,
                              "_", options_jobid, ".RData"))

end_exp_time <- Sys.time()

cat("\n====================\n",
    "\tAll Experiemnts Took:", capture.output(end_exp_time - begin_exp_time), 
    "\n====================\n")

if(run_parallel)
{
  parallel::stopCluster(cl = my.cluster)
  initialized_parallel <- FALSE
}


#All Experiemnts Took: Time difference of 5.296393 mins (1000)
# All Experiemnts Took: Time difference of 2.395469 mins power

#tyie I: All Experiemnts Took: Time difference of 4.932501 mins 
#All Experiemnts Took: Time difference of 1.376206 mins 