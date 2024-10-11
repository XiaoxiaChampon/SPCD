
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

scenario_folder = "spcd_test_typeI"
ensure_dir_exist(scenario_folder)

final_table_folder = paste0("output_spcd_test_typeI")
ensure_dir_exist(final_table_folder)


# spcd_testing_simulation <- function (num_replicas, 
#                                      num_indvs,
#                                      diff_stage2 ,
#                                      n_groups = 3,
#                                      diff_stage1 = 0.5){
  spcd_testing_simulation <- function (num_replicas, 
                                       num_indvs,
                                       diff_stage1 ,
                                       diff_stage2 ,
                                       n_groups = 3,
                                       diff_stage1 = 0){
  cat("CFD Testing Simulation \nNum Replicas:\t", num_replicas)
  #num_replicas=2
  result_all <- foreach (number_simulation = 1:num_replicas, .combine = cbind, .init = NULL,
                         .packages=c("MASS")) %dorng% {
                           
                           source("./source_code/R/data_generator.R")
                           source("./source_code/R/spcd_testing.R")
                                                 
                           #######################################################################
                           #spcd_data_test <- spcd_data(n, n_groups, diff_stage1, diff_stage2)
                           # num_indvs <- 900
                           # n_groups <- 3
                           # diff_stage1 <- 0
                           # diff_stage2 <- 0
                           example_data <- spcd_data(num_indvs, n_groups, trtA_effct, diff_stage1, diff_stage2)
                           
                           non_responders <- example_data$spcd_data
                           result <- hypothesis_testing (non_responders)
                           
                           non_responders2 <- example_data$spcd_data_yes
                           result2 <- hypothesis_testing (non_responders2)
                           
                           # return(list("binary_result"=result$binary_result,"continuous_result"=result$continuous_result,
                           #             "continous_map1"=result$continous_map1,"continous_map2"=result$continous_map2))
                           
                           return(list("binary_result"=result$binary_result,"continuous_result"=result$continuous_result,
                                       "continous_map1"=result$continous_map1,"continous_map2"=result$continous_map2,
                                       "binary_result2"=result2$binary_result,"continuous_result2"=result2$continuous_result,
                                       "continous_map12"=result2$continous_map1,"continous_map22"=result2$continous_map2))
                         } 
  #T_rep <- do.call(rbind, T_rep)
  return(result_all)
  
}

#set.seed(123456 + 10 * options_jobid)
# set.seed(123456 + 10 * 2)
 # result_all <- spcd_testing_simulation  (num_replicas = 6,num_indvs= 300,diff_stage2 = 1.5,n_groups = 3,
 #                                         diff_stage1 = 0.5)
# simulation_pvalues <- matrix(unlist(result_all), nrow=8)
# power <- apply(simulation_pvalues, 1, function(x){sum(x<=0.05)/length(simulation_pvalues[1,])})
# power01 <- apply(simulation_pvalues, 1, function(x){sum(x<=0.1)/length(simulation_pvalues[1,])})


source("./source_code/R/time_track_function.R")
run_experiment_hypothesis <- function(exp_idx,
                                      num_indvs,
                                      diff_stage2,
                                      num_replicas = options_replicas,
                                      alpha = 0.05,
                                      alpha2 = 0.1){
  
  exp_str <- paste("Track time for \nNum Subjects:\t", num_indvs,
                   "\n diff_stage2:\t",diff_stage2)
  writeLines(exp_str)
  timeKeeperStart(exp_str)
  simulation_scenarios <- spcd_testing_simulation(num_replicas=num_replicas, 
                                                 num_indvs=num_indvs, 
                                                 diff_stage2 =  diff_stage2
                                                )
  
  #simulation_pvalues <- matrix(unlist(simulation_scenarios), nrow=8)
  simulation_pvalues <- matrix(unlist(simulation_scenarios), nrow=16)
  save(simulation_pvalues, file = paste0("./", scenario_folder, "/",
                                         
                                       
                                         "_n", num_indvs,
                                         "_ diff_stage2",  diff_stage2,
                                         "_",options_numcpus,
                                         "_",options_jobid,
                                         ".RData"))
 
  #power <- simulation_pvalues[1,] 
  power <- apply(simulation_pvalues, 1, function(x){sum(x<=alpha)/length(simulation_pvalues[1,])})
  power01 <- apply(simulation_pvalues, 1, function(x){sum(x<=alpha2)/length(simulation_pvalues[1,])})
  
  
  timeKeeperNext()
  
  
  return(list("power" = power, "power01" =  power01))
}
# 
# run_experiment_hypothesis <- function(exp_idx,
#                                       num_indvs,
#                                       diff_stage2,
#                                       num_replicas = options_replicas,
#                                       alpha = 0.05,
#                                       alpha2 = 0.1)
# run_experiment_hypothesis (1,
#                                       300,
#                                       1.5,
#                                       num_replicas = 5,
#                                       alpha = 0.05,
#                                       alpha2 = 0.1)

begin_exp_time <- Sys.time()

#set.seed(123456 + 10 * options_jobid)

set.seed(123456 + 6 * options_jobid)
generate_ed_table <- function(subjects_vector,
                              diff_stage2_vector
                              ){
  ed_table_ret <- expand.grid(subjects_vector, diff_stage2_vector)
  return(ed_table_ret)
}



###################
#power
if (options_replicas == 1000){
  trtA_effct <- 3
  diff_stage1 <- 0.75
  ed_table1 <- generate_ed_table(subjects_vector = c(200,400, 600),
                                 diff_stage2_vector = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
                                 )
}

if (options_replicas == 5000){
  trtA_effct <- 2
  diff_stage1 <- 0
  # ed_table1 <- generate_ed_table(subjects_vector = c(300,600),
  #                                diff_stage2_vector = c(0))
  ed_table1 <- generate_ed_table(subjects_vector = c(200, 400,600),
                                 diff_stage2_vector = c(0))
}

# ed_table1 <- generate_ed_table(subjects_vector = c(300,600),
#                                diff_stage2_vector = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5))

ed_table <- ed_table1
###################

colnames(ed_table) <- c("num_subjects", "diff_stage2")

##########################
all_experiment_outputs <- list()
for (row_index in 1:dim(ed_table)[1]){
  num_indvs <- ed_table[row_index,]$num_subjects
  diff_stage2 <- ed_table[row_index,]$diff_stage2

  experiment_output <- run_experiment_hypothesis( row_index,
                                                  num_indvs , 
                                                  diff_stage2)
  save(experiment_output, file = paste0("./", scenario_folder, "/",
                                       
                                        
                                        "_n", num_indvs, 
                                        "_dff2", diff_stage2,
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


