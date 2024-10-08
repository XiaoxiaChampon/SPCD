
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
# Purpose: Simulations for Categorical Functional Data Hypothesis Testing when consdiering a randome variable
#         
# Author:  Xiaoxia Champon
# Date: 8/27/2024
#
##############################################################



library(mgcv)
library(fda)
library(fda.usc)
#library(devtools)
#install_github("stchen3/glmmVCtest")
#library("glmmVCtest")
#library(RLRsim)
library(MASS)
library(splines)
library(parallel)
library(stats)
library(matrixStats)
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
              help="Num Replicas", metavar="NUMREPLICAS"),
  # make_option(c("-s", "--subjects"), type="integer", default=100,
  #             help="Num Subjects/Individuals", metavar="NUMSUBJECTS"),
  # make_option(c("-b", "--boots"), type="integer", default=100,
  #             help="Num Bootstraps", metavar="NUMBOOTS"),
  make_option(c("-l", "--timelength"), type="integer", default=90,
              help="Time Length", metavar="TIMELENGTH")
)

#####need for hazel
# Create parser and parse options
parser <- OptionParser(option_list=option_list)
options <- parse_args(parser)

# options_jobid <- options$jobid
# options_numcpus <- options$numcpus
# options_replicas <- options$replicas
# #options_subjects <- options$subjects
# # options_boots <- options$boots
# options_timelength <- options$timelength
#####################

options_jobid <- 1
options_numcpus <- 10
options_replicas <- 6
# options_subjects <- 100
# options_boots <- 100
#options_timelength <- 90
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

scenario_folder = "spcd_test"
ensure_dir_exist(scenario_folder)

final_table_folder = paste0("output_spcd_test")
ensure_dir_exist(final_table_folder)


spcd_testing_simulation <- function (num_replicas, 
                                     num_indvs,
                                     diff_stage2 ,
                                     n_groups = 3,
                                     diff_stage1 = 0.5){
  cat("CFD Testing Simulation \nNum Replicas:\t", num_replicas)
  #num_replicas=2
  result_all <- foreach (number_simulation = 1:num_replicas, .combine = cbind, .init = NULL,
                         .packages=c("splines","mgcv","fda","fda.usc","MASS")) %dorng% {
                           # result_all <- foreach (number_simulation = 1:num_replicas, .combine = cbind, .init = NULL) %dorng% {
                           source("./source_code/R/data_generator.R")
                           source("./source_code/R/spcd_testing.R")
                           #T_rep <- foreach(this_row = 1:num_replicas ) %dorng%                        
                           #######################################################################
                           #spcd_data_test <- spcd_data(n, n_groups, diff_stage1, diff_stage2)
                           non_responders <- spcd_data(num_indvs, n_groups, diff_stage1, diff_stage2)
                           result <- hypothesis_testing (non_responders)
                           
                           return(list("binary_result"=result$binary_result,"continuous_result"=result$continuous_result,
                                       "continous_map1"=result$continous_map1,"continous_map2"=result$continous_map2))
                         } 
  #T_rep <- do.call(rbind, T_rep)
  return(result_all)
  
}

#set.seed(123456 + 10 * options_jobid)
# set.seed(123456 + 10 * 2)
 result_all <- spcd_testing_simulation  (5,
                                      300,1.5,
                                      n_groups=3)
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
  
  simulation_pvalues <- matrix(unlist(simulation_scenarios), nrow=8)
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

set.seed(123456 + 10 * options_jobid)


generate_ed_table <- function(subjects_vector,
                              diff_stage2_vector
                              ){
  ed_table_ret <- expand.grid(subjects_vector, diff_stage2_vector)
  return(ed_table_ret)
}

########
#type I error rate
# ed_table1=generate_ed_table()
# ed_table2=generate_ed_table(fl_choice_vector = c("200","7","21"),
#                                                          test_type_vector = c("Functional"))
# 
# 
# ed_table <- rbind(ed_table1,ed_table2)

###################
#power
ed_table1 <- generate_ed_table(subjects_vector = c(300,600),
                               diff_stage2_vector = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5))
# ed_table2 <- generate_ed_table(subjects_vector = c(100, 300, 500),
#                                fl_choice_vector = c("200","7","21"),
#                                test_type_vector = c("Functional"))
#ed_table <- rbind(ed_table1,ed_table2)
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


# mu1_coef=c(16.1149728, -85.34458, -87.923671)
# mu2_coef=c(-97.98046, 50.39012, 58.19331 )
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


###final_table[,1:4]
# final_table[,1:4]
# fl_choice test_type num_subjects num_timepoints
# experiment_output           6 Inclusion          100             90
# experiment_output.1        21 Inclusion          100             90
# experiment_output.2         6 Inclusion          300             90
# experiment_output.3        21 Inclusion          300             90
# experiment_output.4         6 Inclusion          500             90
# experiment_output.5        21 Inclusion          500             90
# sum(unlist(final_table[1,]$power)<=0.05)/length(unlist(final_table[1,]$power))
# sum(unlist(final_table[3,]$power)<=0.05)/length(unlist(final_table[3,]$power))
# sum(unlist(final_table[5,]$power)<=0.05)/length(unlist(final_table[5,]$power))
#0.0398,0.0552,0.0564
# 
# sum(unlist(final_table[2,]$power)<=0.05)/length(unlist(final_table[2,]$power))
# sum(unlist(final_table[4,]$power)<=0.05)/length(unlist(final_table[4,]$power))
# sum(unlist(final_table[6,]$power)<=0.05)/length(unlist(final_table[6,]$power))
#0.042,  0.0516, 0.058

# sum(unlist(final_table[1,]$power)<=0.1)/length(unlist(final_table[1,]$power))
# sum(unlist(final_table[3,]$power)<=0.1)/length(unlist(final_table[3,]$power))
# sum(unlist(final_table[5,]$power)<=0.1)/length(unlist(final_table[5,]$power))
#0.0962, 0.1156,  0.1166
# sum(unlist(final_table[2,]$power)<=0.1)/length(unlist(final_table[2,]$power))
# sum(unlist(final_table[4,]$power)<=0.1)/length(unlist(final_table[4,]$power))
# sum(unlist(final_table[6,]$power)<=0.1)/length(unlist(final_table[6,]$power))
#0.0944, 0.1146, 0.1196

