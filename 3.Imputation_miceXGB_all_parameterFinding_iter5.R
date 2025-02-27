rm(list = ls())

setwd("/Users/Shehw001/Documents/GitHub/mice_xgboost_simulation/Simulation_SampleEstimates")

source("seqParamOpt/Functions_simulations.R")
source("param_optimization.R")


library(purrr)    # for functional programming
library(furrr)    # for functional futures
library(mvtnorm)  # for multivariate normal data
library(magrittr) # for pipes
library(dplyr)    # for data manipulation
library(tibble)   # for tibbles
library(mixgb) 
library(microbenchmark)
library(ggplot2)
library(tidyr)
library(future)
library(cowplot)

source("train_testplot.R")

seed <- 123
Num_ds <- 100
prop_values <- c(20, 40, 60, 80)
m <- 5
maxit <- 5

set.seed(seed) 


plan(sequential)
available_cores <- availableCores() - 2
plan(multisession, workers = available_cores)


dir_name <- "Imputation/Mice_xgb_All"
dir_name_iter <- "Imputation/Mice_xgb_All/iter5"



#################################################################################
###                       Data loading                                      ####
#################################################################################

load("Data/simdata.RData")
load("Data/missing_MAR_list.RData")
load("Data/True_coefEstimated.RData")

iterations = 50

#################################################################################
####################.           3. Imputation     ###############################
#################################################################################

########################    3.6 - XGBoost - All parameter ####################################

G1_param_out <- param_cal_run(missing_MAR_list, response_var = "all", features=NULL, iters = iterations, 
                              fixedPAram = NULL, tuneParam = c("eta", "nrounds"), plots=FALSE) # find parameters using G1 group

G1_param_res <- map(G1_param_out, ~ map(., "params"))                 # parameter results
G1_param_time <- map(G1_param_out, ~ map(., "time_taken"))           # Time taken for parameter optimization

save(G1_param_res, file = sprintf("%s/G1_parameters.RData", dir_name))
save(G1_param_time, file = sprintf("%s/G1_parameters_time.RData", dir_name))

##################################################################################################################

G2_param_out <- param_cal_fixed(missing_MAR_list, G1_param_res, response_var = "all", features = NULL, 
                                iters = iterations, tuneParam = c("max_depth","min_child_weight", "subsample"), plots = FALSE)

G2_param_res <- map(G2_param_out, ~ map(., "params"))   #     imputation results
G2_param_time <- map(G2_param_out, ~ map(., "time_taken"))           # Time taken

save(G2_param_res, file = sprintf("%s/G2_parameters.RData", dir_name))
save(G2_param_time, file = sprintf("%s/G2_parameters_time.RData", dir_name))

###########################################################################################

load(sprintf("%s/G2_parameters.RData", dir_name))

mice_allParam_predicted_maxit5 <- mice_run(missing_MAR_list,G2_param_res, pred = "predicted", method = "xgb", m, maxit)

mice_allParam_predicted_maxit5_res <- map(mice_allParam_predicted_maxit5, ~ map(., "results")) # Extracting imputation results
mice_allParam_predicted_maxit5_time <- map(mice_allParam_predicted_maxit5, ~ map(., "time_taken")) # Time taken for imputation

eval_allParam_predicted_maxit5 <- eval_func(mice_allParam_predicted_maxit5_res, coef_true, Num_ds)

train_testplot(simdata,mice_allParam_predicted_maxit5_res, prop_values, sprintf("%s/TT_mice_G2_predicted.pdf", dir_name_iter))

save(mice_allParam_predicted_maxit5_res, file = sprintf("%s/Mice_xgb_predicted_All.RData", dir_name_iter))
save(eval_allParam_predicted_maxit5, file = sprintf("%s/Mice_xgb_predicted_All_eval.RData", dir_name_iter))
save(mice_allParam_predicted_maxit5_time, file = sprintf("%s/time_Mice_xgb_predicted_All.RData", dir_name_iter))

########################## match.type = "predicted.observed" - All ############################

mice_allParam_predicted_observed_maxit5 <- mice_run(missing_MAR_list,G2_param_res, pred = "predicted.observed", method = "xgb",m,maxit)

mice_allParam_predicted_observed_maxit5_res <- map(mice_allParam_predicted_observed_maxit5, ~ map(., "results")) # Extracting imputation results
mice_allParam_predicted_observed_maxit5_time <- map(mice_allParam_predicted_observed_maxit5, ~ map(., "time_taken")) # Time taken for imputation

eval_allParam_predicted_observed_maxit5 <- eval_func(mice_allParam_predicted_observed_maxit5_res, coef_true, Num_ds)

train_testplot(simdata,mice_allParam_predicted_observed_maxit5_res, prop_values, sprintf("%s/TT_mice_G2_predicted_observed.pdf", dir_name_iter))

save(mice_allParam_predicted_observed_maxit5_res, file = sprintf("%s/Mice_xgb_predicted_observed_All.RData", dir_name_iter))
save(eval_allParam_predicted_observed_maxit5, file = sprintf("%s/Mice_xgb_predicted_observed_All_eval.RData", dir_name_iter))
save(mice_allParam_predicted_observed_maxit5_time, file = sprintf("%s/time_Mice_xgb_predicted_observed_All.RData", dir_name_iter))

########################## match.type = "original.observed" - All ############################

mice_allParam_original_observed_maxit5 <- mice_run(missing_MAR_list,G2_param_res, pred = "original.observed", method = "xgb", m, maxit)

mice_allParam_original_observed_maxit5_res <- map(mice_allParam_original_observed_maxit5, ~ map(., "results")) # Extracting imputation results
mice_allParam_original_observed_maxit5_time <- map(mice_allParam_original_observed_maxit5, ~ map(., "time_taken")) # Time taken for imputation

eval_allParam_original_observed_maxit5 <- eval_func(mice_allParam_original_observed_maxit5_res, coef_true, Num_ds)

train_testplot(simdata,mice_allParam_original_observed_maxit5_res, prop_values, sprintf("%s/TT_mice_G2_original_observed.pdf", dir_name_iter))

save(mice_allParam_original_observed_maxit5_res, file = sprintf("%s/Mice_xgb_original_observed_All.RData", dir_name_iter))
save(eval_allParam_original_observed_maxit5, file = sprintf("%s/Mice_xgb_original_observed_All_eval.RData", dir_name_iter))
save(mice_allParam_original_observed_maxit5_time, file = sprintf("%s/time_Mice_xgb_original_observed_All.RData", dir_name_iter))



