rm(list = ls())

source("Functions_simulations.R")
source("train_testplot.R")


library(mice)     # for imputation and amputation
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

proj_dir <- "/Users/Shehw001/Documents/GitHub/mice_xgboost_simulation/Simulation_SampleEstimates/"
setwd(proj_dir)


seed <- 123
Num_ds <- 100
prop_values <- c(20, 40, 60, 80)
m <- 5
maxit <- 5

set.seed(seed) 

plan(sequential)
available_cores <- availableCores() - 2
plan(multisession, workers = available_cores)

#################################################################################
###                       Data loading                                      ####
#################################################################################

load("Data/simdata.RData")
load("Data/missing_MAR_list.RData")
load("Data/True_coefEstimated.RData")

#################################################################################
####################.           3. Imputation     ###############################
#################################################################################


############################     3.1 - Default    ##################################

print("starting default imputation")

impute_MAR_default <- mice_noparam(missing_MAR_list, method = "pmm", m, maxit)

mice_results_default <- map(impute_MAR_default, ~ map(., "results")) #  extract imputation results
time_default <- map(impute_MAR_default, ~ map(., "time_taken"))   # extract time taken for imputation

eval_default <- eval_func(mice_results_default, coef_true, Num_ds)

save(mice_results_default, file = "Imputation/MiceOtherMethods/Default_imputation.RData") # imputation result
save(time_default, file = "Imputation/MiceOtherMethods/Default_time.RData") # imputation time
save(eval_default, file = "Imputation/MiceOtherMethods/Default_evaluation.RData") #regression results

rm(mice_results_default,time_default,eval_default,impute_MAR_default)

############################     3.2 -Imputation using  RF    ##################################

print("starting RF")

impute_MAR_RF <- mice_noparam(missing_MAR_list, method = "rf", m, maxit)

mice_results_rf <- map(impute_MAR_RF, ~ map(., "results"))  #  extract imputation results   
time_RF <- map(impute_MAR_RF, ~ map(., "time_taken")) #  extract time taken for imputation results

eval_RF <-  eval_func(mice_results_rf, coef_true, Num_ds)

save(mice_results_rf, file = "Imputation/MiceOtherMethods/RF_imputation_nonlinear.RData") # save imputation results
save(time_RF, file = "Imputation/MiceOtherMethods/RF_time_nonlinear.RData") # save time taken for imputation results
save(eval_RF, file = "Imputation/MiceOtherMethods/RF_evaluation_nonlinear.RData") # save regression results fitted on imputed data

rm(mice_results_rf,time_RF,eval_RF,impute_MAR_RF)


###################################     3.3 - CART   ##############################################

print("starting CART imputation")

impute_MAR_cart <-  mice_noparam(missing_MAR_list, method = "cart", m, maxit)

mice_results_cart <- map(impute_MAR_cart, ~ map(., "results")) # Extracting imputation results
time_CART <- map(impute_MAR_cart, ~ map(., "time_taken")) # Time taken for imputation

eval_CART <- eval_func(mice_results_cart, coef_true, Num_ds)


save(mice_results_cart, file = "Imputation/MiceOtherMethods/CART_imputation.RData") # Save imputation results
save(time_CART, file = "Imputation/MiceOtherMethods/CART_time.RData")  # Save time taken for imputation
save(eval_CART, file = "Imputation/MiceOtherMethods/CART_evaluation.RData") # save regression results

rm(mice_results_cart,time_CART,eval_CART,impute_MAR_cart)

