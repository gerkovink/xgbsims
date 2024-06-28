rm(list = ls())

setwd("/Users/Shehw001/Documents/GitHub/mice_xgboost_simulation/Simulation_SampleEstimates")

source("Functions_simulations.R")
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
maxit <- 1

set.seed(seed) 


plan(sequential)
available_cores <- availableCores() - 2
plan(multisession, workers = available_cores)


dir_name <- "Imputation/mice_xgb_default"


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


###########    3.4 - XGBoost - default parameter (match.type: predicted)  #################


print("starting Mice-xgb (default parameters) imputation")

impute_MAR_xgb_default <-  mice_run(missing_MAR_list,G1_param_res = rep(list(rep(list(NULL),Num_ds)),length(prop_values)), 
                             pred = "predicted", method = "xgb", m, maxit)

xgb_ParamDefault_maxit5_P_imp_res <- map(impute_MAR_xgb_default, ~ map(., "results")) # Extracting imputation results
xgb_ParamDefault_maxit5_P_imp_time <- map(impute_MAR_xgb_default, ~ map(., "time_taken")) # Time taken for imputation

xgb_ParamDefault_maxit5_P_eval <- eval_func(xgb_ParamDefault_maxit5_P_imp_res, coef_true, Num_ds)

save(xgb_ParamDefault_maxit5_P_imp_res, file = sprintf("%s/xgb_ParamDefault_maxit5_P_imp_res.RData",dir_name))
save(xgb_ParamDefault_maxit5_P_imp_time, file =  sprintf("%s/xgb_ParamDefault_maxit5_P_imp_time.RData",dir_name))
save(xgb_ParamDefault_maxit5_P_eval, file =  sprintf("%s/xgb_ParamDefault_maxit5_P_eval.RData",dir_name))

train_testplot(simdata, xgb_ParamDefault_maxit5_P_imp_res,prop_values, sprintf("%s/TT_xgb_ParamDefault_maxit5_P_imp_res.pdf",dir_name))

rm(xgb_ParamDefault_maxit5_P_imp_res,xgb_ParamDefault_maxit5_P_imp_time,xgb_ParamDefault_maxit5_P_eval,impute_MAR_xgb_default)


########################## match.type = "predicted.observed" - default param ############################

impute_MAR_xgb_default_PO <- mice_run(missing_MAR_list,G1_param_res = rep(list(rep(list(NULL),Num_ds)),length(prop_values)), 
                                      pred = "predicted.observed", method = "xgb", m, maxit)


xgb_ParamDefault_maxit5_PO_imp_res <- map(impute_MAR_xgb_default_PO, ~ map(., "results"))  # imputation results
xgb_ParamDefault_maxit5_PO_imp_time <- map(impute_MAR_xgb_default_PO, ~ map(., "time_taken"))           # Time taken

xgb_ParamDefault_maxit5_PO_eval <- eval_func(xgb_ParamDefault_maxit5_PO_imp_res, coef_true, Num_ds)

save(xgb_ParamDefault_maxit5_PO_imp_res, file = sprintf("%s/xgb_ParamDefault_maxit5_PO_imp_res.RData",dir_name))
save(xgb_ParamDefault_maxit5_PO_imp_time, file = sprintf("%s/xgb_ParamDefault_maxit5_PO_imp_time.RData",dir_name))
save(xgb_ParamDefault_maxit5_PO_eval, file = sprintf("%s/xgb_ParamDefault_maxit5_PO_eval.RData",dir_name))

train_testplot(simdata, xgb_ParamDefault_maxit5_PO_imp_res,prop_values, sprintf("%s/TT_xgb_ParamDefault_maxit5_PO_imp_res.pdf",dir_name))

rm(xgb_ParamDefault_maxit5_PO_imp_res, xgb_ParamDefault_maxit5_PO_imp_time,xgb_ParamDefault_maxit5_PO_eval,impute_MAR_xgb_default_PO)

########################## match.type = "original.observed" - default param ############################

impute_MAR_xgb_default_OO <- mice_run(missing_MAR_list,G1_param_res = rep(list(rep(list(NULL),Num_ds)),length(prop_values)), 
                                      pred = "original.observed", method = "xgb", m, maxit)

xgb_ParamDefault_maxit5_OO_imp_res <- map(impute_MAR_xgb_default_OO, ~ map(., "results"))  # imputation results
xgb_ParamDefault_maxit5_OO_imp_time <- map(impute_MAR_xgb_default_OO, ~ map(., "time_taken"))           # Time taken

xgb_ParamDefault_maxit5_OO_eval <- eval_func(xgb_ParamDefault_maxit5_OO_imp_res, coef_true, Num_ds)


save(xgb_ParamDefault_maxit5_OO_imp_res, file = sprintf("%s/xgb_ParamDefault_maxit5_OO_imp_res.RData",dir_name))
save(xgb_ParamDefault_maxit5_OO_imp_time, file = sprintf("%s/xgb_ParamDefault_maxit5_OO_imp_time.RData",dir_name))
save(xgb_ParamDefault_maxit5_OO_eval, file = sprintf("%s/xgb_ParamDefault_maxit5_OO_eval.RData",dir_name))

train_testplot(simdata, xgb_ParamDefault_maxit5_OO_imp_res,prop_values, sprintf("%s/TT_xgb_ParamDefault_maxit5_OO_imp_res.pdf",dir_name))

rm(xgb_ParamDefault_maxit5_OO_imp_res, xgb_ParamDefault_maxit5_OO_imp_time,xgb_ParamDefault_maxit5_OO_eval, impute_MAR_xgb_default_OO)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


