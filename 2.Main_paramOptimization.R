rm(list = ls())


#########################################################################################

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
library(cowplot)

#########################################################################################


seed <- 123
Num_ds <- 10
prop_values <- c(20,60)
m <- 5
maxit <- 5

set.seed(seed) 

########################################################################################
##############################     load data     #######################################
########################################################################################

load("Data/simdata.RData")
load("Data/missing_MAR_list.RData")
load("Data/True_coefEstimated.RData")

simdata <- simdata[1:Num_ds]
missing_MAR_list <- missing_MAR_list[c(1,3)]
missing_MAR_list <- lapply(missing_MAR_list, function(sublist) {
  sublist[1:Num_ds]
})

coef_true <- coef_true[,1:(Num_ds+1)]

##################################################################################################
###                                     XGB default
##################################################################################################

impute_MAR_xgb_default <- mice_run(missing_MAR_list,G1_param_res=rep(list(rep(list(NULL),Num_ds)),2), pred = "predicted.observed", 
         method = "xgb", m , maxit)

xgb_ParamDefault_maxit5_P_imp_res <- map(impute_MAR_xgb_default, ~ map(., "results"))  # imputation results
xgb_ParamDefault_maxit5_P_imp_time <- map(impute_MAR_xgb_default, ~ map(., "time_taken"))           # Time taken


xgb_ParamDefault_maxit5_P_eval <- eval_func(xgb_ParamDefault_maxit5_P_imp_res, coef_true, Num_ds)
  
save(xgb_ParamDefault_maxit5_P_eval,file =  "ParameterOptimization/Mice_XGB_default.RData")
save(xgb_ParamDefault_maxit5_P_imp_time, file = "ParameterOptimization/Mice_XGB_default_time.RData")

##################################################################################################


param_optimization(missing_MAR_list, seed, coef_true, iterations = 50, dir_name = "ParameterOptimization", prediction = "predicted.observed", m, maxit)

param_optimization(missing_MAR_list, seed, coef_true, iterations = 75, dir_name = "ParameterOptimization", prediction = "predicted.observed", m, maxit)

param_optimization(missing_MAR_list, seed, coef_true, iterations = 100, dir_name = "ParameterOptimization", prediction = "predicted.observed", m, maxit)

