rm(list = ls())

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
maxit <- 1

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

###################################################################################################
############################     3.7 -Imputation using  mixgb    ##################################

##################        PMM = NULL      #############################

print("starting Mixgb Imputation")

mixgb_maxit1_pmmNULL_tmp <- missing_MAR_list %>% # for each percentage
  map(function(mat_list) {
    furrr::future_map(mat_list, function(mat) { # for each dataset
      result <- system.time({
        mice_result <- mixgb(mat,pmm.type = NULL,print = FALSE,m =m, maxit = maxit)
      })
      list(mice_result = mice_result, time_taken = result) # Combining both imputation result and time taken
    }, .options = furrr_options(seed = TRUE))
  })


mixgb_maxit1_pmmNULL_imp <- map(mixgb_maxit1_pmmNULL_tmp, ~ map(., "mice_result"))  #  extract imputation results   
mixgb_maxit1_pmmNULL_time <- map(mixgb_maxit1_pmmNULL_tmp, ~ map(., "time_taken")) #  extract time taken for imputation results


mixgb_maxit1_pmmNULL_eval <- mixgb_maxit1_pmmNULL_imp %>%
  map(function(mat_list) {
    furrr::future_imap(mat_list, function(mat, idx) { 
      results <- mat %>%
        map(~.x %$% lm(y ~ x + z + x:z + I(z^2))) %>%
        pool() %>%
        summary(conf.int = TRUE)
      # Dynamically create the column name using the inner index
      term_col <- paste0("V", idx)
      results <- results %>%
        left_join(coef_true %>%
                    select(term, all_of(term_col)) %>%
                    rename(true_vals = !!sym(term_col)), by = "term") %>%
        mutate(
          cov = conf.low < true_vals & true_vals < conf.high,  # coverage
          bias = estimate - true_vals,
          width = conf.high - conf.low  # bias
        ) %>%
        column_to_rownames("term")
      
      return(results)
      
    }, .options = furrr_options(seed = TRUE)) %>%
      Reduce("+", .) / Num_ds
  })



save(mixgb_maxit1_pmmNULL_imp, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmmNULL_imp.RData") # save imputation results
save(mixgb_maxit1_pmmNULL_time, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmmNULL_time.RData") # save time taken for imputation results
save(mixgb_maxit1_pmmNULL_eval, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmmNULL_eval.RData") # save regression results fitted on imputed data


rm(mixgb_maxit1_pmmNULL_tmp, mixgb_maxit1_pmmNULL_imp, mixgb_maxit1_pmmNULL_time, mixgb_maxit1_pmmNULL_eval)


####################################################### pmm = 1 ##############################################

mixgb_maxit1_pmm1_tmp <- missing_MAR_list %>% # for each percentage
  map(function(mat_list) {
    furrr::future_map(mat_list, function(mat) { # for each dataset
      result <- system.time({
        mice_result <- mixgb(mat,pmm.type = 1, 
                             print = FALSE,m =m, maxit = maxit)
      })
      list(mice_result = mice_result, time_taken = result) # Combining both imputation result and time taken
    }, .options = furrr_options(seed = TRUE))
  })


mixgb_maxit1_pmm1_imp <- map(mixgb_maxit1_pmm1_tmp, ~ map(., "mice_result"))  #  extract imputation results   
mixgb_maxit1_pmm1_time <- map(mixgb_maxit1_pmm1_tmp, ~ map(., "time_taken")) #  extract time taken for imputation results


mixgb_maxit1_pmm1_eval <- mixgb_maxit1_pmm1_imp %>% 
  map(function(mat_list) {
    furrr::future_imap(mat_list, function(mat, idx) { 
      results <- mat %>%
        map(~.x %$% lm(y ~ x + z + x:z + I(z^2))) %>%
        pool() %>%
        summary(conf.int = TRUE)
      # Dynamically create the column name using the inner index
      term_col <- paste0("V", idx)
      results <- results %>%
        left_join(coef_true %>%
                    select(term, all_of(term_col)) %>%
                    rename(true_vals = !!sym(term_col)), by = "term") %>%
        mutate(
          cov = conf.low < true_vals & true_vals < conf.high,  # coverage
          bias = estimate - true_vals,
          width = conf.high - conf.low  # bias
        ) %>%
        column_to_rownames("term")
      
      return(results)
      
    }, .options = furrr_options(seed = TRUE)) %>%
      Reduce("+", .) / Num_ds
  })


save(mixgb_maxit1_pmm1_imp, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmm1_imp.RData") # save imputation results
save(mixgb_maxit1_pmm1_time, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmm1_time.RData") # save time taken for imputation results
save(mixgb_maxit1_pmm1_eval, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmm1_eval.RData") # save regression results fitted on imputed data


rm(mixgb_maxit1_pmm1_tmp, mixgb_maxit1_pmm1_imp, mixgb_maxit1_pmm1_time, mixgb_maxit1_pmm1_eval)


##################### pmm = 2 ##############################

mixgb_maxit1_pmm2_tmp <- missing_MAR_list %>% # for each percentage
  map(function(mat_list) {
    furrr::future_map(mat_list, function(mat) { # for each dataset
      result <- system.time({
        mice_result <- mixgb(mat,pmm.type = 2,
                             print = FALSE,m =m, maxit = maxit)
      })
      list(mice_result = mice_result, time_taken = result) # Combining both imputation result and time taken
    }, .options = furrr_options(seed = TRUE))
  })

mixgb_maxit1_pmm2_imp <- map(mixgb_maxit1_pmm2_tmp, ~ map(., "mice_result"))  #  extract imputation results   
mixgb_maxit1_pmm2_time <- map(mixgb_maxit1_pmm2_tmp, ~ map(., "time_taken")) #  extract time taken for imputation results


mixgb_maxit1_pmm2_eval <- mixgb_maxit1_pmm2_imp %>% 
  map(function(mat_list) {
    furrr::future_imap(mat_list, function(mat, idx) { 
      results <- mat %>%
        map(~.x %$% lm(y ~ x + z + x:z + I(z^2))) %>%
        pool() %>%
        summary(conf.int = TRUE)
      # Dynamically create the column name using the inner index
      term_col <- paste0("V", idx)
      results <- results %>%
        left_join(coef_true %>%
                    select(term, all_of(term_col)) %>%
                    rename(true_vals = !!sym(term_col)), by = "term") %>%
        mutate(
          cov = conf.low < true_vals & true_vals < conf.high,  # coverage
          bias = estimate - true_vals,
          width = conf.high - conf.low  # bias
        ) %>%
        column_to_rownames("term")
      
      return(results)
      
    }, .options = furrr_options(seed = TRUE)) %>%
      Reduce("+", .) / Num_ds
  })

save(mixgb_maxit1_pmm2_imp, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmm2_imp.RData") # save imputation results
save(mixgb_maxit1_pmm2_time, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmm2_time.RData") # save time taken for imputation results
save(mixgb_maxit1_pmm2_eval, file = "Imputation/Mixgb/iter1/mixgb_maxit1_pmm2_eval.RData") # save regression results fitted on imputed data

rm(mixgb_maxit1_pmm2_tmp, mixgb_maxit1_pmm2_imp, mixgb_maxit1_pmm2_time, mixgb_maxit1_pmm2_eval)

###########################################################################################################
###########################################################################################################
###########################################################################################################

