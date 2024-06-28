

param_optimization <- function(missing_MAR_list, seed, coef_true, iterations, dir_name, prediction, m, maxit){
  
  #set up parallel processing and set up seed
  set.seed(seed)
  plan(sequential)
  available_cores <- availableCores() - 1
  plan(multisession, workers = available_cores)
  
  
  
  #####################################################################################
  ###########                       G1
  #####################################################################################
  
  G1_param_out <- param_cal_run(missing_MAR_list, response_var = "all", features=NULL, 
                                iters = iterations, fixedPAram = NULL, tuneParam = c("eta", "nrounds")) # find parameters using G1 group
  
  G1_param_res <- map(G1_param_out, ~ map(., "params"))                 # parameter results
  G1_param_time <- map(G1_param_out, ~ map(., "time_taken"))           # Time taken for parameter optimization
  
  Map(function(x, name) {
    extract_plots(x, sprintf("%s/G1_param_%s_%s.pdf",dir_name, name, iterations))
  }, G1_param_res, names(G1_param_res))
  
  mice_G1 <- mice_run(missing_MAR_list,G1_param_res, pred = prediction, method = "xgb", m, maxit)
  
  mice_G1_res <- map(mice_G1, ~ map(., "results"))                 # mice results
  
  eval_G1 <- eval_func(mice_G1_res, coef_true, Num_ds)
  
  
  #####################################################################################
  ###########                       G2
  #####################################################################################
  
  G2_param_out <- param_cal_fixed(missing_MAR_list, G1_param_res, response_var = "all", features = NULL, 
                                  iters = iterations, tuneParam = c("max_depth","min_child_weight", "subsample"))
  
  G2_param_res <- map(G2_param_out, ~ map(., "params"))  # imputation results
  G2_param_time <- map(G2_param_out, ~ map(., "time_taken"))           # Time taken
  
  
  Map(function(x, name) {
    extract_plots(x, sprintf("%s/G2_param_%s_%s.pdf",dir_name, name, iterations))
  }, G2_param_res, names(G2_param_res))
  
  
  mice_G2 <- mice_run(missing_MAR_list,G2_param_res, pred = prediction, method = "xgb", m , maxit)
  
  mice_G2_res <- map(mice_G2, ~ map(., "results"))                 # mice results
  
  eval_G2 <- eval_func(mice_G2_res, coef_true, Num_ds)
  
  #####################################################################################
  ###########                       G3
  #####################################################################################
  
  G3_param_out <- param_cal_fixed(missing_MAR_list, G2_param_res, response_var = "all", features = NULL, 
                                  iters = iterations, tuneParam = c("alpha","lambda", "gamma"))
  
  G3_param_res <- map(G3_param_out, ~ map(., "params"))  # imputation results
  G3_param_time <- map(G3_param_out, ~ map(., "time_taken"))           # Time taken
  
  
  Map(function(x, name) {
    extract_plots(x, sprintf("%s/G3_param_%s_%s.pdf",dir_name, name, iterations))
  }, G3_param_res, names(G3_param_res))
  
  
  mice_G3 <- mice_run(missing_MAR_list,G3_param_res, pred = prediction, method = "xgb" , m , maxit)
  
  mice_G3_res <- map(mice_G3, ~ map(., "results"))                 # mice results
  
  eval_G3 <- eval_func(mice_G3_res, coef_true, Num_ds)
  
  ####################            G4              #################################
  
  remove_fixed <- function(x) {
    if (is.list(x)) {
      x <- lapply(x, remove_fixed)  # Apply recursively to each element in the list
      if (!is.null(names(x))) {
        x <- x[setdiff(names(x), c("eta" , "nrounds"))]  # Remove 'eta' if it exists in a named list
      }
    }
    return(x)
  }
  
  Param_G3_noEta <- remove_fixed(G3_param_res)
  
  G4_param_out <- param_cal_fixed(missing_MAR_list, Param_G3_noEta, response_var = "all", features = NULL, 
                                  iters = iterations, tuneParam = c("eta", "nrounds"))
  
  G4_param_res <- map(G4_param_out, ~ map(., "params"))  # imputation results
  
  G4_param_time <- map(G4_param_out, ~ map(., "time_taken"))           # Time taken
  
  Map(function(x, name) {
    extract_plots(x, sprintf("%s/G4_param_%s_%s.pdf",dir_name, name, iterations))
  }, G4_param_res, names(G4_param_res))
  
  
  mice_G4 <- mice_run(missing_MAR_list,G4_param_res, pred = prediction, method = "xgb", m , maxit)
  
  mice_G4_res <- map(mice_G4, ~ map(., "results"))  # imputation results
  
  eval_G4 <- eval_func(mice_G4_res, coef_true, Num_ds)
  
  #####################################################################################
  ###########                       G1 and G2 - Together
  #####################################################################################
  
  G1_G2_param_out <-  param_cal_run(missing_MAR_list, response_var = "all", features=NULL, iters = iterations, 
                                    fixedPAram = NULL, tuneParam = c("eta", "nrounds", "max_depth","min_child_weight", "subsample"))
  
  G1_G2_param_res <- map(G1_G2_param_out, ~ map(., "params"))  # imputation results
  G1_G2_param_time <- map(G1_G2_param_out, ~ map(., "time_taken"))           # Time taken
  
  Map(function(x, name) {
    extract_plots(x, sprintf("%s/G1_G2_param_%s_%s.pdf", dir_name, name, iterations))
  }, G1_G2_param_res, names(G1_G2_param_res))
  
  
  mice_G1_G2 <- mice_run(missing_MAR_list,G1_G2_param_res, pred = prediction, method = "xgb", m , maxit)
  
  mice_G1_G2_res <- map(mice_G1_G2, ~ map(., "results"))  # imputation results
  
  eval_G1_G2_together <- eval_func(mice_G1_G2_res, coef_true, Num_ds)
  
  
  #####################################################################################
  ###########                       G1 and G3 - Together
  #####################################################################################
  
  G1_G3_param_out <- param_cal_run(missing_MAR_list, response_var = "all", features=NULL, iters = iterations, 
                                   fixedPAram = NULL, tuneParam = c("eta", "nrounds","alpha","lambda", "gamma"))
  G1_G3_param_res <- map(G1_G3_param_out, ~ map(., "params"))  # imputation results
  G1_G3_param_time <- map(G1_G3_param_out, ~ map(., "time_taken"))           # Time taken
  
  
  Map(function(x, name) {
    extract_plots(x, sprintf("%s/G1_G3_param_%s_%s.pdf", dir_name, name, iterations))
  }, G1_G3_param_res, names(G1_G3_param_res))
  
  
  mice_G1_G3 <- mice_run(missing_MAR_list,G1_G3_param_res, pred = prediction, method = "xgb", m , maxit)
  
  mice_G1_G3_res <- map(mice_G1_G3, ~ map(., "results"))  # imputation results
  
  eval_G1_G3_together <- eval_func(mice_G1_G3_res, coef_true, Num_ds)
  
  ########################################################################################################
  #####################            Tuning All parameters together               ##########################
  ########################################################################################################
  
  all_param_out <- param_cal_run(missing_MAR_list, response_var = "all", features=NULL, iters = iterations, 
                                 fixedPAram = NULL, tuneParam = c("eta", "nrounds", "max_depth", "min_child_weight","subsample", "alpha", "lambda", "gamma"))
  all_param_res <- map(all_param_out, ~ map(., "params"))  # imputation results
  all_param_time <- map(all_param_out, ~ map(., "time_taken"))           # Time taken
  
  Map(function(x, name) {
    extract_plots(x, sprintf("%s/All_param_%s_%s.pdf",dir_name, name, iterations))
  }, all_param_res, names(all_param_res))
  
  mice_all <- mice_run(missing_MAR_list,all_param_res, pred = prediction, method = "xgb", m, maxit)
  
  mice_all_res <- map(mice_all, ~ map(., "results"))  # imputation results
  
  eval_all_together <- eval_func(mice_all_res, coef_true, Num_ds)
  
  
  ########################################################################################################
  #####################           Combining all results             ##########################
  ########################################################################################################
  
  
  eval_combined <- list("G1" = eval_G1,"G2" = eval_G2, "G3" = eval_G3,"G4"= eval_G4, "G1_G2_together" = eval_G1_G2_together, "G1_G3_together" = eval_G1_G3_together, "all_together" = eval_all_together)
  param_time <- list("G1" = G1_param_time,"G2" = G2_param_time, "G3" = G3_param_time,"G4"= G4_param_time, "G1_G2_together" = G1_G2_param_time, "G1_G3_together" = G1_G3_param_time, "all_together" = all_param_time)
  
  combined_df <- map_dfr(names(eval_combined), function(method) {
    map_dfr(names(eval_combined[[method]]), function(percentage) {
      eval_combined[[method]][[percentage]] %>%
        rownames_to_column(var = "term") %>%
        mutate(method = method, percentage = percentage)
    })
  })
  
  
  
  combined_time <- map_dfr(names(param_time), function(method) {
    map_dfr(names(param_time[[method]]), function(percentage) {
      # Convert list of named vectors to data frame and add columns
      do.call(rbind, lapply(param_time[[method]][[percentage]], as.data.frame)) %>%
        mutate(method = method, percentage = percentage)
    })
  })
  
  
  write.csv(combined_df,sprintf("%s/ParameterTuning_iter%s.csv", dir_name, iterations))
  save(combined_df,file = sprintf("%s/ParameterTuning_iter%s.RData",dir_name, iterations))
  
  
  write.csv(combined_time,sprintf("%s/time_ParameterTuning_iter%s.csv", dir_name, iterations))
  save(combined_time,file = sprintf("%s/time_ParameterTuning_iter%s.RData",dir_name, iterations))
  
}



