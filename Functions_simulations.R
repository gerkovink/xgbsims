

#################################################################################################
#     Call - Finding parameters without any fixed parameters
#################################################################################################

param_cal_run <- function(missing_MAR_list, response_var, features, iters, fixedPAram, tuneParam, plots){
  
  Param_out <- missing_MAR_list %>%
    map(function(mat_list) { 
      furrr::future_map(mat_list, function(mat) {
        result <- system.time({
          params <- xgb_param_calc(mat,response = response_var, select_features=features, 
                                   iter = iters, fixed_params =fixedPAram,  
                                   Params_to_tune = tuneParam, plots=plots)
        })
        list(params = params, time_taken = result)
      }, .options = furrr_options(seed = TRUE,future.globals.maxSize = 1000 * 1024^2))
    })
  
  return(Param_out)
}

#################################################################################################
#     Call - Finding parameters with a set of fixed parameters
#################################################################################################


param_cal_fixed <- function(missing_MAR_list, Fixed_param_list, response_var = "all", features = NULL, iters = 50, tuneParam = c("max_depth", "min_child_weight", "subsample"),plots){
  
  Param_out <- map2(missing_MAR_list, Fixed_param_list, 
                           .f = function(data_inner, params_inner) {
                             future_map2(data_inner, params_inner, 
                                         .f = function(data_single, params_single) {
                                           result <- system.time({
                                             params <- xgb_param_calc(data_single, response = response_var , select_features=NULL, 
                                                                      iter = iters, fixed_params =params_single,  
                                                                      Params_to_tune = tuneParam, plots=plots)
                                           })
                                           list(params = params, time_taken = result)
                                         }, .options = furrr_options(seed = TRUE))
                           })
  
  return(Param_out)
}

#################################################################################################
#     Call - mice 


mice_run <- function(missing_MAR_list,G1_param_res, pred = "predicted.observed", method = "xgb", m , maxit){
  
  mice_res <- map2(missing_MAR_list, G1_param_res, 
                           .f = function(data_inner, params_inner) {
                             future_map2(data_inner, params_inner, 
                                         .f = function(data_single, params_single) {
                                           timetaken <- system.time({
                                           mice_result <- mice(data_single, m = m, method = method,
                                                               maxit = maxit,xgb.params =  params_single,
                                                               match.type = pred, print = FALSE)
                                           })
                                           list(results = mice_result, time_taken = timetaken)
                                          }, .options = furrr_options(seed = TRUE))
                           })
  
  return(mice_res)
}



mice_noparam <- function(missing_MAR_list,method, m , maxit){
  
  mice_res <- map(missing_MAR_list, 
                   .f = function(data_inner) {
                     future_map(data_inner, 
                                 .f = function(data_single) {
                                   timetaken <- system.time({
                                     mice_result <- mice(data_single, m = m, method = method, 
                                                         maxit = maxit, print = FALSE)
                                   })
                                   list(results = mice_result, time_taken = timetaken)
                                 }, .options = furrr_options(seed = TRUE))
                   })
  
  return(mice_res)
}


#################################################################################################

eval_func <- function(mice_res, coef_true, Num_ds){
  
  eval_res <- mice_res %>% 
    map(function(mat_list) { # For each percentage
      furrr::future_map(seq_along(mat_list), ~{ # Loop over indices
        mat <- mat_list[[.x]] # Extract the matrix using the index
        complete(mat, "all") %>% # Create a list of completed data sets
          map(~.x %$% lm(y ~ x + z + x:z + I(z^2))) %>% # Fit model for each completed data set
          pool() %>%  # Pool coefficients
          summary(conf.int = TRUE) %>% # Summary of coefficients
          left_join(coef_true %>%
                      select(term, !!as.name(paste0("V", .x))) %>%
                      rename(true_vals = !!as.name(paste0("V", .x))), by = "term") %>%
          mutate( cov = conf.low < true_vals & true_vals < conf.high, # coverage
                  bias = estimate - true_vals,
                  width = conf.high - conf.low) %>% # bias
          column_to_rownames("term")}, 
        .options = furrr_options(seed = TRUE)) %>% # `term` as rownames
        Reduce("+", .) / Num_ds })
  
  return(eval_res)
}



#################################################################################################

extract_plots <- function(obj,file) {
  
  p = list()
  q = list()
  
  pdf(file, width = 10, height = 13)
  for (i in seq_along(obj)) {
    # Extract convergence plots for each sublist
    p <- lapply(obj[[i]], function(x) x$fig$Convergence_plot)
    q <- lapply(obj[[i]], function(x) x$fig$Hyperparameter_trace_plot)

    # Ensure that we only process non-NULL plots
    p <- p[!sapply(p, is.null)]
    q <- q[!sapply(q, is.null)]

    
    combined_plots <- vector("list", length(p) * 2)
    
    combined_plots[seq(1, length(p) * 2, by = 2)] <- p
    combined_plots[seq(2, length(p) * 2, by = 2)] <- q
    
    # Ensure that combined_plots is a flat list
    # combined_plots <- unlist(combined_plots, recursive = FALSE)
    
    # Arrange the plots in a grid and print to a new page in the PDF
    if (length(p) > 0) {
      print(plot_grid(plotlist = combined_plots, ncol = 2))
    }
    
  }
  dev.off()
}

#################################################################################################

extract_plots_pairwise <- function(obj,file) {
  
  r = list()
  
  pdf(file, width = 13, height = 10)
  for (i in seq_along(obj)) {
    # Extract convergence plots for each sublist
    r <- lapply(obj[[i]], function(x) x$fig$Pairwise_parameter_plot)
    
    r <- r[!sapply(r, is.null)]
    
    # Flatten the list of lists to a single list, ignoring NULLs
    r <- do.call(c, r)

    # Arrange the plots in a grid and print to a new page in the PDF
   r[[1]]
   r[[2]]
   r[[3]]
   
    
  }
  dev.off()
}

#################################################################################################














