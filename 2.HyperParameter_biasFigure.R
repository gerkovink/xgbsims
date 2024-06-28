load("ParameterOptimization/ParameterTuning_iter50.RData")
iter50 <- combined_df
#iter50$iteration <- 50

load("ParameterOptimization/Mice_XGB_default.RData")
write.csv(xgb_ParamDefault_maxit5_P_eval, "ParameterOptimization/Mice_XGB_default.csv")

df <- do.call(rbind, lapply(names(xgb_ParamDefault_maxit5_P_eval), function(name) {
              method_df <- xgb_ParamDefault_maxit5_P_eval[[name]]
              method_df$method <- "Default_param"
              method_df$percentage <- name
              method_df <- rownames_to_column(method_df, "term")  # Convert row names to column
  return(method_df)
}))


iter50 <- rbind(iter50, df)


desired_order <- c("Default_param","G1", "G2", "G3", "G4", "G1_G2_together", "G1_G3_together", "all_together")
iter50$method <- factor(iter50$method, levels = desired_order)
iter50$lower <- iter50$bias - (iter50$width/2)
iter50$upper <- iter50$bias + (iter50$width/2)

iter50_20 <- iter50[iter50$percentage=="20",]
iter50_60 <- iter50[iter50$percentage=="60",]


pdf("ParameterOptimization/BiasPlotOfDifferentMethods.pdf", width = 15)

ggplot(iter50_20, aes(x = bias, y = method, xmin = lower, xmax = upper, color = method)) +
  geom_pointrange() +  # Add point ranges for confidence intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  facet_wrap(~ term, ncol = 3, scales = "free") +  # Create separate plots for each term, arranged in 2 columns
  labs(
    title = "20% missingness",
    x = "Bias",
    y = "Parameter tuning groups",
    color = "Method"
  ) +
  theme_bw() +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    strip.text = element_text(size = 12, face = "bold")  # Increase font size of facet titles
  )

ggplot(iter50_60, aes(x = bias, y = method, xmin = lower, xmax = upper, color = method)) +
  geom_pointrange() +  # Add point ranges for confidence intervals
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add vertical line at x = 0
  facet_wrap(~ term, ncol = 3, scales = "free") +  # Create separate plots for each term, arranged in 2 columns
  labs(
    title = "60% missingness",
    x = "Bias",
    y = "Method",
    color = "Method"
  ) +
  theme_bw() +  # Use a clean, white background theme
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
        strip.text = element_text(size = 12, face = "bold")  # Increase font size of facet titles
        )

dev.off()





