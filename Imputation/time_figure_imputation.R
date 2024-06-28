rm(list = ls())

library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(purrr)
library(plyr)
library(tidyr)

dir <- "/Users/Shehw001/Documents/GitHub/mice_xgboost_simulation/Simulation_SampleEstimates/Imputation/"
setwd(dir)

results_dir <- "FiguresAndTables"

source("functions_imp.R")

N = 15

###############################################################################################################
####################                Data file names                   ########################################
###############################################################################################################

eval_files <- list(
  "MiceOtherMethods/Default_time.RData","MiceOtherMethods/CART_time.RData", "MiceOtherMethods/RF_time_nonlinear.RData",
  "mice_xgb_default/iter5/xgb_ParamDefault_maxit5_P_imp_time.RData", "mice_xgb_default/iter5/xgb_ParamDefault_maxit5_PO_imp_time.RData", "mice_xgb_default/iter5/xgb_ParamDefault_maxit5_OO_imp_time.RData",
  "Mice_xgb_random/iter5/time_Mice_xgb_predicted_Random.RData", "Mice_xgb_random/iter5/time_Mice_xgb_predicted_observed_Random.RData", "Mice_xgb_random/iter5/time_Mice_xgb_original_observed_Random.RData",
  "Mice_xgb_All/iter5/time_Mice_xgb_predicted_All.RData", "Mice_xgb_All/iter5/time_Mice_xgb_predicted_observed_All.RData", "Mice_xgb_All/iter5/time_Mice_xgb_original_observed_All.RData",
  "Mixgb/iter5/mixgb_maxit5_pmmNULL_time.RData","Mixgb/iter5/mixgb_maxit5_pmm1_time.RData", "Mixgb/iter5/mixgb_maxit5_pmm2_time.RData"
)



for(i in 1:15){
  load(eval_files[[i]])
}


time_default <- time_arrange(time_default, method="PMM")
time_CART <- time_arrange(time_CART, method="CART")
time_RF <- time_arrange(time_RF,method = "RF")

xgb_ParamDefault_maxit5_P_imp_time <- time_arrange(xgb_ParamDefault_maxit5_P_imp_time, method="ParamDefault_predicted_maxit5")
xgb_ParamDefault_maxit5_PO_imp_time <- time_arrange(xgb_ParamDefault_maxit5_PO_imp_time, method="ParamDefault_predicted.observed_maxit5")
xgb_ParamDefault_maxit5_OO_imp_time <- time_arrange(xgb_ParamDefault_maxit5_OO_imp_time, method="ParamDefault_original_observed_maxit5")

mice_random_predicted_maxit5_time <- time_arrange(mice_random_predicted_maxit5_time, method="Random_predicted_maxit5")
mice_random_predicted_observed_maxit5_time <- time_arrange(mice_random_predicted_observed_maxit5_time,  method="Random_predicted.observed_maxit5")
mice_random_original_observed_maxit5_time <- time_arrange(mice_random_original_observed_maxit5_time, method="Random_original.observed_maxit5")

mice_allParam_predicted_maxit5_time <- time_arrange(mice_allParam_predicted_maxit5_time, method="AllParam_predicted_maxit5")
mice_allParam_predicted_observed_maxit5_time <- time_arrange(mice_allParam_predicted_observed_maxit5_time, method="AllParam_predicted.observed_maxit5")
mice_allParam_original_observed_maxit5_time <- time_arrange(mice_allParam_original_observed_maxit5_time, method="AllParam_original.observed_maxit5")

mixgb_maxit5_pmmNULL_time <- time_arrange(mixgb_maxit5_pmmNULL_time, method="Mixgb - Pmm.Null")
mixgb_maxit5_pmm1_time <- time_arrange(mixgb_maxit5_pmm1_time, method="Mixgb - Pmm.1")
mixgb_maxit5_pmm2_time <- time_arrange(mixgb_maxit5_pmm2_time,method = "Mixgb - Pmm.2")

###################################################################################################################

param_tune <- c("Mice_xgb_All/G1_parameters_time.RData", "Mice_xgb_All/G2_parameters_time.RData")

for(i in 1:length(param_tune)){
  load(param_tune[[i]])
}

time_G1 <- time_arrange(G1_param_time, method="G1")
time_G2 <- time_arrange(G2_param_time, method="G2")

time_combined <- time_G1$Timetaken + time_G2$Timetaken

mice_allParam_predicted_maxit5_time$Timetaken <- mice_allParam_predicted_maxit5_time$Timetaken + time_combined
mice_allParam_predicted_observed_maxit5_time$Timetaken <- mice_allParam_predicted_observed_maxit5_time$Timetaken + time_combined
mice_allParam_original_observed_maxit5_time$Timetaken <- mice_allParam_original_observed_maxit5_time$Timetaken + time_combined

###################################################################################################################

rm(G1_param_time,G2_param_time)
param_tune2 <- c("Mice_xgb_Random/G1_parameters_time.RData", "Mice_xgb_Random/G2_parameters_time.RData")

for(i in 1:length(param_tune)){
  load(param_tune2[[i]])
}

time_G1_random <- time_arrange(G1_param_time, method="G1")
time_G2_random <- time_arrange(G2_param_time, method="G2")

time_combined_random <- time_G1_random$Timetaken + time_G2_random$Timetaken

mice_random_predicted_maxit5_time$Timetaken <- mice_random_predicted_maxit5_time$Timetaken + time_combined_random
mice_random_predicted_observed_maxit5_time$Timetaken <- mice_random_predicted_observed_maxit5_time$Timetaken + time_combined_random
mice_random_original_observed_maxit5_time$Timetaken <- mice_random_original_observed_maxit5_time$Timetaken + time_combined_random

###################################################################################################################

combined <- bind_rows(
  time_default, time_CART,  time_RF,
  xgb_ParamDefault_maxit5_P_imp_time, xgb_ParamDefault_maxit5_PO_imp_time, xgb_ParamDefault_maxit5_OO_imp_time,
  mice_random_predicted_maxit5_time, mice_random_predicted_observed_maxit5_time, mice_random_original_observed_maxit5_time,
  mice_allParam_predicted_maxit5_time, mice_allParam_predicted_observed_maxit5_time, mice_allParam_original_observed_maxit5_time,
  mixgb_maxit5_pmmNULL_time, mixgb_maxit5_pmm1_time, mixgb_maxit5_pmm2_time
)


combined$method <- gsub("_maxit5", "", combined$method)


desired_level <- c("PMM", "CART", "RF" , "ParamDefault_predicted" , "ParamDefault_predicted.observed" , "ParamDefault_original_observed",
                   "Random_predicted", "Random_predicted.observed" ,"Random_original.observed",  
                   "AllParam_predicted", "AllParam_predicted.observed" , "AllParam_original.observed" , 
                   "Mixgb - Pmm.Null" , "Mixgb - Pmm.1" , "Mixgb - Pmm.2")



combined$method <- factor(combined$method, levels = desired_level)
write.table(combined, "FiguresAndTables/Imputation_time_paramtimeAdded.txt", sep = "\t", quote = FALSE)


pdf("FiguresAndTables/Imputation_time.pdf", width = 10)
plot <- ggplot(combined, aes(x = method, y = log(Timetaken, 2), fill = as.factor(MissingPercent), group = interaction(method, MissingPercent))) +
  geom_boxplot(position = "dodge", outlier.size = 0.5, size = 0.2) +  # Set boxplot border size and color
  labs(
    x = "Imputation methods",
    y = "log (Mean Time in seconds)", 
    fill = "Percentage of \n missing data"
  ) +
  scale_color_discrete(guide = FALSE) +  # Hide legend for color
  theme_bw() +  # Use a black and white theme
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),  # Larger x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Larger y-axis title
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Larger font for x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")  # Increase legend title size and make it bold
  )
plot
dev.off()


mean_df <- combined %>%
  dplyr::group_by(MissingPercent, method) %>%
  dplyr::summarize(mean_Timetaken = mean(Timetaken))

pivot_df <- mean_df %>%
  pivot_wider(names_from = MissingPercent, values_from = mean_Timetaken)


write.table(pivot_df, "FiguresAndTables/Imputation_Meantime_paramtimeAdded.txt", sep = "\t", quote = FALSE)



elapsed_rows <- grep("elapsed", rownames(combined_time), value = TRUE)
combined_time_reformatted <- combined_time[elapsed_rows, ]
colnames(combined_time_reformatted) <- c("elapsed", "method", "percentage")


df <- map_dfr(names(xgb_ParamDefault_maxit5_P_imp_time), ~ {
  percentage <- .x
  bind_rows(xgb_ParamDefault_maxit5_P_imp_time[[.x]]) %>%
    mutate(method = "Default_param", percentage = percentage)
})

df <- df[,c("elapsed", "method", "percentage")]

combined_all <- rbind(combined_time_reformatted, df)

combined_all[(combined_all$method == "G2"),"elapsed"] <- combined_all[(combined_all$method == "G2"),"elapsed"] + combined_all[(combined_all$method == "G1"),"elapsed"]
combined_all[(combined_all$method == "G3"),"elapsed"] <- combined_all[(combined_all$method == "G3"),"elapsed"] + combined_all[(combined_all$method == "G2"),"elapsed"]
combined_all[(combined_all$method == "G4"),"elapsed"] <- combined_all[(combined_all$method == "G4"),"elapsed"] + combined_all[(combined_all$method == "G3"),"elapsed"]


desiredOrder <- c("Default_param", "G1", "G2","G3", "G4", "G1_G2_together", "G1_G3_together", "all_together")
combined_all$method <- factor(combined_all$method, levels = desiredOrder)


pdf("ParameterOptimization/Hyperparameter_tuning_time.pdf", width = 10)
plot <- ggplot(combined_all, aes(x = method, y = log(elapsed, 2), fill = as.factor(percentage), group = interaction(method, percentage))) +
  geom_boxplot(position = "dodge", outlier.size = 0.5, size = 0.2) +  # Set boxplot border size and color
  labs(
    x = "Hyperparameter groups",
    y = "log (Mean Time in seconds)", 
    fill = "Percentage of \n missing data"
  ) +
  scale_color_discrete(guide = FALSE) +  # Hide legend for color
  theme_bw() +  # Use a black and white theme
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),  # Larger x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Larger y-axis title
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Larger font for x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")  # Increase legend title size and make it bold
  )
plot
dev.off()
