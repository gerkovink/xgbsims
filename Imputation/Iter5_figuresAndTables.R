rm(list = ls())

library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)


dir <- "/Users/Shehw001/Documents/GitHub/mice_xgboost_simulation/Simulation_SampleEstimates/Imputation/"
setwd(dir)

results_dir <- "FiguresAndTables"

source("functions_imp.R")

N = 15

###############################################################################################################
####################                Data file names                   ########################################
###############################################################################################################

eval_files <- list(
  "MiceOtherMethods/Default_evaluation.RData","MiceOtherMethods/CART_evaluation.RData", "MiceOtherMethods/RF_evaluation_nonlinear.RData",
  "mice_xgb_default/iter5/xgb_ParamDefault_maxit5_P_eval.RData", "mice_xgb_default/iter5/xgb_ParamDefault_maxit5_PO_eval.RData", "mice_xgb_default/iter5/xgb_ParamDefault_maxit5_OO_eval.RData",
  "Mice_xgb_random/iter5/Mice_xgb_predicted_Random_eval.RData", "Mice_xgb_random/iter5/Mice_xgb_predicted_observed_Random_eval.RData", "Mice_xgb_random/iter5/Mice_xgb_original_observed_Random_eval.RData",
  "Mice_xgb_All/iter5/Mice_xgb_predicted_All_eval.RData", "Mice_xgb_All/iter5/Mice_xgb_predicted_observed_All_eval.RData", "Mice_xgb_All/iter5/Mice_xgb_original_observed_All_eval.RData",
 "Mixgb/iter5/mixgb_maxit5_pmmNULL_eval.RData","Mixgb/iter5/mixgb_maxit5_pmm1_eval.RData", "Mixgb/iter5/mixgb_maxit5_pmm2_eval.RData"
)


# Load all RData files into the environment

for(i in 1:15){
  load(eval_files[[i]])
}




###############################################################################################################
####################      Data loading and renaming                   ########################################
###############################################################################################################

eval_default <- data_transform(eval_default, p="PMM")
eval_CART <- data_transform(eval_CART, p="CART")
eval_RF <- data_transform(eval_RF,p = "RF")

xgb_ParamDefault_maxit5_P_eval <- data_transform(xgb_ParamDefault_maxit5_P_eval, p="ParamDefault_predicted_maxit5")
xgb_ParamDefault_maxit5_PO_eval <- data_transform(xgb_ParamDefault_maxit5_PO_eval, p="ParamDefault_predicted.observed_maxit5")
xgb_ParamDefault_maxit5_OO_eval <- data_transform(xgb_ParamDefault_maxit5_OO_eval, p="ParamDefault_original_observed_maxit5")

eval_random_predicted_maxit5_res <- data_transform(eval_random_predicted_maxit5_res, p="Random_predicted_maxit5")
eval_random_predicted_observed_maxit5 <- data_transform(eval_random_predicted_observed_maxit5, p="Random_predicted.observed_maxit5")
eval_random_original_observed_maxit5 <- data_transform(eval_random_original_observed_maxit5, p="Random_original.observed_maxit5")

eval_allParam_predicted_maxit5 <- data_transform(eval_allParam_predicted_maxit5, p="AllParam_predicted_maxit5")
eval_allParam_predicted_observed_maxit5 <- data_transform(eval_allParam_predicted_observed_maxit5, p="AllParam_predicted.observed_maxit5")
eval_allParam_original_observed_maxit5 <- data_transform(eval_allParam_original_observed_maxit5, p="AllParam_original.observed_maxit5")

mixgb_maxit5_pmmNULL_eval <- data_transform(mixgb_maxit5_pmmNULL_eval, p="Mixgb - Pmm.Null")
mixgb_maxit5_pmm1_eval <- data_transform(mixgb_maxit5_pmm1_eval, p="Mixgb - Pmm.1")
mixgb_maxit5_pmm2_eval <- data_transform(mixgb_maxit5_pmm2_eval,p = "Mixgb - Pmm.2")

all_eval <- bind_rows(
  eval_default, eval_CART, eval_RF,
  xgb_ParamDefault_maxit5_P_eval,xgb_ParamDefault_maxit5_PO_eval,xgb_ParamDefault_maxit5_OO_eval,
  eval_random_predicted_maxit5_res, eval_random_predicted_observed_maxit5, eval_random_original_observed_maxit5,
  eval_allParam_predicted_maxit5, eval_allParam_predicted_observed_maxit5, eval_allParam_original_observed_maxit5,
  mixgb_maxit5_pmmNULL_eval,mixgb_maxit5_pmm1_eval,mixgb_maxit5_pmm2_eval
)


desired_level <- c("PMM", "CART", "RF" , "ParamDefault_predicted" , "ParamDefault_predicted.observed" , "ParamDefault_original_observed",
                  "Random_predicted", "Random_predicted.observed" ,"Random_original.observed",  
                  "AllParam_predicted", "AllParam_predicted.observed" , "AllParam_original.observed" , 
                  "Mixgb - Pmm.Null" , "Mixgb - Pmm.1" , "Mixgb - Pmm.2")


all_eval$Method <- gsub("_maxit5", "", all_eval$Method)

all_eval$lower <- all_eval$bias - all_eval$width/2
all_eval$upper <- all_eval$bias + all_eval$width/2


###############################################################################################################

all_eval_20 <- all_eval[all_eval$name==20,]
all_eval_20$Method <- factor(all_eval_20$Method, levels = desired_level)
pp <- plot_percent(all_eval_20, "20% missingness")
pp_c <- plot_cov_2(all_eval_20, "20% missingness")
ggsave(sprintf("%s/Bias_20p_iter5.pdf",results_dir), plot = pp, width = 12, height = 8)
ggsave(sprintf("%s/Coverage_20p_iter5.pdf",results_dir), plot = pp_c, width = 12, height = 8)



all_eval_60 <- all_eval[all_eval$name==60,]
all_eval_60$Method <- factor(all_eval_60$Method, levels = desired_level)
pp60 <- plot_percent(all_eval_60, "60% missingness")
pp60_c <- plot_cov_2(all_eval_60, "60% missingness")
ggsave(sprintf("%s/Bias_60p_iter5.pdf",results_dir), plot = pp60, width = 12, height = 8)
ggsave(sprintf("%s/Coverage_60p_iter5.pdf",results_dir), plot = pp60_c, width = 12, height = 8)


all_eval_40 <- all_eval[all_eval$name==40,]
all_eval_40$Method <- factor(all_eval_40$Method, levels = desired_level)
pp40 <- plot_percent(all_eval_40, "40% missingness")
pp40_c <- plot_cov_2(all_eval_40, "40% missingness")
ggsave(sprintf("%s/Bias_40p_iter5.pdf",results_dir), plot = pp40, width = 12, height = 8)
ggsave(sprintf("%s/Coverage_40p_iter5.pdf",results_dir), plot = pp40_c, width = 12, height = 8)


all_eval_80 <- all_eval[all_eval$name==80,]
all_eval_80$Method <- factor(all_eval_80$Method, levels = desired_level)
pp80 <- plot_percent(all_eval_80, "80% missingness")
pp80_c <- plot_cov_2(all_eval_80, "80% missingness")
ggsave(sprintf("%s/Bias_80p_iter5.pdf",results_dir), plot = pp80, width = 12, height = 8)
ggsave(sprintf("%s/Coverage_80p_iter5.pdf", results_dir), plot = pp80_c, width = 12, height = 8)


all_eval_20 <- all_eval_20 %>%
  arrange(term)

all_eval_60 <- all_eval_60 %>%
  arrange(term)


all_eval_40 <- all_eval_40 %>%
  arrange(term)

all_eval_80 <- all_eval_80 %>%
  arrange(term)


write.table(all_eval_20, sprintf("%s/Paper_Imputation_20p_iter5.txt",results_dir), sep = "\t")
save(all_eval_20, file = sprintf("%s/Paper_Imputation_20p_iter5.RData", results_dir))

write.table(all_eval_60, sprintf("%s/Paper_Imputation_60p_iter5.txt",results_dir), sep = "\t")
save(all_eval_60, file = sprintf("%s/Paper_Imputation_60p_iter5.RData",results_dir))

write.table(all_eval_40, sprintf("%s/Paper_Imputation_40p_iter5.txt",results_dir), sep = "\t")
save(all_eval_40, file = sprintf("%s/Paper_Imputation_40p_iter5.RData",results_dir))

write.table(all_eval_80, sprintf("%s/Paper_Imputation_80p_iter5.txt",results_dir), sep = "\t")
save(all_eval_80, file = sprintf("%s/Paper_Imputation_80p_iter5.RData",results_dir))

