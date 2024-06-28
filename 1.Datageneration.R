rm(list = ls())

library(mice)
library(dplyr)
library(tidyverse)
library(future)
library(furrr)

seed <- 123
Num_ds <- 100
prop_values <- c(20, 40, 60, 80)
m <- 5
maxit <- 5

set.seed(seed) 

available_cores <- availableCores() - 1
plan(multisession, workers = available_cores)

dir <- "Users/Shehw001/Documents/GitHub/mice_xgboost_simulation/Simulation_SampleEstimates"
setwd(dir)

#################################################################################
########################     Step 1: Data generation    #########################
#################################################################################

sigma <- matrix(data = c(1, 0.7, 0.7, 1),     # covariance matrix
                ncol = 2)

#Let's generate 1000 data sets with 1000 entries each. Each data set is saved as a separate element in the list.

simdata <- replicate(n = Num_ds, 
                     expr = mvtnorm::rmvnorm(n = 1000, 
                                             mean = c(4, 1), 
                                             sigma = sigma) %>% 
                       as_tibble() %>% # make into a tibble
                       rename(x = V1, z = V2) %>% # rename columns
                       mutate(y = as.numeric(3 * x +  z + 3*I(x*z) + z^2 + rnorm(1000, sd = 20))), # add y # mean 0 std of 5 or 10
                     simplify = FALSE) # keep as list of generated sets

save(simdata, file = "Data/simdata.RData") # imputation result



#Run model on entire dataset

desired_order <- c("(Intercept)", "x", "z", "x:z", "I(z^2)")

coef_true <- simdata %>% 
  map_dfr(~.x %$% # for every simulated set in simdata....
            lm(y ~ x + z + x:z + I(z^2)) %>% # fit model
            coefficients) %>% # extract coefficients) %>%
   t() %>% # all variables as row and each dataset as column
  as.data.frame() %>% # convert to dataframe
  rownames_to_column(var = "term") %>% # convert variable coefficients to column "term"
  mutate(term = factor(term, levels = desired_order)) %>% #rearrange variable coefficients in the order described in desired_order
  arrange(term) # ensure term is sorted according to desired order

save(coef_true, file = "Data/True_coefEstimated.RData")


#################################################################################
#############          2. Missing data 
#################################################################################

# make data missing. For each missingness percentage, generate 100 data sets with given percentage of missing data. 


apply_ampute <- function(simdata, prop_value) {
  simdata %>%
    furrr::future_map(function(x) {
      x %>%
        ampute(prop = prop_value / 100, mech = "MAR", type = "RIGHT") %>%
        .$amp 
    }, .options = furrr_options(seed = TRUE))
}

missing_MAR_list <- map(prop_values, ~ apply_ampute(simdata, .x))
names(missing_MAR_list) <- prop_values

# missing_MAR_list is a list of missingness percentage with a sub-list of 1000 datasets

NAs_in_data <- map(missing_MAR_list, ~ map_dbl(.x, ~ sum(is.na(.x))))

save(missing_MAR_list, file = "Data/missing_MAR_list.RData") # imputation time

