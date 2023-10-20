library(tidymodels)
library(magrittr)
library(tidyverse)
library(furrr)

source("dgp.R")
source("discreteSL.R")
source("estimators.R")
source("experiment_asym.R")

res <- function(results, truth =  -0.8) {
  results %>%
    summarize(
      #emp_se = sd(effect),
      #mean_est_se = mean(se),
      #est_eff = mean(effect),
      bias_effect = mean(effect) - truth,
      var_effect = var(effect),
      bias_est_se = mean(se) - sd(effect),
      var_est_se = var(se),
      rmse = sqrt(mean((effect - truth) ^ 2)),
      power = mean(2*(1 - pnorm(abs((effect) / se))) < 0.05),
      coverage = mean(((-1.96 * se + effect) < truth) &
                        (truth < (1.96 * se + effect)))
      
    )
}

plan(multisession, workers=59)

# run experiment ----------------------------------------------------------

params = expand_grid(
  n_hist=c(100^2),
  n_trial=c(100),
  p=c(20),
  y_noise=c(2),
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous")
)
results100 = 1:1000 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                      .options = furrr_options(seed = 3021377))

saveRDS(results100, file = "../results/sp1_asym_100_1000runs.RDS")

params = expand_grid(
  n_hist=c(200^2),
  n_trial=c(200),
  p=c(20),
  y_noise=c(2),
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous")
)
results200 = 1:1000 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                      .options = furrr_options(seed = 3021377))

saveRDS(results200, file = "../results/sp1_asym_200_1000runs.RDS")

params = expand_grid(
  n_hist=c(300^2), 
  n_trial=c(300), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous") 
)
results300 = 1:1000 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                       .options = furrr_options(seed = 3021377))

saveRDS(results300, file = "../results/sp1_asym_300_1000runs.RDS")

params = expand_grid(
  n_hist=c(400^2), 
  n_trial=c(400), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous") 
)
results400 = 1:1000 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                       .options = furrr_options(seed = 3021377))

saveRDS(results400, file = "../results/sp1_asym_400_1000runs.RDS")

params = expand_grid(
  n_hist=c(500^2), 
  n_trial=c(500), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous") 
)
results500 = 1:1000 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                       .options = furrr_options(seed = 3021377))

saveRDS(results500, file = "../results/sp1_asym_500_1000runs.RDS")


# examine result ----------------------------------------------------------

results500 %>% group_by(estr, prog, n_trial, n_hist) %>% res()
