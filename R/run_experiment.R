library(tidymodels)
library(magrittr)
library(tidyverse)
library(furrr)

source("dgp.R")
source("discreteSL.R")
source("estimators.R")
source("experiment.R")

plan(multisession, workers=59)

res <- function(results, truth = 5.8814) {
  results %>%
    summarize(
      emp_se = sd(effect),
      mean_est_se = mean(se),
      est_eff = mean(effect),
      rmse = sqrt(mean((effect - truth) ^ 2)),
      power = mean(2*(1 - pnorm(abs((effect) / se))) < 0.05),
      coverage = mean(((-1.96 * se + effect) < truth) &
                        (truth < (1.96 * se + effect)))
      
    )
}


# RUNS --------------------------------------------------------------------

# specification 0
params = expand_grid(
  n_hist=c(1000), 
  n_trial=c(250), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous") 
)
results = 1:200 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                   .options = furrr_options(seed = 3021377))

saveRDS(results, file = "sim_pipe2/results3/sp0_200runs.RDS")

# specification 01
params = expand_grid(
  n_hist=c(1000), 
  n_trial=c(250), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("constant") 
)
results = 1:200 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                   .options = furrr_options(seed = 3021377))

saveRDS(results, file = "sim_pipe2/results3/sp01_200runs.RDS")

# specification 1W1
params = expand_grid(
  n_hist=c(1000), 
  n_trial=c(250), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(-3, -5),
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous") 
)
results = 1:200 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                   .options = furrr_options(seed = 3021377))

saveRDS(results, file = "sim_pipe2/results3/sp1W1_7_200runs.RDS")

# specification 1U
params = expand_grid(
  n_hist=c(1000), 
  n_trial=c(250), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(0), # historical only
  shift_U=c(.5, 1), # historical only
  dgp_string =c("heterogeneous") 
)
results = 1:200 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                   .options = furrr_options(seed = 3021377))

saveRDS(results, file = "sim_pipe2/results3/sp1U_200runs.RDS")

# specification 2
params = expand_grid(
  n_hist=c(100, 250, 500, 750, 1000), 
  n_trial=c(250), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous") 
)
results = 1:200 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                   .options = furrr_options(seed = 3021377))

saveRDS(results, file = "sim_pipe2/results3/sp2_200runs.RDS")

# specification 3
params = expand_grid(
  n_hist=c(1000), 
  n_trial=c(100, 250, 500, 750, 1000), 
  p=c(20),
  y_noise=c(2), 
  shift_W1=c(0), # historical only
  shift_U=c(0), # historical only
  dgp_string =c("heterogeneous") 
)
results = 1:200 %>% future_map_dfr(~params %>% pmap_df(experiment),
                                   .options = furrr_options(seed = 3021377))

saveRDS(results, file = "sim_pipe2/results3/sp3_200runs.RDS")
