# script 4: Experiment #

experiment = function(n_hist, n_trial,
                      p=p,
                      y_noise=y_noise, 
                      shift_W1=shift_W1, # historical only
                      shift_U=shift_U, # historical only
                      dgp_string =c("constant")) {
  set_dgp(dgp_string)
  data_hist = dgp(n = n_hist,
                  p = p, 
                  y_noise = y_noise, 
                  shift_W1 = shift_W1,
                  shift_U = shift_U) %>% 
    mutate(Y = Y0) %>%
    select(Y, starts_with('W'))
  
  if(n_hist >= 5000) {V = 3} else if(n_hist >= 1000) {V = 5} else {V = 10}
  
  lrnr = rsample::vfold_cv(data_hist, v = V) %>%
    get_best_learner() %>%
    fit(data_hist)
  
  data_trial = dgp(n = n_trial,
                   p = p, 
                   y_noise = y_noise, 
                   shift_W1 = 0,
                   shift_U = 0) %>%
    select(Y, A, starts_with('W'), m0)
  
  data_trial %<>% mutate(prog = predict(lrnr, data_trial %>% 
                                          select(Y, starts_with('W'))) %>% 
                           pull(.pred)) 
  
  bind_rows(
    data_trial %>% 
      select(-prog, -m0) %>%
      unadjusted_estimator() %>% 
      mutate(prog="none", estr="unadjusted"),
    data_trial %>% 
      select(-prog, -m0) %>%
      linear_estimator() %>% 
      mutate(prog="none", estr="linear"),
    data_trial %>% 
      select(-m0) %>%
      linear_estimator() %>% 
      mutate(prog="fit", estr="linear"),
    data_trial %>% 
      select(-prog) %>%
      linear_estimator() %>% 
      mutate(prog="oracle", estr="linear"),
    data_trial %>% 
      select(-prog, -m0) %>%
      tmle_estimator() %>% 
      mutate(prog="none", estr="tmle"),
    data_trial %>% 
      select(-m0) %>%
      tmle_estimator() %>% 
      mutate(prog="fit", estr="tmle"),
    data_trial %>% 
      select(-prog) %>%
      tmle_estimator() %>% 
      mutate(prog="oracle", estr="tmle")
  ) %>%
    mutate(n_hist=n_hist, n_trial=n_trial, p=p, y_noise=y_noise, shift_W1=shift_W1, shift_U=shift_U)
}
