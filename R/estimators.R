# script 3: Estimators #


# naïve/unadjusted estimator ----------------------------------------------
unadjusted_estimator <- function(data) {
  # unadjusted
  n = nrow(data)
  est_unadj = data %$% {
    mean(Y[A == 1]) - mean(Y[A == 0])
  }
  se_unadj = data %$% {
    sqrt(2*(var(Y[A == 1]) + var(Y[A == 0])) / n)
  }
  return(tibble(
    effect = est_unadj,
    se = se_unadj
  ))
}


# linear estimator --------------------------------------------------------
linear_estimator <- function(data){
  # linear
  linear_fit <- estimatr::lm_robust(Y ~ ., data)
  est_linear = linear_fit$coefficients[['A']]
  se_linear = linear_fit$std.error[['A']]
  return(tibble(
    effect = est_linear,
    se = se_linear
  ))
}


# tmle efficient estimator ------------------------------------------------
tmle_estimator <- function(
    data,
    ...
){
  n = nrow(data)
  
  # true g
  prop = rep(0.5, n)
  
  # get Q
  mu = cross_stack(data, ...)
  
  # get clever covariate
  H = (data$A)/(prop) - (!data$A)/(1 - prop)
  
  data %<>%
    mutate(mu0=mu$mu0, mu1=mu$mu1)
  
  target_fit = 
    glm(
      Y ~ -1 + H + offset(muA),
      data = data.table(Y = data$Y, muA = ifelse(data$A, data$mu1, data$mu0), H=H)
    )
  
  eps = coef(target_fit)
  
  updated_mu0 <- data$mu0 - eps*1/(1-prop)
  updated_mu1 <- data$mu1 + eps*1/prop
  
  data %<>%
    mutate(mu0=updated_mu0, 
           mu1=updated_mu1)
  
  data %<>% 
    mutate(Y_hat = ifelse(A==1, mu1, mu0))
  
  estimate = data %$% mean(mu1 - mu0)
  IC <- data %$% {(Y - Y_hat)*H +
      mu1 - mu0 - estimate}
  
  variance = var(IC)
  return(tibble(
    effect = estimate,
    se = sqrt(variance/n)
  ))
}



# efficient aipw estimator ------------------------------------------------
aipw_estimator <- function(
    data,
    ...
){
  n = nrow(data)
  # g fit
  prop = rep(0.5, n)
  mu = cross_stack(data, ...)
  data %<>%
    mutate(mu0=mu$mu0, mu1=mu$mu1)
  
  est_eq = data %$% {
    (   A    * (Y - mu1) /    prop    + mu1) - 
      ((1 - A) * (Y - mu0) / (1 - prop) + mu0)
  }
  estimate = mean(est_eq)
  
  inf_fn = est_eq - estimate
  variance = mean(inf_fn^2) / n
  
  return(tibble(
    effect = estimate,
    se = sqrt(variance)
  ))
}
