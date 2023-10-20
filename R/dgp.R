# script 1: Data #

library(data.table)
library(magrittr)

# Data generating process outcome specification ---------------------------
set_dgp <- function(dgp_string = 'constant'){
  if(dgp_string == 'constant'){
    mu1 <<- function(X){
      X %$%
        {(sin((abs(W1))*pi)*10) - 0.8}
    }
    
    mu0 <<- function(X){
      X %$%
        {sin(abs(W1)*pi)*10}
    }
  } else if (dgp_string == 'heterogeneous'){
    # dgp2 is m0^2
    mu1 <<- function(X){
      X %$%
        {(sin((abs(W1))*pi)*10)^2 - 42}
    }
    
    mu0 <<- function(X){
      X %$%
        {sin(abs(W1)*pi)*10}
    }
  } 
  invisible()
}


# data generating process -------------------------------------------------
dgp = function(n, p=10, y_noise = 2, shift_W1 = 0, shift_U = 0) {
  p = pmax(p, 10)
  W1_lbound = -2 + shift_W1
  W1_ubound = 1 + shift_W1
  U_lbound = 0 + shift_U
  U_ubound = 1 + shift_U
  
  W1 <- runif(n, min = W1_lbound, max = W1_ubound)
  W2 <- rnorm(n, 0, 3)
  W3 <- rexp(n, .8)
  W4 <- rgamma(n, 5, 10)
  W5 <- rgamma(n, 2, 1)
  
  U <- runif(n, min = U_lbound, max = U_ubound)
  
  matrix(runif(n * (p-5), -1, 1), ncol = (p-5)) %>%
    as_tibble(name_repair='universal') %>% 
    set_names(str_c("W", 6:(p))) %>%
    tibble(U, W1, W2, W3, W4, W5) %>%
    inset("m0", value= mu0(.)  +
            as.numeric(U > 1.01) *8 +
            as.numeric(U > 1.55) *15) %>% 
    inset("m1", value= mu1(.) +
            as.numeric(U > 1.01) *8 + 
            as.numeric(U > 1.55) *15) %>% 
    mutate(
      A = rbinom(n, 1, prob=1/2),
      Y0 = m0 + rnorm(n,sd=y_noise),
      Y1 = m1 + rnorm(n,sd=y_noise),
      Y = ifelse(A,Y1,Y0)
    ) 
}
