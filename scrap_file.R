#storage for code that might need to be re-used

#feed data into STAN
stan_data_1 <- list(
  X = X,
  K = ncol(X),
  N = nrow(X),
  y = y,
  use_y_rep = T,
  use_log_lik = F
)

stan_data_2 <- list(
  X = X2,
  K = ncol(X2),
  N = nrow(X2),
  y = y,
  use_y_rep = T,
  use_log_lik = F
)

#initialize models
stan.mod = stan_model('Final_Assignment_Schnabel.stan')

#flat priors WITHOUT income variable
# does not work
# flat.fit.hmc = sampling(stan.mod, data = stan_data_2,
#                     algorithm = "HMC",
#                  warmup = 1000, iter = 10000, chains = 1, thin = 1)

#does work
flat.fit.fixedparam = sampling(stan.mod, data = stan_data_2, 
                               algorithm = "Fixed_param",
                               warmup = 1000, iter = 10000, chains = 4, thin = 1)

#does not work
# flat.fit.nuts = sampling(stan.mod, data = stan_data_2, 
#                       algorithm = "NUTS",
#                       warmup = 1000, iter = 10000, chains = 1, thin = 1)