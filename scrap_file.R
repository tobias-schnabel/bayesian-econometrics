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







dot = ggtitle("Density Overlay Plot, 1000 Draws")
do = ppc_dens_overlay(y, yrep.flat) + 
  scale_x_continuous( limits=c(0, 1), 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  dot

ppc_ecdf_overlay(y, yrep.flat, discrete = T)



#check proportions of 0s and ones
ppc_stat(y, yrep.flat, stat = "prop_zero", binwidth = 0.00005)
ppc_stat(y, yrep.flat, stat = "prop_one", binwidth = 0.00005)

#check posterior.flat trace
color_scheme_set("mix-blue-pink")
mcmc_trace(flat.fit)
mcmc_pairs(flat.fit)

# mcmc diagnostics
# rhat
plot(flat.fit, "rhat")
plot(flat.fit, "rhat_hist")
# ratio of effective sample size to total posterior.flat sample size
plot(flat.fit, "neff")
plot(flat.fit, "neff_hist")
# autocorrelation by chain
plot(flat.fit, "acf", pars = "(Intercept)")
plot(flat.fit, "acf_bar", pars = "(Intercept)")
mcmc_acf(flat.fit)