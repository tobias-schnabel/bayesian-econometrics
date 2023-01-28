## Tobias Schnabel ##
## i6255807 ##

rm(list = ls(all = TRUE)) #CLEAR ALL

####Housekeeping####
packages <- c("tidyverse", "broom", "ggpubr", "stargazer", "caret", "rstan",
              "rstanarm", "kableExtra", "bayesplot", "coda", "ISLR")

#Comment in lines below to Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }

#load packages
invisible(lapply(packages, library, character.only = TRUE))

#set options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#load Credit Card Default Data Set
attach(Default)

#record start time
start.time = Sys.time()

# tidy
#make factors numerical
data = Default %>% 
  mutate(default=ifelse(default=="No", 0,1)) %>% 
  mutate(student=ifelse(student=="No", 0,1))

#df summary statistics
stargazer(data, type = "text")
#prepare data for STAN
#make recipe
rec = recipe(default ~ student + balance + income, data = data) %>% 
  prep(retain = T)

#extract X matrix and y vector
X = juice(rec, all_predictors(), composition = 'matrix')
y = juice(rec, all_outcomes(), composition = 'matrix') %>% drop()

#feed data into STAN
stan_data <- list(
  X = X,
  K = ncol(X),
  N = nrow(X),
  y = y,
  use_y_rep = T,
  use_log_lik = F
)

#initialize models
stan.mod = stan_model('Final_Assignment_Schnabel.stan')

#flat priors WITHOUT income variable
# does not work
# flat.fit.hmc = sampling(stan.mod, data = stan_data,
#                     algorithm = "HMC",
#                  warmup = 1000, iter = 10000, chains = 1, thin = 1)

#does work, but does not perform MCMC
flat.fit.fixedparam = sampling(stan.mod, data = stan_data, 
                               algorithm = "Fixed_param",
                               warmup = 1000, iter = 10000, chains = 4, thin = 1)

#does not work
# flat.fit.nuts = sampling(stan.mod, data = stan_data, 
#                       algorithm = "NUTS",
#                       warmup = 1000, iter = 10000, chains = 1, thin = 1)


fixedparam_params = rstan::extract(flat.fit.fixedparam)
monitor(flat.fit.fixedparam)

#check proportions of 0s and ones
ppc_stat(y, yrep.flat, stat = "prop_zero", binwidth = 0.00005)
ppc_stat(y, yrep.flat, stat = "prop_one", binwidth = 0.00005)

#check posterior.flat trace
color_scheme_set("mix-blue-pink")
mcmc_trace(flat.fit.fixedparam)
mcmc_pairs(flat.fit.fixedparam)

# mcmc diagnostics
# rhat
plot(flat.fit.fixedparam, "rhat")
plot(flat.fit.fixedparam, "rhat_hist")
# ratio of effective sample size to total posterior.flat sample size
plot(flat.fit.fixedparam, "neff")
plot(flat.fit.fixedparam, "neff_hist")
# autocorrelation by chain
plot(flat.fit.fixedparam, "acf", pars = "(Intercept)")
plot(flat.fit.fixedparam, "acf_bar", pars = "(Intercept)")
mcmc_acf(flat.fit.fixedparam)