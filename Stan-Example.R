rm(list = ls(all = TRUE)) ###CLEAR ALL

#load packages / setup
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)