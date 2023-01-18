## Tobias Schnabel ##
## i6255807 ##

rm(list = ls(all = TRUE)) #CLEAR ALL

## Housekeeping ##
library(tidyverse)
library(stargazer)
library(recipes)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)