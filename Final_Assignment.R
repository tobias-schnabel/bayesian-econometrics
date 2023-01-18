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

#load ISLR for data
library(ISLR)
attach(Default)

#df summary statistics
stargazer(Default, type = "text")

pairs(Default)

# tidy
data = Default

#make factors numerical
data %>% 
  mutate(default=ifelse(default=="No", 0,1)) %>% 
  mutate(student=ifelse(student=="No", 0,1))
  
