# Tutorial 1
rm(list = ls(all = TRUE)) ###CLEAR ALL

#load packages / setup
library(lubridate)
library(xts)

##1##
#get data
rawdata = read.csv('NLInflation.csv')

##make TS

#create date object
rawdata$dateobj = lubridate::parse_date_time(rawdata$Month, "ym")
#create xts
tsdat = xts(rawdata$AnnualCPIchange, order.by=rawdata$dateobj)
names(tsdat) = "AnnualCPIchange"

##2##
#plot
plot(tsdat)

#re-extract to regular df
data = data.frame(date=index(tsdat), coredata(tsdat))

#sumstats
library(stargazer)
stargazer(data, type = 'text')

##3##
#regression model
model1 = lm(AnnualCPIchange ~ date, data = data)

##4##
stargazer(model1, type = 'text')

##5##
#CPI change requires a prior, months are fixed and known in advance with 0 uncertainty

##6##
#flat prior would make sense, inflation is unpredictable

#####PREP STAN#####
library(tidyverse)
library(recipes)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat = data
dat$date = 1:84

#make recipe
rec = recipe(AnnualCPIchange ~ date, data = dat) %>% 
  prep(retain = T)

X = juice(rec, all_predictors(), composition = 'matrix')
y = drop(juice(rec, all_outcomes(), composition = 'matrix'))

#initialize
mod1 = stan_model('tut1.stan')

#prep data
mod1_data <- list(
  X = X,
  K = ncol(X),
  N = nrow(X),
  y = y,
  use_y_rep = FALSE,
  use_log_lik = FALSE
)

# mod1_data$scale_alpha <- sd(y) * 10
# mod1_data$scale_beta <- apply(X, 2, sd) * sd(y) * 2.5
mod1_data$loc_sigma <- sd(y)


mod1_fit = sampling(mod1, data = mod1_data, 
                    warmup = 500, iter = 1000, chains = 1, cores = 1, thin = 1)
summary(mod1_fit)
