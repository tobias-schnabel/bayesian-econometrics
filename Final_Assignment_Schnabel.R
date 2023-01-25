## Tobias Schnabel ##
## i6255807 ##

rm(list = ls(all = TRUE)) #CLEAR ALL

## Housekeeping ##
library(tidyverse)
library(ggpubr)
library(stargazer)
library(caret)
library(rstan)
library(rstanarm)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(bayesplot)
library(coda)

#load ISLR for data
library(ISLR)
attach(Default)

# tidy
#make factors numerical
data = Default %>% 
  mutate(default=ifelse(default=="No", 0,1)) %>% 
  mutate(student=ifelse(student=="No", 0,1))

#df summary statistics
stargazer(data, type = "text")

#generate datasets with fewer obs to compare models
prop_default = table(data$default)[2]/table(data$default)[1] #3.4% default rate


#estimate logit baseline
form = formula(default ~ student + balance + income)

baseline = glm(form, data = data, family = "binomial")
tidy(baseline)
plot(baseline)


#flat priors
flat.fit = stan_glm(default ~ student + balance + income, data = data, 
                    family = binomial(link = "logit"), y = T, 
                    algorithm = "sampling", 
                 warmup = 1000, iter = 10000, chains = 4, refresh = 10000)

yrep.flat = posterior_predict(flat.fit, draws = 1000)

posterior.flat = as.matrix(flat.fit)

#tidy df for ggplot
plotposterior.flat = as.data.frame(flat.fit) %>% 
  reshape2::melt(measure.vars = 1:4)

#strong priors (data-driven)
strong.fit = stan_glm(default ~ student + balance + income, data = data, 
                    family = binomial(link = "logit"), y = T, 
                    algorithm = "sampling", 
                    prior = normal(location = c(2, 1, 1), scale = c(0.492, ), autoscale = F),
                    warmup = 1000, iter = 10000, chains = 4, refresh = 10000)

yrep.strong = posterior_predict(strong.fit, draws = 1000)

posterior.strong = as.matrix(strong.fit)

#tidy df for ggplot
plotposterior.strong = as.data.frame(strong.fit) %>% 
  reshape2::melt(measure.vars = 1:4)


#monitor results
monitor(posterior.flat)

#look at flat priors
prior_summary(flat.fit)

#monitor results
monitor(posterior.strong)

#look at strong priors
prior_summary(strong.fit)

#define custom functions for plots below
prop_zero <- function(x) mean(x == 0)
prop_one <- function(x) mean(x == 1)

#Geweke Test
geweke.diag(posterior.flat)

geweke.diag(posterior.strong)


#loocv with diff sample sizes


#trimming the posterior


# do plots
source('Plots.R')

#export plots and Tables
if (Sys.info()[7] == "ts") { 
  #this code only executes on my machine to prevent errors
  source('Tables.R')
  source('Plot.Export.R')
  setwd('/Users/ts/Git/ise')
}

#display plots (run each line to show plots, might take a few seconds)
phf
dof
dodf
propcomp
denscomp
discretedenscomp
rhatcomp
acfcomp






