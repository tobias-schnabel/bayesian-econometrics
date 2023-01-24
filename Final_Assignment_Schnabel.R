## Tobias Schnabel ##
## i6255807 ##

rm(list = ls(all = TRUE)) #CLEAR ALL

## Housekeeping ##
library(tidyverse)
library(broom)
library(stargazer)
library(recipes)
library(rstan)
library(rstanarm)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(bayesplot)

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

#estimate logit baseline
form = formula(default ~ student + balance + income)

baseline = glm(form, data = data, family = "binomial")
tidy(baseline)
plot(baseline)

#summary histograms
if (Sys.info()[7] == "ts") { #this code only executes on my machine to prevent errors
  setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Figures')
  par(mfrow = c(2, 2)) #enable grid plot
  png("fss.png", width = 1000, height = 1000, units = "px") #start export
  plot(Default$default, main = "Default")
  plot(Default$student, main = "Student")
  hist(data$balance, main = "Balance", xlab = "Balance")
  hist(data$income, main = "Income", xlab = "Income")
  dev.off() #end export
  par(mfrow = c(1, 1)) #disable grid plot
  
  #correlograms
  png("corr1.png", width = 1000, height = 1000, units = "px") 
  pairs(data[c(2,4)])
  dev.off()
  png("corr2.png", width = 1000, height = 1000, units = "px") 
  pairs(data[3:4])
  dev.off()
  setwd('/Users/ts/Git/ise')
}

#prepare data for STAN
#make recipe
rec = recipe(default ~ student + balance + income, data = data) %>% 
  prep(retain = T)

#extract X matrix and y vectors
X = juice(rec, all_predictors(), composition = 'matrix')
y = juice(rec, all_outcomes(), composition = 'matrix') %>% drop()

#make recipe #2 without income due to high correlation
rec2 = recipe(default ~ student + balance, data = data) %>% 
  prep(retain = T)
X2 = juice(rec2, student, balance)
# y is identical


#flat priors WITH income variable
flat.fit = stan_glm(default ~ student + balance + income, data = data, 
                 family = "binomial", y = T, algorithm = "sampling", 
                 warmup = 1000, iter = 10000, chains = 4)

y_rep = posterior_predict(flat.fit, draws = 1000)

color_scheme_set("brightblue")
ppc_dens_overlay(y, yrep[1:50, ])

#compare results
par(mar=rep(0,4))
dev.off()
# pairs(params, pars = c("alpha", "beta"))
# pairs(flat.fit.fixedparam.2, pars = c("alpha", "beta", "eta"))

#data driven priors


#posterior predictive checks



# do tables
if (Sys.info()[7] == "ts") { #this code only executes on my machine to prevent errors
  setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Tables')
  #summary stats
  stargazer(data, summary = T, title = "Summary Statistics", float = T, 
            table.placement = "H")
  #baseline
  stargazer(baseline, title = "Baseline Estimation Results", float = T, 
            table.placement = "H")
  
  setwd('/Users/ts/Git/ise')
}

