## Tobias Schnabel ##
## i6255807 ##

rm(list = ls(all = TRUE)) #CLEAR ALL

## Housekeeping ##
library(tidyverse)
library(broom)
library(stargazer)
library(recipes)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

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

#make recipe #2 without income due to high correlation

#extract X matrix and y vectors
X = juice(rec, all_predictors(), composition = 'matrix')
X2 = juice(rec, student, balance)
y = juice(rec, all_outcomes(), composition = 'matrix') %>% drop()

#feed data into STAN
stan_data <- list(
  X = X,
  K = ncol(X),
  N = nrow(X),
  y = y,
  use_y_rep = FALSE,
  use_log_lik = F
)



#flat priors
flat.fit = stan("Final_Assignment_Schnabel.stan", data = stan_data, 
                 warmup = 1000, iter = 10000, chains = 1, thin = 1)

params2 = rstan::extract(trunc.fit)

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

