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

#estimate logit baseline
form = formula(default ~ student + balance + income)

baseline = glm(form, data = data, family = "binomial")
tidy(baseline)
plot(baseline)



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

#strong priors
strong.fit = stan_glm(default ~ student + balance + income, data = data, 
                    family = binomial(link = "logit"), y = T, 
                    algorithm = "sampling", 
                    prior = normal(location = c(2, 1, 1), scale = NULL, autoscale = T),
                    warmup = 1000, iter = 10000, chains = 4, refresh = 10000)

yrep.strong = posterior.flat_predict(strong.fit, draws = 1000)

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


####Gaphical PPC###
color_scheme_set("brightblue")
#color_scheme_get()
# 1    #cce5ff
# 2    #99cbff
# 3    #4ca5ff
# 4    #198bff
# 5    #0065cc
# 6    #004c99

#histogram of posterior.flat
ht = ggtitle("Histogram of 1000 Draws from posterior.flat")
ph = ggplot(data = plotposterior.flat,aes(x = value, group = variable)) +
  geom_histogram(bins=300, colour = "#99cbff") +
  facet_wrap(~ variable, scales = "free_x") + ht +
  scale_x_continuous(labels = scales::comma)

dot = ggtitle("Density Overlay Plot, 1000 Draws")
do = ppc_dens_overlay(y, yrep.flat) + 
  scale_x_continuous( limits=c(0, 1), 
              breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
              dot
                     
ppc_ecdf_overlay(y, yrep.flat, discrete = T)

#define custom functions
prop_zero <- function(x) mean(x == 0)
prop_one <- function(x) mean(x == 1)

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

#Geweke Test
geweke.diag(posterior.flat)


#posterior.flat predictive checks


#trimming the posterior


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

# do plots
if (Sys.info()[7] == "ts") { 
  #this code only executes on my machine to prevent errors
  setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Figures')
  
  #summary histograms
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
  
  
  #set variables for plots for FLAT PRIORS
  yrep = yrep.flat
  posterior = posterior.flat
  fit = flat.fit
  
  #density overlay
  ppc_dens_overlay(y, yrep) + 
    scale_x_continuous( limits=c(0, 1), 
                        breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                   0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
                    dot
  ggsave("density_overlay_flat.jpg")
  #discrete density overlay
  ppc_ecdf_overlay(y, yrep, discrete = T)
  ggsave("density_overlay_discrete_flat.jpg")
  
  #check proportions of 0s and ones
  ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.00005)
  ggsave("prop0_flat.jpg")
  ppc_stat(y, yrep, stat = "prop_one", binwidth = 0.00005)
  ggsave("prop1_flat.jpg")
  
  #check posterior.flat trace
  color_scheme_set("mix-blue-pink")
  mcmc_trace(fit)
  ggsave("mcmc_trace_flat.jpg")
  mcmc_pairs(fit)
  ggsave("mcmc_pairs.jpg")
  
  # mcmc diagnostics
  # rhat
  plot(fit, "rhat")
  ggsave("rhat_flat.jpg")
  #plot(fit, "rhat_hist")
  # ratio of effective sample size to total posterior.flat sample size
  plot(fit, "neff")
  ggsave("neff_flat.jpg")
  #plot(fit, "neff_hist")
  # autocorrelation by chain
  # plot(fit, "acf", pars = "(Intercept)")
  plot(fit, "acf_bar", pars = "(Intercept)")
  ggsave("acf_bars_flat.jpg")
  
  #joint acf
  mcmc_acf(fit)
  ggsave("acf_flat.jpg")
  
  
  #########REPEAT ALL PLOTS FOR STRONG PRIOR MODEL#########
  
  #set variables for plots for FLAT PRIORS
  yrep = yrep.strong
  posterior = posterior.strong
  fit = strong.fit
  
  #density overlay
  ppc_dens_overlay(y, yrep) + 
    scale_x_continuous( limits=c(0, 1), 
                        breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                   0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
                          dot
  ggsave("density_overlay_strong.jpg")
  #discrete density overlay
  ppc_ecdf_overlay(y, yrep, discrete = T)
  ggsave("density_overlay_discrete_strong.jpg")
  
  #check proportions of 0s and ones
  ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.00005)
  ggsave("prop0_strong.jpg")
  ppc_stat(y, yrep, stat = "prop_one", binwidth = 0.00005)
  ggsave("prop1_strong.jpg")
  
  #check posterior.flat trace
  color_scheme_set("mix-blue-pink")
  mcmc_trace(fit)
  ggsave("mcmc_trace_strong.jpg")
  mcmc_pairs(fit)
  ggsave("mcmc_pairs.jpg")
  
  # mcmc diagnostics
  # rhat
  plot(fit, "rhat")
  ggsave("rhat_strong.jpg")
  #plot(fit, "rhat_hist")
  # ratio of effective sample size to total posterior.flat sample size
  plot(fit, "neff")
  ggsave("neff_strong.jpg")
  #plot(fit, "neff_hist")
  # autocorrelation by chain
  # plot(fit, "acf", pars = "(Intercept)")
  plot(fit, "acf_bar", pars = "(Intercept)")
  ggsave("acf_bars_strong.jpg")
  
  #joint acf
  mcmc_acf(fit)
  ggsave("acf_strong.jpg")
  
  setwd('/Users/ts/Git/ise')
}


