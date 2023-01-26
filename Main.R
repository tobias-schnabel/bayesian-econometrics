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

#load Default Data Set
attach(Default)

# tidy
#make factors numerical
data = Default %>% 
  mutate(default=ifelse(default=="No", 0,1)) %>% 
  mutate(student=ifelse(student=="No", 0,1))

#df summary statistics
stargazer(data, type = "text")

#generate datasets with fewer obs to compare models
intrain_1k = caret::createDataPartition(
  y = data$default,
  p = 0.1,
  list = F
)

intrain_5k = caret::createDataPartition(
  y = data$default,
  p = 0.5,
  list = F
)

subset1 = data[intrain_1k,]
subset2 = data[intrain_5k,]

#verify proportions
props = rbind(table(data$default)[2]/table(data$default)[1],
              table(subset1$default)[2]/table(subset1$default)[1],
              table(subset2$default)[2]/table(subset2$default)[1])
nrows = rbind(nrow(data), nrow(subset1), nrow(subset2))

#create matrix to export later
data_integrity = as.matrix(cbind(nrows, props))
rownames(data_integrity) = c("Original Data", "Subset 1", "Subset 2")
colnames(data_integrity) = c("n_obs", "Proportion of Defaults")
print(data_integrity)

#estimate logit baseline
form = formula(default ~ student + balance + income)

baseline = glm(form, data = data, family = "binomial")
tidy(baseline)
plot(baseline)


####flat priors####

#fit stan model
flat.fit = stan_glm(default ~ student + balance + income, data = data, 
                    family = binomial(link = "logit"), y = T, 
                    algorithm = "sampling", 
                 warmup = 1000, iter = 10000, chains = 4, refresh = 10000)

#generate yrep for this prior
yrep.flat = posterior_predict(flat.fit, draws = 1000)

#extract posterior
posterior.flat = as.matrix(flat.fit)

#generate tidy df for use with ggplot
plotposterior.flat = as.data.frame(flat.fit) %>% 
  reshape2::melt(measure.vars = 1:4)



####strong / data-driven priors####
tidy(baseline)
#set data-driven priors: means and SD taken from baseline logit output
data_driven_prior = normal(location = c(0.5, -0.1, -0.011), 
           scale = c(0.236, 0.000232, 0.00000820), autoscale = F)

#estimate models with data-driven / strong prior
#fit strong priors (data-driven) on ***FULL data set***
strong.fit = stan_glm(default ~ student + balance + income, data = data, 
                    family = binomial(link = "logit"), y = T, 
                    algorithm = "sampling", 
                    prior = data_driven_prior,
                    warmup = 1000, iter = 10000, chains = 4, refresh = 10000)

yrep.strong = posterior_predict(strong.fit, draws = 1000) #gen yrep
posterior.strong = as.matrix(strong.fit) #extract posterior

#generate tidy df for ggplot
plotposterior.strong = as.data.frame(strong.fit) %>% 
  reshape2::melt(measure.vars = 1:4)

#fit strong priors (data-driven) on  ***SUBSET 1***
strong.fit.s1 = stan_glm(default ~ student + balance + income, data = subset1, 
                      family = binomial(link = "logit"), y = T, 
                      algorithm = "sampling", 
                      prior = data_driven_prior,
                      warmup = 1000, iter = 10000, chains = 4, refresh = 0)

yrep.strong.s1 = posterior_predict(strong.fit.s1, draws = 1000) #gen yrep
posterior.strong.s1 = as.matrix(strong.fit.s1) #extract posterior

#fit strong priors (data-driven) on ***SUBSET 2***
strong.fit.s2 = stan_glm(default ~ student + balance + income, data = subset2, 
                      family = binomial(link = "logit"), y = T, 
                      algorithm = "sampling", 
                      prior = data_driven_prior,
                      warmup = 1000, iter = 10000, chains = 4, refresh = 0)

yrep.strong.s2 = posterior_predict(strong.fit.s2, draws = 1000) #gen yrep
posterior.strong.s2 = as.matrix(strong.fit.s2) #extract posterior


####COMPARE RESULTS####
#monitor results
monitor(posterior.flat)

#look at flat priors
prior_summary(flat.fit)

#monitor results
monitor(posterior.strong)

#look at strong priors
prior_summary(strong.fit)

#monitor results subset 1
monitor(posterior.strong.s1)

#monitor results subset 2
monitor(posterior.strong.s2)

#Geweke Test
geweke.diag(posterior.flat)
geweke.diag(posterior.strong)
geweke.diag(posterior.strong.s1)
geweke.diag(posterior.strong.s2)

#Geweke plots
geweke.plot(as.mcmc(posterior.flat))
geweke.plot(as.mcmc(posterior.strong))
geweke.plot(as.mcmc(posterior.strong.s1))
geweke.plot(as.mcmc(posterior.strong.s2))


#compare 10-fold cv with diff sample sizes, this way of performing 10-fold cv
#adds an attribute to each model
#*****NOTE*****this will take quite a while to run
#I would recommend lowering k to 3-4 unless strong compute is available
# reason why I chose k = 10: my machine has 10 cores, which this function utilizes
# reason why not LOOCV: too computationally expensive
flat.fit$loo = kfold(flat.fit, k = nrow(data))
strong.fit$loo = kfold(strong.fit, k = nrow(data))
strong.fit.s1$loo = kfold(strong.fit.s1, k = 10)
strong.fit.s2$loo = kfold(strong.fit.s2, k = nrow(subset2))

#compare
loocv.comp = loo_compare(flat.fit, strong.fit)
strong.fit.s1$loo
strong.fit.s2$loo

####Graphical PPC####
# do plots
#define custom functions for plots below
prop_zero <- function(x) mean(x == 0)
prop_one <- function(x) mean(x == 1)
source('Plots.R')

####Housekeeping Pt2####
#export plots and Tables and copy code files
if (Sys.info()[7] == "ts") { 
  #this code only executes on my machine to prevent errors
  source('Tables.R')
  source('Plot.Export.R')
  setwd('/Users/ts/Git/ise')
  
  #copy code files to overleaf
  file.copy('Main.R', '/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Code', overwrite = T)
  file.copy('scrap_file.R', '/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Code', overwrite = T)
}

####Show Plots####
#display plots (run each line to show plots, might take a few seconds)
phf
dof
dodf
propcomp
denscomp
discretedenscomp
rhatcomp
neffcomp
acfcomp
do_sample_comp
gp1
gp2
gp3
gp4






