#define plot title for flat and strong priors, note of how many dras
tf = ggtitle("Flat Prior")
ts = ggtitle("Strong Prior")
dn = labs(caption = "1000 Draws from Posterior")

#set variables for plots for FLAT PRIORS
yrep = yrep.flat
posterior = posterior.flat
fit = flat.fit

#histogram of posterior.flat
htf = ggtitle("Flat Priors")
phf = ggplot(data = plotposterior.flat,aes(x = value, group = variable)) +
  geom_histogram(bins=300, colour = "#99cbff") +
  facet_wrap(~ variable, scales = "free_x") + htf +
  scale_x_continuous(labels = scales::comma) + 
  dn

#density overlay
dof = ppc_dens_overlay(y, yrep) + 
  scale_x_continuous( limits=c(0, 1), 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  tf

#discrete density overlay
dodf = ppc_ecdf_overlay(y, yrep, discrete = T) + tf

#check proportions
p0f = ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.00005) + tf
p1f = ppc_stat(y, yrep, stat = "prop_one", binwidth = 0.00005) + tf

#check posterior.flat trace
color_scheme_set("mix-blue-pink")
trace.flat = mcmc_trace(fit) #+ tf
pairs.flat = mcmc_pairs(fit) #+ tf

# mcmc diagnostics
# rhat
rhat.flat = plot(fit, "rhat") + tf

# ratio of effective sample size to total posterior sample size
neff.flat = plot(fit, "neff") + tf

# autocorrelation by chain
acfb.flat = plot(fit, "acf_bar", pars = "(Intercept)") + tf

#joint acf
acf.flat = mcmc_acf(fit) + tf

#########REPEAT ALL PLOTS FOR STRONG PRIOR MODEL#########

#set variables for plots for STRONG PRIORS
yrep = yrep.strong
posterior = posterior.strong
fit = strong.fit

color_scheme_set("red")

#histogram of posterior.strong
hts = ggtitle("Strong Priors")
phs = ggplot(data = plotposterior.strong,aes(x = value, group = variable)) +
  geom_histogram(bins=300, colour = "#99cbff") +
  facet_wrap(~ variable, scales = "free_x") + hts +
  scale_x_continuous(labels = scales::comma)
+ dn

#density overlay
dos = ppc_dens_overlay(y, yrep) + 
  scale_x_continuous( limits=c(0, 1), 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  ts

#discrete density overlay
dods = ppc_ecdf_overlay(y, yrep, discrete = T) + ts

#check proportions of 0s and ones
p0s = ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.00005) + ts
p1s = ppc_stat(y, yrep, stat = "prop_one", binwidth = 0.00005) + ts

#check posterior.flat trace
color_scheme_set("mix-blue-pink")
trace.strong = mcmc_trace(fit) #+ ts
pairs.strong = mcmc_pairs(fit) #+ ts

# mcmc diagnostics
# rhat
rhat.strong = plot(fit, "rhat") + ts
neff.strong = plot(fit, "neff") + ts
acfb.strong = plot(fit, "acf_bar", pars = "(Intercept)") + ts


#joint acf
acf.strong = mcmc_acf(fit) + ts

######Do side-by-side-comparison Plots######
#compare posterior histograms
histcomp = ggarrange(phf, phs)
#compare prop 0/1
propcomp = ggarrange(p0f, p0s, p1f, p1s)

#compare density overlays
denscomp = ggarrange(dof, dos)
#compare discrete density overlays
discretedenscomp = ggarrange(dodf, dods)
#mcmc
rhatcomp = ggarrange(rhat.flat, rhat.strong, neff.flat, neff.strong, ncol = 2)
#acf
acfcomp = ggarrange(acf.flat, acf.strong)

#make list of plots for printing
plotlist = list(phf, dof, dodf, propcomp, denscomp, discretedenscomp, rhatcomp, acfcomp)
