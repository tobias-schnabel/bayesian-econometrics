#define plot title for flat and strong priors, note of how many dras
tf = ggtitle("Flat Prior")
ts = ggtitle("Strong Prior")
dn = labs(caption = "1000 Draws from Posterior")
full = ggtitle("Full Data Set")
sub1 = ggtitle("1,000 obs")
sub2 = ggtitle("5,000 obs")

color_scheme_set("brightblue")

#set variables for plots for FLAT PRIORS
y = data$default
y_s1 = subset1$default
y_s2 = subset2$default
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
  scale_x_continuous(limits=c(0, 1), 
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
trace.flat = mcmc_trace(flat.fit) #+ tf
pairs.flat = mcmc_pairs(flat.fit) #+ tf

# mcmc diagnostics
# rhat
rhat.flat = plot(flat.fit, "rhat") + tf

# ratio of effective sample size to total posterior sample size
neff.flat = plot(flat.fit, "neff") + tf

# autocorrelation by chain
acfb.flat = plot(flat.fit, "acf_bar", pars = "student") + tf

#joint acf
acf.flat = mcmc_acf(flat.fit) + tf

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
  scale_x_continuous(labels = scales::comma) + dn

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
p0s_ = ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.00005) + full
p1s_ = ppc_stat(y, yrep, stat = "prop_one", binwidth = 0.00005) + full

#check posterior.flat trace
color_scheme_set("mix-blue-pink")
trace.strong = mcmc_trace(strong.fit) #+ ts
pairs.strong = mcmc_pairs(strong.fit) #+ ts

# mcmc diagnostics
# rhat
rhat.strong = plot(strong.fit, "rhat") + ts
neff.strong = plot(strong.fit, "neff") + ts
acfb.strong = plot(strong.fit, "acf_bar", pars = "student") + ts


#joint acf
acf.strong = mcmc_acf(strong.fit) + ts

######Do side-by-side-comparison Plots######
#compare posterior histograms
histcomp = ggarrange(phf, phs)
#compare prop 0/1
propcomp = ggarrange(p0f, p0s, p1f, p1s)

#prop 0 and 1 by sample size
p0s.s1 = ppc_stat(y_s1, yrep.strong.s1, stat = "prop_zero", binwidth = 0.00005) + sub1
p1s.s1 = ppc_stat(y_s1, yrep.strong.s1, stat = "prop_one", binwidth = 0.00005) + sub1
p0s.s2 = ppc_stat(y_s2, yrep.strong.s2, stat = "prop_zero", binwidth = 0.00005) + sub2
p1s.s2 = ppc_stat(y_s2, yrep.strong.s2, stat = "prop_one", binwidth = 0.00005) + sub2

propcomp_ss = ggarrange(p0s.s1, p1s.s1, p0s.s2, p1s.s2, p0s_, p1s_, nrow = 3, ncol = 2)

#compare density overlays
denscomp = ggarrange(dof, dos)
#compare discrete density overlays
discretedenscomp = ggarrange(dodf, dods)
#mcmc
rhatcomp = ggarrange(rhat.flat, rhat.strong,
                     common.legend = T, legend = "bottom")

neffcomp = ggarrange(neff.flat, neff.strong,
                     common.legend = T, legend = "bottom")
#acf
acfcomp = ggarrange(acf.flat, acf.strong)

#set color scheme for sample size comparisons
color_scheme_set("mix-blue-red")

######DO sample size comparison Plots######
dos.comp = ppc_dens_overlay(y, yrep.strong) + 
  scale_x_continuous( limits=c(0, 1), 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  full

do.s1 = ppc_dens_overlay(y_s1, yrep.strong.s1) + 
  scale_x_continuous( limits=c(0, 1), 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  sub1

do.s2 = ppc_dens_overlay(y_s2, yrep.strong.s2) + 
  scale_x_continuous( limits=c(0, 1), 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  sub2

do_sample_comp = ggarrange(do.s1, do.s2, dos.comp, nrow = 3)

rhat.s.full = plot(strong.fit, "rhat") + full
rhat.s.s1 = plot(strong.fit.s1, "rhat") + sub1
rhat.s.s2 = plot(strong.fit.s2, "rhat") + sub2

rhat_sample_comp = ggarrange(rhat.s.s1, rhat.s.s2, rhat.s.full, nrow = 3,
                             common.legend = T, legend = "bottom")

neff.s.full = plot(strong.fit, "neff") + full
neff.s.s1 = plot(strong.fit.s1, "neff") + sub1
neff.s.s2 = plot(strong.fit.s2, "neff") + sub2

neff_sample_comp = ggarrange(neff.s.s1, neff.s.s2, neff.s.full, nrow = 3,
                             common.legend = T, legend = "bottom")

acf.s.full = plot(strong.fit, "acf", pars = "(Intercept)") + full + xlim(0,7)
acf.s.s1 = plot(strong.fit.s1, "acf", pars = "(Intercept)") + sub1 + xlim(0,7)
acf.s.s2 = plot(strong.fit.s2, "acf", pars = "(Intercept)") + sub2 + xlim(0,7)

acf_sample_comp = ggarrange(acf.s.s1, acf.s.s2, acf.s.full, nrow = 3,
                             common.legend = T, legend = "bottom") 



#reset color scheme
color_scheme_set("brightblue")




