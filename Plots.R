setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Figures/ind')

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

####Gaphical PPC###
color_scheme_set("brightblue")
#color_scheme_get()
# 1    #cce5ff
# 2    #99cbff
# 3    #4ca5ff
# 4    #198bff
# 5    #0065cc
# 6    #004c99

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

ggsave("posterior_hist_flat.jpg", phf)

#density overlay
dof = ppc_dens_overlay(y, yrep) + 
  scale_x_continuous( limits=c(0, 1), 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  tf
ggsave("density_overlay_flat.jpg", dof)
#discrete density overlay
dodf = ppc_ecdf_overlay(y, yrep, discrete = T) + tf
ggsave("density_overlay_discrete_flat.jpg", dodf)

#check proportions of 0s and ones
p0f = ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.00005) + tf
ggsave("prop0_flat.jpg", p0f)
p1f = ppc_stat(y, yrep, stat = "prop_one", binwidth = 0.00005) + tf
ggsave("prop1_flat.jpg", p1f)

#check posterior.flat trace
color_scheme_set("mix-blue-pink")
trace.flat = mcmc_trace(fit) #+ tf
ggsave("mcmc_trace_flat.jpg", trace.flat)
pairs.flat = mcmc_pairs(fit) #+ tf
ggsave("mcmc_pairs.jpg", pairs.flat)

# mcmc diagnostics
# rhat
rhat.flat = plot(fit, "rhat") + tf
ggsave("rhat_flat.jpg", rhat.flat) 
#plot(fit, "rhat_hist")
# ratio of effective sample size to total posterior.flat sample size
neff.flat = plot(fit, "neff") + tf
ggsave("neff_flat.jpg", neff.flat) 
#plot(fit, "neff_hist")
# autocorrelation by chain
# plot(fit, "acf", pars = "(Intercept)")
acfb.flat = plot(fit, "acf_bar", pars = "(Intercept)") + tf
ggsave("acf_bars_flat.jpg", acfb.flat)

#joint acf
acf.flat = mcmc_acf(fit) + tf
ggsave("acf_flat.jpg", acf.flat)


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

ggsave("posterior_hist_strong.jpg")

#density overlay
dos = ppc_dens_overlay(y, yrep) + 
  scale_x_continuous( limits=c(0, 1), 
                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 
                                 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  ts
ggsave("density_overlay_strong.jpg", dos) 
#discrete density overlay
dods = ppc_ecdf_overlay(y, yrep, discrete = T) + ts
ggsave("density_overlay_discrete_strong.jpg", dods) 

#check proportions of 0s and ones
p0s = ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.00005) + ts
ggsave("prop0_strong.jpg", p0s)
p1s = ppc_stat(y, yrep, stat = "prop_one", binwidth = 0.00005) + ts
ggsave("prop1_strong.jpg", p1s)

#check posterior.flat trace
color_scheme_set("mix-blue-pink")
trace.strong = mcmc_trace(fit) #+ ts
ggsave("mcmc_trace_strong.jpg", trace.strong)
pairs.strong = mcmc_pairs(fit) #+ ts
ggsave("mcmc_pairs.jpg", pairs.strong)

# mcmc diagnostics
# rhat
rhat.strong = plot(fit, "rhat") + ts
ggsave("rhat_strong.jpg", rhat.strong)
#plot(fit, "rhat_hist")
# ratio of effective sample size to total posterior.flat sample size
neff.strong = plot(fit, "neff") + ts
ggsave("neff_strong.jpg", neff.strong)
#plot(fit, "neff_hist")
# autocorrelation by chain
# plot(fit, "acf", pars = "(Intercept)")
acfb.strong = plot(fit, "acf_bar", pars = "(Intercept)") + ts
ggsave("acf_bars_strong.jpg", acfb.strong)

#joint acf
acf.strong = mcmc_acf(fit) + ts
ggsave("acf_strong.jpg", acf.strong)

######DO side-by-side-comparison Plots######
setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Figures/comp')

#compare posterior histograms
ggarrange(phf, phs)
ggsave('hist_comp.jpg')

#compare prop 0/1
ggarrange(p0f, p0s, p1f, p1s)
ggsave('prop_comp.jpg')

#compare density overlays
ggarrange(dof, dos)
ggsave('dens_comp.jpg')

#compare discrete density overlays
ggarrange(dodf, dods)
ggsave('dens_dis_comp.jpg')

#mcmc
ggarrange(rhat.flat, rhat.strong, neff.flat, neff.strong, ncol = 2)
ggsave('mcmc_comp.jpg')

#acf
ggarrange(acf.flat, acf.strong)
ggsave('acf_comp.jpg')

setwd('/Users/ts/Git/ise')