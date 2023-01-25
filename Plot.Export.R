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

# export all plots
ggsave("posterior_hist_flat.png", phf)
ggsave("density_overlay_flat.png", dof)
ggsave("density_overlay_discrete_flat.png", dodf)

#check proportions of 0s and ones
ggsave("prop0_flat.png", p0f)
ggsave("prop1_flat.png", p1f)
ggsave("mcmc_trace_flat.png", trace.flat)
ggsave("mcmc_pairs.png", pairs.flat)
ggsave("rhat_flat.png", rhat.flat) 
ggsave("neff_flat.png", neff.flat) 
ggsave("acf_bars_flat.png", acfb.flat)
ggsave("acf_flat.png", acf.flat)
ggsave("posterior_hist_strong.png")
ggsave("density_overlay_strong.png", dos) 
ggsave("density_overlay_discrete_strong.png", dods) 
ggsave("prop0_strong.png", p0s)
ggsave("prop1_strong.png", p1s)
ggsave("mcmc_trace_strong.png", trace.strong)
ggsave("mcmc_pairs.png", pairs.strong)
ggsave("rhat_strong.png", rhat.strong)
ggsave("neff_strong.png", neff.strong)
ggsave("acf_bars_strong.png", acfb.strong)
ggsave("acf_strong.png", acf.strong)

######DO side-by-side-comparison Plots######
setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Figures/comp')

ggsave('hist_comp.png', histcomp)
ggsave('prop_comp.png', propcomp)
ggsave('dens_comp.png', denscomp)
ggsave('dens_dis_comp.png', discretedenscomp)

png("rhat_comp.png", width = 1000, height = 1000, units = "px") 
#ggsave('rhat_comp.png', rhatcomp)
rhatcomp
dev.off()
ggsave('acf_comp.png', acfcomp)

######DO sample size comparison Plots######
ggsave('dos_comp_ss', do_sample_comp)

setwd('/Users/ts/Git/ise')
