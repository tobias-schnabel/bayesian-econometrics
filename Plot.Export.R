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
ggsave("posterior_hist_flat.jpg", phf)
ggsave("density_overlay_flat.jpg", dof)
ggsave("density_overlay_discrete_flat.jpg", dodf)

#check proportions of 0s and ones
ggsave("prop0_flat.jpg", p0f)
ggsave("prop1_flat.jpg", p1f)
ggsave("mcmc_trace_flat.jpg", trace.flat)
ggsave("mcmc_pairs.jpg", pairs.flat)
ggsave("rhat_flat.jpg", rhat.flat) 
ggsave("neff_flat.jpg", neff.flat) 
ggsave("acf_bars_flat.jpg", acfb.flat)
ggsave("acf_flat.jpg", acf.flat)
ggsave("posterior_hist_strong.jpg")
ggsave("density_overlay_strong.jpg", dos) 
ggsave("density_overlay_discrete_strong.jpg", dods) 
ggsave("prop0_strong.jpg", p0s)
ggsave("prop1_strong.jpg", p1s)
ggsave("mcmc_trace_strong.jpg", trace.strong)
ggsave("mcmc_pairs.jpg", pairs.strong)
ggsave("rhat_strong.jpg", rhat.strong)
ggsave("neff_strong.jpg", neff.strong)
ggsave("acf_bars_strong.jpg", acfb.strong)
ggsave("acf_strong.jpg", acf.strong)

######DO side-by-side-comparison Plots######
setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Figures/comp')

ggsave('hist_comp.jpg', histcomp)
ggsave('prop_comp.jpg', propcomp)
ggsave('dens_comp.jpg', denscomp)
ggsave('dens_dis_comp.jpg', discretedenscomp)
ggsave('mcmc_comp.jpg', rhatcomp)
ggsave('acf_comp.jpg', acfcomp)

setwd('/Users/ts/Git/ise')
