setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Tables')
#summary stats
stargazer(data, summary = T, title = "Summary Statistics", float = T, 
          table.placement = "H")
#baseline
stargazer(baseline, title = "Baseline Estimation Results", float = T, 
          table.placement = "H")

#Data Integrity Table
kable(data_integrity, format = "latex", 
      digits = 4,
      caption = "Data Splits") %>% 
  save_kable("data_integrity.tex")

#summary tables of fits
kable(as.data.frame(summary(flat.fit)), format = "latex", 
      digits = 4,
      caption = "Fit with Flat Priors") %>% 
  save_kable("flatfitsumm.tex")

kable(as.data.frame(summary(strong.fit)), format = "latex", 
      digits = 4,
      caption = "Fit with Strong Priors, Full Data Set") %>% 
  save_kable("strongfit_full.tex")

kable(as.data.frame(summary(strong.fit.s1)), format = "latex", 
      digits = 4,
      caption = "Fit with Strong Priors, Subset 1") %>% 
  save_kable("strongfit_sub1.tex")

kable(as.data.frame(summary(strong.fit.s2)), format = "latex", 
      digits = 4,
      caption = "Fit with Strong Priors, Subset 2") %>% 
  save_kable("strongfit_sub2.tex")

setwd('/Users/ts/Git/ise')