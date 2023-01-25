setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Tables')
#summary stats
stargazer(data, summary = T, title = "Summary Statistics", float = T, 
          table.placement = "H")
#baseline
stargazer(baseline, title = "Baseline Estimation Results", float = T, 
          table.placement = "H")

#summary tables of fits
kable(as.data.frame(summary(flat.fit)), format = "latex", 
      digits = 4,
      caption = "Fit with Flat Priors") %>% 
  save_kable("flatfitsumm.tex")

setwd('/Users/ts/Git/ise')

kable(as.data.frame(summary(strong.fit)), format = "latex", 
      digits = 4,
      caption = "Fit with Strong Priors") %>% 
  save_kable("strongfitsumm.tex")

setwd('/Users/ts/Git/ise')