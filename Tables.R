setwd('/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Tables')
#summary stats
stargazer(data, summary = T, title = "Summary Statistics", float = T, 
          table.placement = "H",
          label = "tab:sumstats",
          out = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Tables/sumstats")
#baseline
stargazer(baseline, title = "Baseline Estimation Results", float = T, 
          table.placement = "H",
          single.row = T,
          label = "tab:baseline",
          out = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/ISE_Assignment/Tables/baseline")

#Data Integrity Table
kable(data_integrity, format = "latex", 
      align = 'c',
      caption = "Data Splits") %>% 
  save_kable("data_integrity.tex")

#summary tables of fits
kable(as.data.frame(summary(flat.fit)), format = "latex", 
      digits = 4,
      caption = "Fit with Flat Priors") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  save_kable("flatfitsumm.tex")

kable(as.data.frame(summary(strong.fit)), format = "latex", 
      digits = 4,
      caption = "Fit with Strong Priors, Full Data Set") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  save_kable("strongfit_full.tex")

kable(as.data.frame(summary(strong.fit.s1)), format = "latex", 
      align = 'c',
      caption = "Fit with Strong Priors, Subset 1") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  save_kable("strongfit_sub1.tex")

kable(as.data.frame(summary(strong.fit.s2)), format = "latex", 
      align = 'c',
      caption = "Fit with Strong Priors, Subset 2") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  save_kable("strongfit_sub2.tex")

#LOOCV comparison full data
#prep matrix
loocvmat = as.matrix(cbind(rbind(strong.fit.s1$loo, strong.fit.s2$loo, strong.fit$loo, flat.fit$loo)[, 3:6],
                     rbind(nrow(strong.fit.s1$data), nrow(strong.fit.s2$data), 
                           nrow(strong.fit$data), nrow(flat.fit$data))))#loocv.comp
rownames(loocvmat) = c("Strong Priors, 1k obs", "Strong Priors, 5k obs", 
                       "Strong Priors, 10k obs", "Flat Priors")
colnames(loocvmat) = c("ELPD", "ELPD SE",
                       "# Params", "# Params SE",
                       "N")

loo_df = as.data.frame(loocvmat)
#make columns numeric while preserving colanames
loo_df$`ELPD` = as.numeric(loo_df$`ELPD`)
loo_df$`ELPD SE` = as.numeric(loo_df$`ELPD SE`)
loo_df$`# Params` = as.numeric(loo_df$`# Params`)
loo_df$`# Params SE` = as.numeric(loo_df$`# Params SE`)
loo_df$N = as.numeric(loo_df$N)

kable(loo_df, format = "latex", 
      align = 'c',
      caption = "10-fold CV Comparison") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  save_kable("loocv_comp.tex")

setwd('/Users/ts/Git/ise')
