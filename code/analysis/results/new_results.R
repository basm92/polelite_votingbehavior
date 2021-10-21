# new_results
source("./code/analysis/results/get_data.R")
source("./code/analysis/results/oster_selection_statistics.R")

## Models - for both
model1 <- datasets %>%
  filter(category == "fisc" | category == "suffrage", 
         house == "Tweede Kamer",
         class != "neutral") %>%
  lm(formula = vote ~ law + class)
model2 <- datasets %>%
  filter(category == "fisc" | category == "suffrage", house == "Tweede Kamer",
         class != "neutral") %>%
  lm(formula = vote ~ ihs(wealth_timevote) + law + class)
model3 <- datasets %>%
  filter(category == "fisc" | category == "suffrage", house == "Tweede Kamer",
         class != "neutral") %>%
  lm(formula = vote ~ ihs(wealth_timevote):category + law + class)
model4 <- lm(data = suffrage, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)
model5 <- lm(data = fiscal, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)

ols_pooled <- list(model1, model2, model3, model4, model5)
description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, 
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(25,26,27)
knitr::opts_current$set(label = "ols_pooled")
modelsummary(ols_pooled, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_pooled.tex",
             add_rows = description,
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage and Fiscal Legislation",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as ihs(Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise.")
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::add_header_above(c(" " = 1, "Pooled" = 3, "Suffrage" = 1, "Fiscal" = 1))

# Suffrage (First three panels) and Fiscal (Second three panels)
model1 <-lm(data = suffrage, formula = vote ~ ihs(wealth_timevote) + law + class)
model2 <- lm(data = suffrage, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct)
model3 <- lm(data = suffrage, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)
model4 <- lm(data = fiscal, formula = vote ~ ihs(wealth_timevote) + law + class)
model5 <- lm(data = fiscal, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct)
model6 <- lm(data = fiscal, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)

hoihoi <- list(model1, model2, model3, model4, model5, model6)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(21, 22, 23)
knitr::opts_current$set(label = "ols_separated")
modelsummary(hoihoi, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_separated.tex",
             add_rows = description,
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage and Fiscal Legislation",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as ihs(Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise.")) %>%
  kableExtra::add_header_above(c(" " = 1, "Suffrage Extension" = 3, "Fiscal Legislation" = 3))

saveRDS(fiscal, "./figures/model_ols_data.RDS")
saveRDS(model5, "./figures/model_ols.RDS")


## Endogeneity test
suffrage_iv <- left_join(suffrage, fiscal_iv %>%
                           select(b1_nummer, profdummy3, par_wealth, exp_inherit), 
                         by = "b1_nummer") %>%
  distinct() %>%
  mutate(profdummy3 = profdummy3.y, 
         harnas = if_else((date_of_death - einde_periode)/365 < 2, 1, 0),
         harnas5 = if_else((date_of_death - einde_periode)/365 < 5, 1, 0),
         exp_inherit = exp_inherit.y,
         par_wealth = par_wealth.y)

datasets2 <- bind_rows(fiscal_iv, suffrage_iv) %>%
  filter(category == "fisc_iv" | category == "suffrage", 
         house == "Tweede Kamer",
         class != "neutral")

model1 <- lm(formula = vote ~ ihs(wealth_timevote) + harnas + ihs(wealth_timevote):harnas + law + class, 
             data = datasets2)
model2 <- lm(formula = vote ~ ihs(wealth_timevote) + harnas + ihs(wealth_timevote):harnas + law + class +
               strikes+tvs+turnout+ncm+tenure+rk_pct, 
             data = datasets2)
model3 <- lm(formula = vote ~ ihs(wealth_timevote) + harnas + ihs(wealth_timevote):harnas + law + class +
               strikes+tvs+turnout+ncm+tenure+rk_pct, 
             data = suffrage_iv)
model4 <- lm(formula = vote ~ ihs(wealth_timevote) + harnas + ihs(wealth_timevote):harnas + law + class +
               strikes+tvs+turnout+ncm+tenure+rk_pct+percentage_aangesl, 
             data = suffrage_iv)
model5 <- lm(formula = vote ~ ihs(wealth_timevote) + harnas + ihs(wealth_timevote):harnas + law + class +
                         strikes+tvs+turnout+ncm+tenure+rk_pct, 
                       data = fiscal_iv)
model6 <-lm(formula = vote ~ ihs(wealth_timevote) + harnas + ihs(wealth_timevote):harnas + law + class +
              strikes+tvs+turnout+ncm+tenure+rk_pct+percentage_aangesl, 
            data = fiscal_iv)
bido <- list(model1, model2, model3, model4, model5, model6)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

attr(description, 'position') <- c(25, 26, 27)
knitr::opts_current$set(label = "ols_endogeneity")
modelsummary(bido, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_endogeneity.tex",
             add_rows = description,
             title = "Endogeneity Test for Suffrage Extension and Fiscal Legislation",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as ihs(Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise.")) %>%
  kableExtra::add_header_above(c(" " = 1, "Pooled" = 2, "Suffrage" = 2, "Fiscal" = 2))

saveRDS(model6, "./figures/model_endog_ols.RDS")
saveRDS(fiscal_iv, "./figures/model_endog_ols_data.RDS")
## Iv results for suffrage - null results

suffrage_iv <- left_join(suffrage, fiscal_iv %>%
                           select(b1_nummer, profdummy3), by = "b1_nummer") %>%
  distinct() %>%
  mutate(profdummy3 = profdummy3.y, percentage_aangesl = percentage_aangesl*100)

fs1 <- lm(data = suffrage_iv, formula = ihs(wealth_timevote) ~ profdummy3 + law + class)
iv1 <- ivreg(data = suffrage_iv, formula = vote ~ ihs(wealth_timevote) + law + class | profdummy3 + law + class)
fs2 <- update(fs1, . ~ . + tvs  + turnout + ncm + tenure + rk_pct)
iv2 <- update(iv1, . ~ . +  tvs + turnout + ncm + tenure + rk_pct | . + tvs + socialistpercentage + turnout + ncm + tenure + rk_pct)
fs3 <- update(fs2, . ~ . + industry_share + percentage_aangesl)
iv3 <- update(iv2, . ~ . + industry_share  + percentage_aangesl  | . + industry_share  + percentage_aangesl )

ivres <- list(fs1, iv1, fs2, iv2, fs3, iv3)

fstats <- ivres[c(2,4,6)] %>%
  map_dbl(.f = ~ summary(.x) %>%
            .$diagnostics %>%
            .[1,3]) %>%
  round(2) %>%
  as.character()

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
  "Kleibergen-Paap F Stat.", "", fstats[1], "", fstats[2], "", fstats[3])
attr(description, 'position') <- c(23,24)

knitr::opts_current$set(label = "ivresults_suffrage")
modelsummary(ivres, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC3",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_suffrage.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Suffrage Extensions",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as ihs(Wealth at Time of Vote), and instrumented by Fathers profession.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 3))) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

## Iv results - profdummy 3 - fiscal 
fs1 <- lm(data = fiscal_iv %>%
            filter(class != "neutral"),
          formula = ihs(wealth_timevote) ~ profdummy3 + class + law)
iv1 <- ivreg(data = fiscal_iv %>%
               filter(class != "neutral"), 
             formula = vote ~ ihs(wealth_timevote) + class + law | profdummy3 + class + law)
fs2 <- update(fs1, . ~ . + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure)
iv2 <- update(iv1, . ~ . + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure | . + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure)
fs3 <- update(fs2, . ~ . + industry_share)
iv3 <- update(iv2, . ~ . + industry_share | . + industry_share)

ivresults <- list(fs1, iv1, fs2, iv2, fs3, iv3)

fstats <- ivresults[c(2,4,6)] %>%
  map_dbl(.f = ~ summary(.x) %>%
            .$diagnostics %>%
            .[1,3]) %>%
  round(2) %>%
  as.character()

compute_delta(fiscal_iv %>% filter(class != "neutral"), 
              main_iv = "wealth_timevote", 
              other_ivs = c("rk_pct", "tvs", "socialistpercentage","tenure", "turnout", "ncm", "industry_share"),
              instrument = "profdummy3", 
              dv = "vote", 
              trans_iv = ihs, 
              prtlout = c("law", "class"),
              first_stage = "small", 
              R2max = 0.75)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
  "Kleibergen-Paap F Stat.", "", fstats[1], "", fstats[2], "", fstats[3])

attr(description, 'position') <- c(23,24)
knitr::opts_current$set(label = "ivresults_fisc")
modelsummary(ivresults, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC3",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_fisc.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as ihs(Wealth at Time of Vote), and instrumented by Fathers profession.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 3))) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))


saveRDS(iv2, "./figures/model_iv2.RDS")
saveRDS(fiscal_iv, "./figures/model_iv2_data.RDS")

## Inheritance results - fiscal 
fs1 <- lm(data = fiscal_iv %>%
            mutate(exp_inherit = exp_inherit/100000) %>%
            filter(class != "neutral"),
          formula = ihs(wealth_timevote) ~ exp_inherit + class + law)
iv1 <- ivreg(data = fiscal_iv %>%
               filter(class != "neutral"), 
             formula = vote ~ ihs(wealth_timevote) + class + law | exp_inherit + class + law)

fs2 <- update(fs1, . ~ .  + tvs + socialistpercentage + turnout + ncm + tenure + rk_pct)
iv2 <- update(iv1, . ~ . + tvs + socialistpercentage + turnout + ncm + tenure + rk_pct | . + tvs + socialistpercentage + turnout + ncm + tenure + rk_pct)
fs3 <- update(fs2, . ~ . + agricul_share + percentage_aangesl)
iv3 <- update(iv2, . ~ . + agricul_share + percentage_aangesl | . + agricul_share + percentage_aangesl)

ivresults <- list(fs1, iv1, fs2, iv2, fs3, iv3)

fstats <- ivresults[c(2,4,6)] %>%
  map_dbl(.f = ~ summary(.x) %>%
            .$diagnostics %>%
            .[1,3]) %>%
  round(2) %>%
  as.character()

compute_delta(fiscal_iv %>% filter(class != "neutral"), 
              main_iv = "wealth_timevote", 
              other_ivs = c("rk_pct", "tvs", "socialistpercentage","tenure", "turnout", "ncm", "percentage_aangesl"),
              instrument = "exp_inherit", 
              dv = "vote", 
              trans_iv = ihs, 
              prtlout = c("law", "class"),
              first_stage = "small", 
              R2max = 0.60)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
  "Kleibergen-Paap F Stat.", "", fstats[1], "", fstats[2], "", fstats[3])

attr(description, 'position') <- c(25, 26)
knitr::opts_current$set(label = "ivresults_fisc_inherit")
modelsummary(ivresults, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_fisc_inherit.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as ihs(Wealth at Time of Vote), and instrumented by Exp. Inheritance.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 3))) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

saveRDS(iv2, "./figures/model_inheritance_iv.RDS")
saveRDS(fiscal_iv, "./figures/model_inheritance_iv_data.RDS")

## selection results
datasets2 <- datasets2 %>%
  mutate(observed = if_else(!is.na(wealth_timevote), 1, 0),
         age_of_death = age_of_death/365,
         agricul_share = agricul_share*100,
         industry_share = industry_share*100)

model1 <- lm(data = datasets2, observed ~ law + class)
model2 <- update(model1, . ~ . + harnas + strikes + tvs + age_of_vote + turnout + tenure + ncm)
model3 <- update(model2, . ~ . + rk_pct + percentage_aangesl)
model4 <- lm(data =datasets2 %>%
               filter(category == "suffrage"),
             observed ~ law + class + strikes+ harnas+tvs+age_of_vote+turnout+tenure+ncm)
model5 <- update(model4, . ~ . + strikes+rk_pct+agricul_share+percentage_aangesl)
model6 <- lm(data = datasets2 %>%
               filter(category == "fisc_iv"), 
             observed ~ law + class + strikes+harnas+tvs+age_of_vote+turnout+tenure+ncm)
model7 <- update(model6, . ~ . + strikes+rk_pct+agricul_share+percentage_aangesl)

selection <- list(model1, model2, model3, model4, model5, model6, model7)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, 
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

attr(description, 'position') <- c(25, 26, 27)
knitr::opts_current$set(label = "ols_selection")
modelsummary(selection, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_selection.tex",
             add_rows = description,
             title = "Selection Equations for Suffrage Extension and Fiscal Legislation",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. The dependent variable is 1 if wealth observed, 0 otherwise.")) %>%
  kableExtra::add_header_above(c(" " = 1, "Pooled" = 3, "Suffrage" = 2, "Fiscal" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))


# logit results - suffrage
library(survival)
model1 <- clogit(formula = vote ~ ihs(wealth_timevote) + strata(law) + strata(class), data = suffrage)
model2 <- update(model1, . ~ . + strikes + tvs + turnout + ncm + tenure)
model3 <- update(model2, . ~ . + rk_pct + percentage_aangesl)
model4 <- clogit(formula = vote ~ ihs(wealth_timevote) + strata(law) + strata(class), data = fiscal)
model5 <- update(model4, . ~ . + strikes + tvs + turnout + ncm + tenure)
model6 <- update(model5, . ~ . + rk_pct + percentage_aangesl)

modelz <- list(model1, model2, model3, model4, model5, model6)

gma <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared","$R^2$", 2,
  "r.squared.max", "Max. $R^2$", 2)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, 
  "Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

attr(description, 'position') <- c(17,18)
knitr::opts_current$set(label = "logit_suffrage_fiscal")
modelsummary(modelz, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gma,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/logit_suffrage_fiscal.tex",
             add_rows = description,
             title = "Logit Analysis of Suffrage Extension and Fiscal Legislation",
             notes = list("Standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional.",
                          "The dependent variable, Vote, is defined as 1 if the politician is in favor of the reform, 0 otherwise.")) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::add_header_above(c(" " = 1, "Suffrage" = 3, "Fiscal" = 3))

## Results with NW0 instead of wealth_timevote - fiscal results
ols1 <- lm(data = fiscal, formula = vote ~ ihs(nw0) + law + class)
ols2 <- lm(data = fiscal, formula = vote ~ ihs(nw0) + law + class + 
             strikes+tvs+socialistpercentage +turnout+ncm+tenure+rk_pct)
ols3 <- lm(data = fiscal, formula = vote ~ ihs(nw0) + law + class + 
             strikes+tvs+socialistpercentage+turnout+ncm+tenure+rk_pct + percentage_aangesl)
iv1 <-ivreg(data = fiscal_iv %>%
              filter(class != "neutral"), 
            formula = vote ~ ihs(nw0) + class + law | profdummy3 + class + law)
iv2 <- update(iv1, . ~ . + strikes + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure | . + strikes + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure)
iv3 <- update(iv2, . ~ . + percentage_aangesl| . + percentage_aangesl)

modelletjes <- list(ols1, ols2, ols3, iv1, iv2, iv3)

fstats <- modelletjes[c(4,5,6)] %>%
  map_dbl(.f = ~ summary(.x) %>%
            .$diagnostics %>%
            .[1,3]) %>%
  round(2) %>%
  as.character()

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
  "Kleibergen-Paap F Stat.", " ", " ", " ", fstats[1], fstats[2], fstats[3])

attr(description, 'position') <- c(23,24)
knitr::opts_current$set(label = "fisc_nw0_ols_iv")

modelsummary(modelletjes, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_iv_fisc_nw0.tex",
             add_rows = description,
             title = "IV Analysis of Fiscal Legislation - Robustness Check",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as ihs(Wealth at Death), and instrumented by Exp. Inheritance.",
                          "The reference political allegiance is confessional.",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::add_header_above(c(" " = 1, "OLS" = 3, "IV" = 3))



## Results with log wealth instead of ihs
fs1 <- lm(data = fiscal_iv,
          formula = log(1+wealth_timevote) ~ profdummy3 + class + law)
iv1 <- ivreg(data = fiscal_iv %>%
               filter(class != "neutral"), 
             formula = vote ~ log(1+wealth_timevote) + class + law | profdummy3 + class + law)
fs2 <- update(fs1, . ~ . + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure)
iv2 <- update(iv1, . ~ . + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure | . + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure)
fs3 <- update(fs2, . ~ . + industry_share)
iv3 <- update(iv2, . ~ . + industry_share | . + industry_share)

ivresults <- list(fs1, iv1, fs2, iv2, fs3, iv3)

fstats <- ivresults[c(2,4,6)] %>%
  map_dbl(.f = ~ summary(.x) %>%
            .$diagnostics %>%
            .[1,3]) %>%
  round(2) %>%
  as.character()

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
  "Kleibergen-Paap F Stat.", "", fstats[1], "", fstats[2], "", fstats[3])

attr(description, 'position') <- c(23,24)
knitr::opts_current$set(label = "ivresults_fisc_log")
modelsummary(ivresults, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC3",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_fisc_log.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Time of Vote), and instrumented by Fathers profession.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 3))) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))


saveRDS(iv2, "./figures/model_iv2_log.RDS")
saveRDS(fiscal_iv, "./figures/model_iv2_log_data.RDS")


## Results of placebo test with government intervention
polfams <- read_csv("./data/polid_data/political_families.csv")
govtint <- govtint %>% left_join(polfams) %>%
  rename(count_polfam = n)

ols_begin <- lm(data = govtint, vote ~ ihs(wealth_timevote) + class + law)
ols_n <- lm(data = govtint, vote ~ ihs(wealth_timevote) + count_polfam + class + law)
ols_polfam <- lm(data = govtint, vote ~ ihs(wealth_timevote) + polfam + class + law)
ols2_n <- lm(data = govtint, vote ~ ihs(wealth_timevote) + count_polfam + strikes + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure + industry_share + class + law)
ols2_polfam <- lm(data = govtint, vote ~ ihs(wealth_timevote) + polfam + strikes + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure + industry_share + class + law)

govtint_iv <- left_join(govtint, fiscal_iv %>%
            select(b1_nummer, profdummy3, par_wealth, exp_inherit), 
          by = "b1_nummer") %>%
  distinct() %>%
  mutate(profdummy3 = profdummy3.y, 
         exp_inherit = exp_inherit.y,
         par_wealth = par_wealth.y)
  
iv_profdummy3 <- ivreg(data = govtint_iv,
                   formula = vote ~ ihs(wealth_timevote) + strikes + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure + industry_share + class + law | profdummy3 +strikes + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure + industry_share + class + law) 
iv_polfam <- ivreg(data = govtint_iv,
                    formula = vote ~ ihs(wealth_timevote) + strikes + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure + industry_share + class + law | polfam + strikes + rk_pct + tvs + socialistpercentage + turnout + ncm + tenure + industry_share + class + law)

results <- list(ols_begin, ols_n, ols_polfam, ols2_n,ols2_polfam, iv_profdummy3, iv_polfam)


fstats <- results[c(6,7)] %>%
  map_dbl(.f = ~ summary(.x) %>%
            .$diagnostics %>%
            .[1,3]) %>%
  round(2) %>%
  as.character()

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
  "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Kleibergen-Paap F Stat.", "", "", "", "", "", fstats[1], fstats[2])

attr(description, 'position') <- c(25,26)
knitr::opts_current$set(label = "govtint_results")
modelsummary(results, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/govtint_results.tex",
             title = "OLS and IV Estimates of Wealth on the Propensity to Vote for Gov't Intervention",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as ihs(Wealth at Time of Vote).",
                          "Personal Wealth is instrumented by Father Politician (Model 6) and Political Family (Model 7).",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  add_header_above(c(" " = 1, "-" = 1, "Count" =1 , "Dummy" = 1, "Count" = 1, "Dummy" = 1, "-" = 1, "Dummy" =1)) %>%
  add_header_above(c(" " = 1, "OLS" = 5, "IV" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))
