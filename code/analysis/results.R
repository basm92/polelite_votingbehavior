## New table-creating file
library(ivreg); library(modelsummary); library(tidyverse); library(stargazer)
library(xtable) ; library(kableExtra); library(viridis)

# Common parameters for the models
coefconvert <- c(
    "log(1 + wealth_timevote)" = "Personal Wealth",
    "strikes" = "Number of Strikes",
    "tvs" = "Vote Share",
    "socialistdum" = "Competed Against Socialist",
    "socialistpercentage" = "% Socialist Vote in District",
    "turnout" = "Turnout",
    "ncm" = "Margin to Nearest Competitor",
    "tenure" = "Tenure",
    "long_elec_horiz" = "Long Electoral Horizon",
    "age_of_vote" = "Age at Time of Vote",
    "age_of_entrance" = "Age at Entry",
    "rk_pct" = "% Catholic",
    "hervormd_pct" = "% Protestant (Hervormd)",
    "gereformeerd_pct" = "% Protestant (Geref.)",
    "agricul_share" = "% District in Agriculture", 
    "industry_share" = "% District in Industry",
    "services_share" = "% District in Services",
    "aandeel_gem" = "Share District in Tot. Taxes",
    "percentage_aangesl" = "Share Tax Liable in District",
    "classliberal" = "Liberal",
    "classsocialist" = "Socialist"
)

gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2)

## Load datasets
fiscal <- readRDS("./data/datasets/fiscal_lowerandupper.RDS") %>%
    mutate(category = "fisc")
fiscal_iv <- readRDS("./data/datasets/fiscal_ivdata.RDS") %>%
    mutate(category = "fisc_iv")
suffrage <- readRDS("./data/datasets/electoral_lower.RDS") %>%
    mutate(category = "suffrage")
govtint <- readRDS("./data/datasets/social_lower.RDS") %>%
    mutate(category = "govtint")

## Mutate couple of vars
datasets <- list(suffrage, fiscal, fiscal_iv, govtint)

datasets <- purrr::map_df(datasets, ~ .x %>%
                              mutate(across(everything(), ~ as.character(.x)))) %>%
    type_convert()
# Now mutate the variables in this dataframe and then write them back to original dataframe
datasets <- datasets %>%
    mutate(
        tenure = tenure/365,
    long_elec_horiz = long_elec_horiz/365,
    age_of_vote = age_of_vote/365,
    age_of_entrance = age_of_entrance/365,
    rk_pct = rk_pct/100,
    hervormd_pct = hervormd_pct/100,
    gereformeerd_pct = gereformeerd_pct/100,
    aandeel_gem = aandeel_gem/100,
    percentage_aangesl = percentage_aangesl/100,
    socialistpercentage = socialistpercentage/100
    )

fiscal <- datasets %>%
    filter(category == "fisc")
suffrage <- datasets %>%
    filter(category == "suffrage")
govtint <- datasets %>%
    filter(category == "govtint")
fiscal_iv <- datasets %>%
    filter(category == "fisc_iv")

## Baseline OLS suffrage
model1 <- lm(data = suffrage %>%
                 filter(house == "Tweede Kamer", class != "neutral") %>%
                 mutate(tenure = tenure/365),
             formula = vote ~ log(1+wealth_timevote) + class + law)
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + tvs)
model4 <- update(model3, . ~ . + socialistpercentage)
model5 <- update(model4, . ~ . + turnout)
model6 <- update(model5, . ~ . + ncm)
model7 <- update(model6, . ~ . + tenure)
model8 <- update(model7, . ~ . + rk_pct)

electorallaw_ols <- list("(1)" = model1, 
                         "(2)" = model2, 
                         "(3)" = model3, 
                         "(4)" = model4, 
                         "(5)" = model5, 
                         "(6)" = model6, 
                         "(7)" = model7,
                         "(8)" = model8
                         )

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,~model8,
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(19, 20, 21)

knitr::opts_current$set(label = "baseline_ols_suffrage")
modelsummary(electorallaw_ols, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             #output = "./tables/electorallaw_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage Expansion",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

## Baseline OLS govt intervention
#### Tweede Kamer
model1 <- lm(data = govtint %>%
                 filter(house == "Tweede Kamer", class != "neutral") %>%
                 mutate(tenure = tenure/365),
             formula = vote ~ log(1+wealth_timevote) + class + law)
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + tvs)
model4 <- update(model3, . ~ . + socialistpercentage)
model5 <- update(model4, . ~ . + turnout)
model6 <- update(model5, . ~ . + ncm)
model7 <- update(model6, . ~ . + tenure)
model8 <- update(model7, . ~ . + rk_pct)

govtint_ols <- list("(1)" = model1, 
                    "(2)" = model2, 
                    "(3)" = model3, 
                    "(4)" = model4, 
                    "(5)" = model5, 
                    "(6)" = model6, 
                    "(7)" = model7,
                    "(8)" = model8)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, ~model8,
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(21,22,23)

knitr::opts_current$set(label = "baseline_ols_govtint")

modelsummary(govtint_ols, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             #output = "./tables/socialred_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Government Intervention",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

## baseline ols fiscal

model1 <- lm(data = fiscal %>%
                 filter(house == "Tweede Kamer", class != "neutral") %>%
                 mutate(tenure = tenure/365),
             formula = vote ~ log(1+wealth_timevote) + class + law)
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + tvs)
model4 <- update(model3, . ~ . + socialistpercentage)
model5 <- update(model4, . ~ . + turnout)
model6 <- update(model5, . ~ . + ncm)
model7 <- update(model6, . ~ . + tenure)
model8 <- update(model7, . ~ . + rk_pct)

fiscal_controls_ols <- list("(1)" = model1, 
                    "(2)" = model2, 
                    "(3)" = model3, 
                    "(4)" = model4, 
                    "(5)" = model5, 
                    "(6)" = model6, 
                    "(7)" = model7,
                    "(8)" = model8)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, ~model8,
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(21,22,23)

knitr::opts_current$set(label = "ols_controls")
modelsummary(fiscal_controls_ols, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             #output = "./tables/fiscal_controls_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Legislation",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = c("hold_position","scale_down"))


## OLS fiscal different houses
baseline <- lm(data = fiscal,
               formula = vote ~ log(1 + wealth_timevote))
model1 <- lm(data = fiscal,
             formula = vote ~ log(1+wealth_timevote) + class)
model2 <- lm(data = fiscal %>%
                 filter(house == "Tweede Kamer"),
             formula = vote ~ log(1+wealth_timevote) + class)
model3 <- lm(data = fiscal %>%
                 filter(house == "Eerste Kamer"),
             formula = vote ~ log(1+wealth_timevote) + class) 
model4 <- lm(data = fiscal,
             formula = vote ~ log(1+wealth_timevote) + class + law)
model5 <- lm(data = fiscal %>%
                 filter(house == "Tweede Kamer"),
             formula = vote ~ log(1+wealth_timevote) + class + law)
model6 <- lm(data = fiscal %>%
                 filter(house == "Eerste Kamer"),
             formula = vote ~ log(1+wealth_timevote) + class + law)

first_regs <- list("(1)" = baseline,
                   "(2)" = model1, 
                   "(3)" = model2, 
                   "(4)" = model3, 
                   "(5)" = model4, 
                   "(6)" = model5, 
                   "(7)" = model6)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
    "House", "Both", "Both", "Tweede Kamer", "Eerste Kamer", "Both", "Tweede Kamer", "Eerste Kamer",
    "Law Dummies", "No", "No", "No", "No", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(7,8,9)

gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2)

knitr::opts_current$set(label = "baseline_ols")
modelsummary(first_regs, 
             vcov = vcovHC,
             coef_omit = "Intercept|law",
             coef_map = c("log(1 + wealth_timevote)" = "Personal Wealth",
                          "classliberal" = "Liberal",
                          "classsocialist" = "Socialist"),
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             out = "kableExtra",
             add_rows = description,
             output = "./tables/baseline_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. The reference political allegiance is confessional.",
                          "Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise.")) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 

## Harnas

coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "harnasTRUE" = "Died Within 2 Years",
                 "log(1 + wealth_timevote):harnasTRUE" = "Wealth x Died Within 2 Years",
                 "strikes" = "Amount of Strikes",
                 "rk_pct" = "Catholics in district",
                 "industry_share" = "Share Industrial",
                 "tvs" = "Vote Share",
                 "socialistdum1" = "Competed Against Socialist",
                 "tenure" = "Tenure",
                 "classliberal" = "Liberal",
                 "classsocialist" = "Socialist"
)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
    "Law Dummies", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(21, 22, 23)

h2model1 <- lm(data = fiscal %>%
                   filter(house == "Tweede Kamer"), formula = vote ~ log(1+wealth_timevote) + harnas + log(1+wealth_timevote)*harnas + class + law)
h2model2 <- update(h2model1, . ~ . + strikes)
h2model3 <- update(h2model2, . ~ . + rk_pct)
h2model4 <- update(h2model3, . ~ . + industry_share)
h2model5 <- update(h2model3, . ~ . + tvs)
h2model6 <- update(h2model5, . ~ . + turnout)
h2model7 <- update(h2model6, . ~ . + tenure)

harnas2 <- list("(1)" = h2model1,
                "(2)" = h2model2,
                "(3)" = h2model3,
                "(4)" = h2model4,
                "(5)" = h2model5,
                "(6)" = h2model6,
                "(7)" = h2model7)

knitr::opts_current$set(label = "harnas")
modelsummary(harnas2, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/harnas2.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Reforms - Endogeneity Test",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 

## IV results
#ivreg - baseline with profdummy3
baseline <- ivreg(data = fiscal_iv %>%
                      mutate(tenure = tenure/365,
                             age_of_vote = age_of_vote/365) %>%
                      filter(class != "neutral"),
                  formula = vote ~ log(1+wealth_timevote) + class + law | profdummy3 + class + law)

# All models
model2 <- update(baseline, . ~ . + strikes | . + strikes)
model3 <- update(model2, . ~ . + rk_pct | . + rk_pct)
model4 <- update(model3, . ~ . + industry_share | . + industry_share)
model5 <- update(model3, . ~ . + tvs | . + tvs)
model6 <- update(model5, . ~ . + turnout | . + turnout)
model7 <- update(model6, . ~ . + tenure | . + tenure)

ivresults <- list("(1)" = baseline,
                  "(2)" = model2,
                  "(3)" = model3,
                  "(4)" = model4,
                  "(5)" = model5,
                  "(6)" = model6,
                  "(7)" = model7)

### table
gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2,
)

fstats <- ivresults %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,3]) %>%
    round(2) %>%
    as.character()

pvals <- ivresults %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,4])

coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "strikes" = "Amount of Strikes",
                 "rk_pct" = "Catholics in district",
                 "industry_share" = "Share Industrial",
                 "tvs" = "Vote Share",
                 "tenure" = "Tenure",
                 "turnout" = "Turnout",
                 "classsocialist" = "Socialist",
                 "classliberal" = "Liberal"
                 )

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
    "Law Dummies", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "F-Stat. First stage", fstats[1], fstats[2], fstats[3], fstats[4], fstats[5], fstats[6], fstats[7])

attr(description, 'position') <- c(20,21,22)

knitr::opts_current$set(label = "ivresults")
modelsummary(ivresults, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death), and instrumented by Fathers profession.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 

## Iv results other

model13 <- update(baseline, . ~ . + agricul_share | . + agricul_share)
model14 <- update(baseline, . ~ . + age_of_vote | . + age_of_vote)
model15 <- update(model14, . ~ . + hervormd_pct | . + hervormd_pct)
model16 <- update(model15, . ~ . + ncm | . + ncm)
model17 <- update(model16 , . ~ . + turnout | . + turnout)
model18 <- update(model17, . ~ . + tvs | . + tvs)
model19 <- update(model18, . ~ . + socialistpercentage | . + socialistpercentage)

iv_results2 <- list("(1)" = baseline,
                     "(2)" = model13,
                     "(3)" = model14,
                     "(4)" = model15,
                     "(5)" = model17,
                     "(6)" = model18,
                     "(7)" = model19)

### table
gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2,
)

fstats <- iv_results2 %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,3]) %>%
    round(2) %>%
    as.character()

pvals <- iv_results2 %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,4])

coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "agricul_share" = "Share Agricultural",
                 "age_of_vote" = "Age of Vote",
                 "hervormd_pct" = "Dutch Reformed in district",
                 "ncm" = "Nearest Competitor Margin",
                 "turnout" = "Turnout",
                 "tvs" = "Vote Share",
                 "socialistpercentage" = "Percentage Socialist Vote",
                 "classsocialist" = "Socialist",
                 "classliberal" = "Liberal"
)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
    "Law Dummies", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "F-Stat. First stage", fstats[1], fstats[2], fstats[3], fstats[4], fstats[5], fstats[6], fstats[7])

attr(description, 'position') <- c(21,22,23)

knitr::opts_current$set(label = "ivresults2")
modelsummary(iv_results2, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results2.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death), and instrumented by Fathers profession.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 


## Iv results inheritance

baseline <- ivreg(data = fiscal_iv %>%
                      mutate(#wealth_timevote = wealth_timevote / 100000,
                          tenure = tenure/10000), 
                  formula = vote ~ log(1+wealth_timevote) + class + law | exp_inherit + class + law)

# All models
model2 <- update(baseline, . ~ . + strikes | . + strikes)
model3 <- update(model2, . ~ . + rk_pct | . + rk_pct)
model4 <- update(model3, . ~ . + industry_share | . + industry_share)
model5 <- update(model3, . ~ . + tvs | . + tvs)
model6 <- update(model5, . ~ . + turnout | . + turnout)
model7 <- update(model6, . ~ . + tenure | . + tenure)

iv_results_inheritance <- list("(1)" = baseline,
                  "(2)" = model2,
                  "(3)" = model3,
                  "(4)" = model4,
                  "(5)" = model5,
                  "(6)" = model6,
                  "(7)" = model7)

fstats <- ivresults %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,3]) %>%
    round(2) %>%
    as.character()

pvals <- ivresults %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,4])

coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "strikes" = "Amount of Strikes",
                 "rk_pct" = "Catholics in district",
                 "industry_share" = "Share Industrial",
                 "tvs" = "Vote Share",
                 "turnout" = "Electoral Turnout",
                 "tenure" = "Tenure",
                 "classliberal" = "Liberal",
                 "classsocialist" = "Socialist"
)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
    "Law Dummies", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
    "F-Stat. First stage", fstats[1], fstats[2], fstats[3], fstats[4], fstats[5], fstats[6], fstats[7])

attr(description, 'position') <- c(19, 20, 21)

knitr::opts_current$set(label = "iv_results_inheritance")
modelsummary(iv_results_inheritance, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC2",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_exp_inherit.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death), and instrumented by Expected inheritance.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
                          
             )) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 



