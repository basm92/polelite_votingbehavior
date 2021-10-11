## New table-creating file
library(ivreg); library(modelsummary); library(tidyverse); library(stargazer)
library(xtable) ; library(kableExtra); library(viridis)

# Common parameters for the models
coefconvert <- c(
    "profdummy3" = "Father Politician",
    "log(1 + wealth_timevote)" = "Personal Wealth",
    "harnasTRUE" = "Died W 2 Yrs",
    "log(1 + wealth_timevote):harnasTRUE" = "Personal Wealth x Died W 2 Yrs",
    "strikes" = "Number of Strikes",
    "tvs" = "Vote Share",
    "age_of_vote" = "Age at Time of Vote",
    "turnout" = "Turnout",
    "ncm" = "Margin to Nearest Competitor",
    "tenure" = "Tenure",
    "long_elec_horiz" = "Long Electoral Horizon",
    "age_of_entrance" = "Age at Entry",
    "socialistdum" = "Competed Against Socialist",
    "socialistpercentage" = "Share Socialist Vote in District",
    "rk_pct" = "Share Catholic",
    "hervormd_pct" = "Share Protestant (Hervormd)",
    "gereformeerd_pct" = "Share Protestant (Geref.)",
    "agricul_share" = "Share District in Agriculture", 
    "industry_share" = "Share District in Industry",
    "services_share" = "Share District in Services",
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
    filter(category == "fisc", law != "Staatsschuldwet 1914")
suffrage <- datasets %>%
    filter(category == "suffrage")
govtint <- datasets %>%
    filter(category == "govtint")
fiscal_iv <- datasets %>%
    filter(category == "fisc_iv", law != "Staatsschuldwet 1914")

## Baseline OLS suffrage
model1 <- lm(data = suffrage %>%
                 filter(house == "Tweede Kamer", class != "neutral") %>%
                 mutate(tenure = tenure/365),
             formula = vote ~ wealth_timevote + class + law)
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + tvs)
model4 <- update(model3, . ~ . + age_of_vote)
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
attr(description, 'position') <- c(21,22,23)

knitr::opts_current$set(label = "baseline_ols_suffrage")
modelsummary(electorallaw_ols, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/electorallaw_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage Expansion",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Time of Vote).",
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
model4 <- update(model3, . ~ . + turnout)
model5 <- update(model4, . ~ . + ncm)
model6 <- update(model5, . ~ . + tenure)
model7 <- update(model6, . ~ . + socialistpercentage)
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
             output = "./tables/socialred_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Government Intervention",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

## Baseline OLS Suffrage + Government intervention in one table
model1 <- lm(data = suffrage %>%
                 filter(house == "Tweede Kamer", class != "neutral"),
             formula = vote ~ log(1+wealth_timevote) + class + law)
model2 <- update(model1, . ~ . + strikes + tvs + age_of_vote + turnout +
                     ncm + tenure + rk_pct)
model3 <- update(model2, . ~ . + agricul_share)

model4 <- lm(data = govtint %>%
                 filter(house == "Tweede Kamer", class != "neutral"),
             formula = vote ~ log(1+wealth_timevote) + class + law)
model5 <- update(model4, . ~ . + strikes + tvs + age_of_vote + turnout +
                     ncm + tenure + rk_pct)
model6 <- update(model5, . ~ . + agricul_share)

suffrage_gi_together <- list("(1)" = model1, 
                         "(2)" = model2, 
                         "(3)" = model3, 
                         "(4)" = model4, 
                         "(5)" = model5, 
                         "(6)" = model6
)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, 
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(23,24,25)

knitr::opts_current$set(label = "suffrage_govtint_together")

modelsummary(suffrage_gi_together, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/suffrage_gi_together.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    add_header_above(c(" " = 1, "Suffrage Extension" = 3, "Gov't Intervention" = 3)) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))


## baseline ols fiscal

model1 <- lm(data = fiscal %>%
                 filter(house == "Tweede Kamer", class != "neutral") %>%
                 mutate(tenure = tenure/365),
             formula = vote ~ log(1+wealth_timevote) + class + law)
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + tvs)
model4 <- update(model3, . ~ . + turnout)
model5 <- update(model4, . ~ . + ncm)
model6 <- update(model5, . ~ . + tenure)
model7 <- update(model6, . ~ . + socialistpercentage)
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
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Time of Vote).",
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
    "Law Fixed Effects", "No", "No", "No", "No", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(7,8,9)

gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2)

knitr::opts_current$set(label = "baseline_ols")
modelsummary(first_regs, 
             vcov = "HC0",
             coef_omit = "Intercept|law",
             coef_map = c("log(1 + wealth_timevote)" = "Personal Wealth",
                          "classliberal" = "Liberal",
                          "classsocialist" = "Socialist"),
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             out = "kableExtra",
             add_rows = description,
             #output = "./tables/baseline_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. The reference political allegiance is confessional.",
                          "Personal Wealth is defined as log(1+Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise.")) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

## Harnas

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, ~model8,
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(25, 26, 27)

model1 <- lm(data = fiscal %>%
               # mutate(harnas = (fiscal$date_of_death - fiscal$date)/365 < 3) %>%
                # mutate(harnas = (fiscal$date_of_death - fiscal$einde_periode)/365 < 2) %>%
                filter(house == "Tweede Kamer"), formula = vote ~ log(1+wealth_timevote) + harnas + log(1+wealth_timevote)*harnas + class + law)
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + tvs)
model4 <- update(model3, . ~ . + turnout)
model5 <- update(model4, . ~ . + ncm)
model6 <- update(model5, . ~ . + tenure)
model7 <- update(model6, . ~ . + socialistpercentage)
model8 <- update(model7, . ~ . + rk_pct)

harnas2 <- list("(1)" = model1,
                "(2)" = model2,
                "(3)" = model3,
                "(4)" = model4,
                "(5)" = model5,
                "(6)" = model6,
                "(7)" = model7,
                "(8)" = model8)

knitr::opts_current$set(label = "harnas")
modelsummary(harnas2, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             #output = "./tables/harnas2.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Reforms - Endogeneity Test",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as log(1+Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = c("hold_position","scale_down"))

## IV results
#ivreg - baseline with profdummy3

fs1 <- lm(data = fiscal_iv %>%
                 filter(class != "neutral"),
             formula = log(1+wealth_timevote) ~ profdummy3 + class + law)
iv1 <- ivreg(data = fiscal_iv %>%
                      filter(class != "neutral"),
                  formula = vote ~ log(1+wealth_timevote) + class + law | profdummy3 + class + law)

# All models
fs2 <- update(fs1, . ~ . + strikes + rk_pct)
iv2<- update(iv1, . ~ . + strikes + rk_pct | . + strikes + rk_pct)
fs3 <- update(fs2, . ~ . + tvs + socialistpercentage + turnout + ncm + tenure)
iv3 <- update(iv2, . ~ . +  tvs + socialistpercentage + turnout + ncm + tenure | . + tvs + socialistpercentage + turnout + ncm + tenure)
fs4 <- update(fs3, . ~ . + industry_share)
iv4 <- update(iv3, . ~ . + industry_share | . + industry_share)

ivresults <- list("(1)" = fs1,
                  "(2)" = iv1,
                  "(3)" = fs2,
                  "(4)" = iv2,
                  "(5)" = fs3,
                  "(6)" = iv3,
                  "(7)" = fs4,
                  "(8)" = iv4)

### table
fstats <- ivresults[c(2,4,6,8)] %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,3]) %>%
    round(2) %>%
    as.character()

pvals <- ivresults %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,4])

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, ~model8,
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", #,
    "F-Stat. First stage", "", fstats[1], "", fstats[2], "", fstats[3], "", fstats[4])

gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2)

# new coefmap
coefconvert <- c(
    "exp_inherit" = "Expected Inheritance",
    "profdummy3" = "Father Politician",
    "log(1 + wealth_timevote)" = "Personal Wealth",
    "harnasTRUE" = "Died W 2 Yrs",
    "log(1 + wealth_timevote):harnasTRUE" = "Personal Wealth x Died W 2 Yrs",
    "strikes" = "Number of Strikes",
    "socialistdum" = "Competed Against Socialist",
    "socialistpercentage" = "Share Socialist Vote in District",
    "rk_pct" = "Share Catholic",
    "hervormd_pct" = "Share Protestant (Hervormd)",
    "gereformeerd_pct" = "Share Protestant (Geref.)",
    "tvs" = "Vote Share",
    "age_of_vote" = "Age at Time of Vote",
    "turnout" = "Turnout",
    "ncm" = "Margin to Nearest Competitor",
    "tenure" = "Tenure",
    "long_elec_horiz" = "Long Electoral Horizon",
    "age_of_entrance" = "Age at Entry",
    "agricul_share" = "Share District in Agriculture", 
    "industry_share" = "Share District in Industry",
    "services_share" = "Share District in Services",
    "aandeel_gem" = "Share District in Tot. Taxes",
    "percentage_aangesl" = "Share Tax Liable in District",
    "classliberal" = "Liberal",
    "classsocialist" = "Socialist"
)

attr(description, 'position') <- c(25,26,27)

knitr::opts_current$set(label = "ivresults")
modelsummary(ivresults, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC3",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             #output = "./tables/iv_results.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death), and instrumented by Fathers profession.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 4))) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

## Iv results other

fs1 <- lm(data = fiscal_iv %>%
              filter(class != "neutral"),
          formula = log(1+wealth_timevote) ~ profdummy3 + class + law)
iv1 <- ivreg(data = fiscal_iv %>%
                 filter(class != "neutral"),
             formula = vote ~ log(1+wealth_timevote) + class + law | profdummy3 + class + law)

fs2 <- update(fs1, . ~ . + hervormd_pct + strikes)
iv2 <- update(iv1, . ~ . + hervormd_pct + strikes | . + hervormd_pct + strikes)
fs3 <- update(fs2, . ~ . + tvs + socialistdum + age_of_vote + turnout + ncm)
iv3 <- update(iv2, . ~ . + tvs + socialistdum + age_of_vote + turnout + ncm | . + tvs + socialistdum + age_of_vote + turnout + ncm)
fs4 <- update(fs3, . ~ . + agricul_share)
iv4 <- update(iv3, . ~ . + agricul_share | . + agricul_share)

ivresults2 <- list("(1)" = fs1,
                  "(2)" = iv1,
                  "(3)" = fs2,
                  "(4)" = iv2,
                  "(5)" = fs3,
                  "(6)" = iv3,
                  "(7)" = fs4,
                  "(8)" = iv4)

### table
fstats <- ivresults2[c(2,4,6,8)] %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,3]) %>%
    round(2) %>%
    as.character()

pvals <- ivresults2 %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,4])

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, ~model8,
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", #,
    "F-Stat. First stage", "", fstats[1], "", fstats[2], "", fstats[3], "", fstats[4])

gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2)

attr(description, 'position') <- c(25,26,27)

knitr::opts_current$set(label = "ivresults2")
modelsummary(ivresults2, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC3",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
          #   output = "./tables/iv_results2.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death), and instrumented by Fathers profession.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 4))) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))


## Iv results inheritance
fs1 <- lm(data = fiscal_iv %>%
              mutate(exp_inherit = exp_inherit/100000) %>%
              filter(class != "neutral"),
          formula = log(1+wealth_timevote) ~ exp_inherit + class + law)
iv1 <- ivreg(data = fiscal_iv %>%
                 filter(class != "neutral"), 
             formula = vote ~ log(1+wealth_timevote) + class + law | exp_inherit + class + law)

fs2 <- update(fs1, . ~ . + strikes + rk_pct)
iv2<- update(iv1, . ~ . + strikes + rk_pct | . + strikes + rk_pct)
fs3 <- update(fs2, . ~ . + tvs + socialistpercentage + turnout + ncm + tenure)
iv3 <- update(iv2, . ~ . +  tvs + socialistpercentage + turnout + ncm + tenure | . + tvs + socialistpercentage + turnout + ncm + tenure)
fs4 <- update(fs3, . ~ . + industry_share)
iv4 <- update(iv3, . ~ . + industry_share | . + industry_share)

ivresults <- list("(1)" = fs1,
                  "(2)" = iv1,
                  "(3)" = fs2,
                  "(4)" = iv2,
                  "(5)" = fs3,
                  "(6)" = iv3,
                  "(7)" = fs4,
                  "(8)" = iv4)

fstats <- ivresults[c(2,4,6,8)] %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,3]) %>%
    round(2) %>%
    as.character()

pvals <- ivresults %>%
    map_dbl(.f = ~ summary(.x) %>%
                .$diagnostics %>%
                .[1,4])

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, ~model8,
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", #,
    "F-Stat. First stage", "", fstats[1], "", fstats[2], "", fstats[3], "", fstats[4])


attr(description, 'position') <- c(25,26,27)

knitr::opts_current$set(label = "iv_results_inheritance")
modelsummary(ivresults, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC0",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             #output = "./tables/iv_results_exp_inherit.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death), and instrumented by Expected inheritance.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
                          
             )) %>%
    add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 4))) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))


## Make simulation predictions - with any model
modelr::add_predictions(fiscal %>%
                            filter(class != "neutral",
                                   law != "Successiewet 1921", law != "Staatsschuldwet 1914",
                                   wealth_timevote >= 0), model8) %>%
    group_by(law) %>%
    summarize(mean_wealth = mean(wealth_timevote, na.rm = TRUE), mean_prob = mean(pred, na.rm = TRUE))


modelr::add_predictions(fiscal %>%
                            filter(class != "neutral",
                                   law != "Successiewet 1921", law != "Staatsschuldwet 1914",
                                   wealth_timevote >= 0) %>%
                            mutate(wealth_timevote = 5 * wealth_timevote), model8) %>%
    group_by(law) %>%
    summarize(mean_wealth = mean(wealth_timevote, na.rm = TRUE), mean_prob = mean(pred, na.rm = TRUE))

modelr::add_predictions(fiscal_iv %>%
                            filter(class != "neutral",
                                   law != "Successiewet 1921", law != "Staatsschuldwet 1914"), iv3) %>%
    group_by(law) %>%
    summarize(mean_wealth = mean(wealth_timevote, na.rm = TRUE), mean_prob = mean(pred, na.rm = TRUE), var_prob = var(pred, na.rm = TRUE))


modelr::add_predictions(fiscal_iv %>%
                            filter(class != "neutral",
                                   law != "Successiewet 1921") %>%
                            mutate(wealth_timevote = 100000 +  wealth_timevote), iv3) %>%
    group_by(law) %>%
    summarize(mean_wealth = mean(wealth_timevote, na.rm = TRUE), mean_prob = mean(pred, na.rm = TRUE))

