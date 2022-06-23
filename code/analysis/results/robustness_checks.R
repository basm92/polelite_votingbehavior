# some additional robustness checks
# new_results
source("./code/analysis/results/get_data.R")
source("./code/analysis/results/oster_selection_statistics.R")


# check 1: other party classification
rough_classific <- readxl::read_xlsx("./data/polid_data/tk_1815tot1950uu.xlsx") %>%
    janitor::clean_names() %>% 
    select(b1_nummer, partij_en_fractie_s)

datasets <- datasets %>%
    left_join(rough_classific)

datasets_rob <- datasets %>%
    mutate(new_classific = case_when(
        stringr::str_detect(partij_en_fractie_s, 'liberaal|Liberaal|Lib|lib|Thorb|Takk') ~ "liberal",
        stringr::str_detect(partij_en_fractie_s, "RK|Rooms|Katholiek|katholiek|kath|conservatief (katholiek)|RKSP|Schaep|Bahl") ~ "catholic",
        stringr::str_detect(partij_en_fractie_s, "Prot|ARP|CHU|Anti-Rev|anti|cons|AR|Christ|christ|a.r.") ~ "protestant",
        stringr::str_detect(partij_en_fractie_s, "VDB|Vrije Demo|Socia|Soc|Comm|SDAP|Arbeid|Radi|vrijz|Vrijz|CPN|CPH") ~ "socialist",
        TRUE ~ NA_character_
    ))

## OLS Models - for both
model1 <- datasets_rob %>%
    filter(category == "fisc" | category == "suffrage", 
           house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ law + new_classific)
model2 <- datasets_rob %>%
    filter(category == "fisc" | category == "suffrage", house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ ihs(wealth_timevote) + law + new_classific)
model3 <- datasets_rob %>%
    filter(category == "fisc" | category == "suffrage", house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ ihs(wealth_timevote):category + law + new_classific)
model4 <- lm(data = datasets_rob %>% filter(category == "suffrage", house == "Tweede Kamer"), formula = vote ~ ihs(wealth_timevote) + law + new_classific + 
                 strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)
model5 <- lm(data = datasets_rob %>% filter(category == "fisc", house == "Tweede Kamer"), formula = vote ~ ihs(wealth_timevote) + law + new_classific + 
                 strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)

ols_pooled <- list(model1, model2, model3, model4, model5)
description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, 
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- 27
knitr::opts_current$set(label = "ols_pooled_otherclassific")
modelsummary(ols_pooled, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             output = "./tables/ols_pooled_otherclassific.tex",
             add_rows = description,
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage and Fiscal Legislation",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is Catholic. Personal Wealth is defined as ihs(Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise.")
) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    kableExtra::add_header_above(c(" " = 1, "Pooled" = 3, "Suffrage" = 1, "Fiscal" = 1))


## IV models - suffrage
suffrage_iv <- left_join(suffrage, fiscal_iv %>%
                             select(b1_nummer, profdummy3), by = "b1_nummer") %>%
    distinct() %>%
    mutate(profdummy3 = profdummy3.y, percentage_aangesl = percentage_aangesl*100)

suffrage_iv <- suffrage_iv %>% left_join(rough_classific)
suffrage_iv <- suffrage_iv %>% 
    mutate(new_classific = case_when(
        stringr::str_detect(partij_en_fractie_s, 'liberaal|Liberaal|Lib|lib|Thorb|Takk') ~ "liberal",
        stringr::str_detect(partij_en_fractie_s, "RK|Rooms|Katholiek|katholiek|kath|conservatief (katholiek)|RKSP|Schaep|Bahl") ~ "catholic",
        stringr::str_detect(partij_en_fractie_s, "Prot|ARP|CHU|Anti-Rev|anti|cons|AR|Christ|christ|a.r.") ~ "protestant",
        stringr::str_detect(partij_en_fractie_s, "VDB|Vrije Demo|Socia|Soc|Comm|SDAP|Arbeid|Radi|vrijz|Vrijz|CPN|CPH") ~ "socialist",
        TRUE ~ NA_character_
    ))

fs1 <- lm(data = suffrage_iv, formula = ihs(wealth_timevote) ~ profdummy3 + law + new_classific)
iv1 <- ivreg(data = suffrage_iv, formula = vote ~ ihs(wealth_timevote) + law + new_classific | profdummy3 + law + new_classific)
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
attr(description, 'position') <- c(18, 19, 20)

knitr::opts_current$set(label = "ivresults_suffrage_otherclassific")
modelsummary(ivres, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC3",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_suffrage_otherclassific.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Suffrage Extensions",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as ihs(Wealth at Time of Vote), and instrumented by Fathers profession.",
                          "The reference political allegiance is Catholic. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 3))) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

## IV - fiscal
fiscal_iv <- fiscal_iv %>% left_join(rough_classific)
fiscal_iv <- fiscal_iv %>% 
    mutate(new_classific = case_when(
        stringr::str_detect(partij_en_fractie_s, 'liberaal|Liberaal|Lib|lib|Thorb|Takk') ~ "liberal",
        stringr::str_detect(partij_en_fractie_s, "RK|Rooms|Katholiek|katholiek|kath|conservatief (katholiek)|RKSP|Schaep|Bahl") ~ "catholic",
        stringr::str_detect(partij_en_fractie_s, "Prot|ARP|CHU|Anti-Rev|anti|cons|AR|Christ|christ|a.r.") ~ "protestant",
        stringr::str_detect(partij_en_fractie_s, "VDB|Vrije Demo|Socia|Soc|Comm|SDAP|Arbeid|Radi|vrijz|Vrijz|CPN|CPH") ~ "socialist",
        TRUE ~ NA_character_
    ))

fs1 <- lm(data = fiscal_iv %>%
              filter(class != "neutral"),
          formula = ihs(wealth_timevote) ~ profdummy3 + new_classific + law)
iv1 <- ivreg(data = fiscal_iv %>%
                 filter(class != "neutral"), 
             formula = vote ~ ihs(wealth_timevote) + new_classific + law | profdummy3 + new_classific + law)
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
              prtlout = c("law", "new_classific"),
              first_stage = "small", 
              R2max = 0.75)[1,2] %>% pull() %>% abs() %>% round(2)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6,
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
    "Kleibergen-Paap F Stat.", "", fstats[1], "", fstats[2], "", fstats[3],
    "Selection Ratio", "", "", "", "0.96", "", "1.91")

attr(description, 'position') <- c(25,26,27)
knitr::opts_current$set(label = "ivresults_fisc_other_classific")
modelsummary(ivresults, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC2",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results_fisc_other_classific.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as ihs(Wealth at Time of Vote), and instrumented by Fathers profession.",
                          "The reference political allegiance is confessional. Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    add_header_above(c(" " = 1, rep(c("Personal Wealth" = 1, "Vote" = 1), 3))) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

# check 2: party*law interaction fixed effects (only ols)


## with old classification
model1 <- datasets %>%
    filter(category == "fisc" | category == "suffrage", 
           house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ law + class + class:law)
model2 <- datasets %>%
    filter(category == "fisc" | category == "suffrage", house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ ihs(wealth_timevote) + law + class + class:law)
model3 <- datasets %>%
    filter(category == "fisc" | category == "suffrage", house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ ihs(wealth_timevote):category + law + class + class:law)
model4 <- lm(data = suffrage, formula = vote ~ ihs(wealth_timevote) + law + class + class:law +
                 strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)
model5 <- lm(data = fiscal, formula = vote ~ ihs(wealth_timevote) + law + class + class:law + 
                 strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)

ols_pooled <- list(model1, model2, model3, model4, model5)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, 
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes",
    "Law x Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes")

attr(description, 'position') <- c(25,26,27)
knitr::opts_current$set(label = "ols_pooled_lawpartyint")
modelsummary(ols_pooled, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law|class:law",
             out = "kableExtra",
             output = "./tables/ols_pooled_lawpartyint.tex",
             add_rows = description,
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage and Fiscal Legislation",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is confessional. Personal Wealth is defined as ihs(Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise.")
) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    kableExtra::add_header_above(c(" " = 1, "Pooled" = 3, "Suffrage" = 1, "Fiscal" = 1))



## with new classification
model1 <- datasets_rob %>%
    filter(category == "fisc" | category == "suffrage", 
           house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ law + new_classific + law:new_classific)
model2 <- datasets_rob %>%
    filter(category == "fisc" | category == "suffrage", house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ ihs(wealth_timevote) + law + new_classific + law:new_classific)
model3 <- datasets_rob %>%
    filter(category == "fisc" | category == "suffrage", house == "Tweede Kamer",
           class != "neutral") %>%
    lm(formula = vote ~ ihs(wealth_timevote):category + law + new_classific + law:new_classific)
model4 <- lm(data = datasets_rob %>% filter(category == "suffrage", house == "Tweede Kamer"), formula = vote ~ ihs(wealth_timevote) + law + new_classific + law:new_classific + 
                 strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)
model5 <- lm(data = datasets_rob %>% filter(category == "fisc", house == "Tweede Kamer"), formula = vote ~ ihs(wealth_timevote) + law + new_classific + law:new_classific +
                 strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)

ols_pooled <- list(model1, model2, model3, model4, model5)
description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, 
    "Law Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes",
    "Law x Party Fixed Effects", "Yes", "Yes", "Yes", "Yes", "Yes")

attr(description, 'position') <- c(27,28)
knitr::opts_current$set(label = "ols_pooled_otherclassific_lawpartyint")
modelsummary(ols_pooled, 
             stars = c("*" = .1, "**" = 0.05, "***" = 0.01),
             vcov = "HC1",
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law|law:new_classific",
             out = "kableExtra",
             output = "./tables/ols_pooled_otherclassific_lawpartyint.tex",
             add_rows = description,
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage and Fiscal Legislation",
             notes = list("Heteroskedasticity-robust standard errors in parentheses. Results for lower house voting outcomes.",
                          "The reference political allegiance is Catholic. Personal Wealth is defined as ihs(Wealth at Time of Vote).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise.")
) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    kableExtra::add_header_above(c(" " = 1, "Pooled" = 3, "Suffrage" = 1, "Fiscal" = 1))


