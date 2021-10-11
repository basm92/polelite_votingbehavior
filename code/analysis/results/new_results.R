# new_results
source("./code/analysis/results/get_data.R")

datasets <- datasets %>%
  mutate(wealth_timevote = wealth_timevote / 10)
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

modelsummary(ols_pooled, stars = TRUE, vcov = "HC1")


# Suffrage (First three panels) and Fiscal (Second three panels)
model1 <-lm(data = suffrage, formula = vote ~ ihs(wealth_timevote) + law + class)
model2 <- lm(data = suffrage, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct)
model3 <- lm(data = suffrage, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)
model4 <- lm(data = fiscal, formula = vote ~ ihs(wealth_timevote) + law + class)
model5 <- lm(data = fiscal, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+socialistpercentage+rk_pct)
model6 <- lm(data = fiscal, formula = vote ~ ihs(wealth_timevote) + law + class + 
               strikes+tvs+turnout+ncm+tenure+rk_pct + percentage_aangesl)

hoihoi <- list(model1, model2, model3, model4, model5, model6)
modelsummary(hoihoi, stars = T, vcov = "HC1", outpt = "kableExtra") %>%
  kableExtra::add_header_above(c(" " = 1, "Suffrage Extension" = 3, "Fiscal Legislation" = 3))


## Iv results - profdummy 3 - fiscal 
fs1 <- lm(data = fiscal_iv,
          formula = ihs(1+wealth_timevote) ~ profdummy3 + class + law)
iv1 <- ivreg(data = fiscal_iv %>%
               filter(class != "neutral"), 
             formula = vote ~ ihs(1+wealth_timevote) + class + law | profdummy3 + class + law)

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

modelsummary(ivresults, stars = TRUE, vcov = "HC1")

## Inheritance results - fiscal 
fs1 <- lm(data = fiscal_iv %>%
            mutate(exp_inherit = exp_inherit/100000) %>%
            filter(class != "neutral"),
          formula = ihs(1+wealth_timevote) ~ exp_inherit + class + law)
iv1 <- ivreg(data = fiscal_iv %>%
               filter(class != "neutral"), 
             formula = vote ~ ihs(1+wealth_timevote) + class + law | exp_inherit + class + law)

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

modelsummary(ivresults, stars = TRUE, vcov = "HC1")

## Iv results for suffrage - null results

suffrage_iv <- left_join(suffrage, fiscal_iv %>%
                    select(b1_nummer, profdummy3), by = "b1_nummer") %>%
  distinct()

model1 <- ivreg(data = suffrage_iv, formula = vote ~ ihs(wealth_timevote) + law + class | profdummy3.y + law + class)
model2<- update(model1, . ~ . + strikes + rk_pct | . + strikes + rk_pct)
model3 <- update(model2, . ~ . +  tvs + socialistpercentage + turnout + ncm + tenure | . + tvs + socialistpercentage + turnout + ncm + tenure)
model4 <- update(model3, . ~ . + industry_share | . + industry_share)
model5 <- update(model4, . ~ . + percentage_aangesl | . + percentage_aangesl)

ivres <- list(model1, model2, model3, model4, model5) 
modelsummary(ivres, stars = TRUE, vcov = "HC1")

# logit results - suffrage
model1 <- clogit(formula = vote ~ ihs(wealth_timevote) + strata(law) + strata(class), data = suffrage)
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + tvs)
model4 <- update(model3, . ~ . + turnout)
model5 <- update(model4, . ~ . + ncm)
model6 <- update(model5, . ~ . + tenure)
model7 <- update(model6, . ~ . + rk_pct)
model8 <- update(model7, . ~ . + percentage_aangesl)

modelz <- list(model1, model2, model3, model4, model5, model6, model7, model8)

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared","$R^2$", 2,
  "r.squared.max", "Max. $R^2$", 2)

modelsummary(modelz, 
             stars = T,
             gof_map = gm)

# Logit results - fiscal 
library(survival)

model1 <- clogit(formula = vote ~ ihs(wealth_timevote) + strata(law) + strata(class), data = fiscal)
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + tvs)
model4 <- update(model3, . ~ . + turnout)
model5 <- update(model4, . ~ . + ncm)
model6 <- update(model5, . ~ . + tenure)
model7 <- update(model6, . ~ . + socialistpercentage)
model8 <- update(model7, . ~ . + rk_pct)
model9 <- update(model8, . ~ . + percentage_aangesl)

modelz <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9)

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared","$R^2$", 2,
  "r.squared.max", "Max. $R^2$", 2)

modelsummary(modelz, 
             stars = T,
             gof_map = gm)

