# take the data file
library(ivreg); library(modelsummary); 
library(xtable) ; library(kableExtra)
source("./code/data_treatment/data_treatment.R")

## This is the script for all the results in the paper

## Simple districtive statistics tables
### Wealth per law (not in paper)
descriptives <- df %>%
    group_split(house)

descr_ek <- descriptives[[1]] %>%
    group_split(law) %>%
    lapply(group_by, vote) %>%
    lapply(summarize, law = law[1], 
           median = median(wealth_timevote, na.rm = T)
           ) %>%
    lapply(pivot_wider, names_from = vote, 
           values_from = c(median)) %>%
    purrr::reduce(bind_rows) %>%
    relocate(law,contains("0")) %>%
    rename("Median No" = `0`,
           #"Sd No" = sd_0,
           "Median Yes" = `1`#,
           #"Sd Yes" = sd_1
           ) 

descr_tk <- descriptives[[2]] %>%
    group_split(law) %>%
    lapply(group_by, vote) %>%
    lapply(summarize, law = law[1], 
           median = median(wealth_timevote, na.rm = T)
           ) %>%
    lapply(pivot_wider, names_from = vote, 
           values_from = c(median)) %>%
    purrr::reduce(bind_rows) %>%
    relocate(law,contains("0")) %>%
    rename("Median No" = `0`,
           #"Sd No" = sd_0,
           "Median Yes" = `1`#,
           #"Sd Yes" = sd_1
           ) 

descr <- list(descr_ek, descr_tk)

#saveRDS(descr, "./figures/descr.RDS")

### Table on voting behavior
knitr::opts_current$set(label = "votespercentage")
datasummary(data = df %>%
              filter(class != "neutral",
                     !is.na(class)) %>%
              select(vote, law, house, class) %>%
              mutate(law = factor(law, 
                                  levels = c(
                                    "Successiewet 1878","Inkomstenbelasting 1893",
                                    "Successiewet 1911","Staatsschuldwet 1914",
                                    "Inkomstenbelasting 1914", "Successiewet 1916",
                                    "Successiewet 1921")
                                  )
                     ),
            law ~ vote * Mean * house * class,
            output = "./tables/votespercentage.tex",
            out = "kableExtra",
            title = "Votes in favor of Laws",
            notes = list(
              "\\\\footnotesize{Percentage of upper house and lower house members having voted in favor of fiscal reforms.}"
            )) %>%
  kableExtra::kable_styling(latex_options = "hold_position") 


## Baseline results (OLS)
baseline <- lm(data = df,
                  formula = vote ~ log(1 + wealth_timevote))
model1 <- lm(data = df,
             formula = vote ~ log(1+wealth_timevote) + class)
model2 <- lm(data = df %>%
       filter(house == "Tweede Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class)
model3 <- lm(data = df %>%
       filter(house == "Eerste Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class) 
model4 <- lm(data = df,
             formula = vote ~ log(1+wealth_timevote) + class + law)
model5 <- lm(data = df %>%
               filter(house == "Tweede Kamer"),
             formula = vote ~ log(1+wealth_timevote) + class + law)
model6 <- lm(data = df %>%
               filter(house == "Eerste Kamer"),
             formula = vote ~ log(1+wealth_timevote) + class + law)
 

first_regs <- list("(1)" = baseline,
                   "(2)" = model1, 
                   "(3)" = model2, 
                   "(4)" = model3, 
                   "(5)" = model4, 
                   "(6)" = model5, 
                   "(7)" = model6)

stargazer(first_regs, type = "text")

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
  "House", "Both", "Both", "Tweede Kamer", "Eerste Kamer", "Both", "Tweede Kamer", "Eerste Kamer",
  "Controls", "None", "Party", "Party", "Party", "Party+Law", "Party+Law", "Party+Law")
attr(description, 'position') <- c(3,4,5)

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "adj.r.squared","Adj. R2", 2)

knitr::opts_current$set(label = "baseline_ols")
modelsummary(first_regs, 
             stars=TRUE, 
             vcov = vcovHC,
             notes = list("Heteroskedasticity-robust standard errors in parentheses.",
                          "Personal Wealth is defined as log(1+Wealth at Death). Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."),
             coef_omit = "Intercept|class|law",
             coef_map = c("log(1 + wealth_timevote)" = "Personal Wealth"),
             gof_map = gm,
             out = "kableExtra",
             add_rows = description,
             output = "./tables/baseline_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Reforms") %>%
  kableExtra::kable_styling(latex_options = "hold_position",
                            font_size = 9) 

#saveRDS(first_regs, "./figures/first_regs.RDS")

### Baseline Results - Full controls

coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "strikes" = "Amount of Strikes",
                 "rk_pct" = "% Catholics in district",
                 "industry_share" = "Share Industrial",
                 "tvs" = "Vote Share (% Total)",
                 "socialistdum1" = "Competed Against Socialist",
                 "tenure" = "Tenure"
                 )

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
  "Party + Law Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(15,16,17)

#### Tweede Kamer
baseline <- lm(data = df %>%
       filter(house == "Tweede Kamer"),
   formula = vote ~ log(1+wealth_timevote) + law + class)

model1 <- update(baseline, . ~ . + strikes) 
model2 <- update(model1, . ~ . + rk_pct)
model3 <- update(model2, . ~ . + industry_share)
model4 <- update(model2, . ~ . + tvs)
model5 <- update(model4, . ~ . + socialistdum)
model6 <- update(model5, . ~ . + tenure)

tk_fullctrls <- list("(1)" = baseline, 
                     "(2)" = model1, 
                     "(3)" = model2, 
                     "(4)" = model3, 
                     "(5)" = model4,
                     "(6)" = model5,
                     "(7)" = model6)

knitr::opts_current$set(label = "controls_ols")

modelsummary(tk_fullctrls, 
             stars=TRUE, 
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law|class",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/controls_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Reforms - Controls",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  kableExtra::kable_styling(latex_options = "hold_position",
                            font_size = 9) 
  
### Eerste Kamer
coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "tenure" = "Tenure",
                 "age_of_vote" = "Age at Time of Vote",
                 "long_elec_horiz" = "Long Electoral Horizon"
)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5,
  "Controls", "Party", "Party+Law", "Party+Law", "Party+Law", "Party+Law")
attr(description, 'position') <- c(9,10,11)

modelek1 <- lm(data = df %>%
       filter(house == "Eerste Kamer") %>%
         mutate(tenure = tenure/10000,
                age_of_vote = age_of_vote/10000,
                age_of_entrance = age_of_entrance/10000,
                long_elec_horiz = long_elec_horiz/10000),
   formula = vote ~ log(1+wealth_timevote) + class)
modelek2 <- update(modelek1, . ~ . + law)
modelek3 <- update(modelek2, . ~ . + tenure)
modelek4 <- update(modelek3, . ~ . + age_of_vote)
modelek5 <- update(modelek4, . ~ . + long_elec_horiz)

ek_fullctrls <- list("(1)" = modelek1, 
                     "(2)" = modelek2, 
                     "(3)" = modelek3, 
                     "(4)" = modelek4, 
                     "(5)" = modelek5)

knitr::opts_current$set(label = "baseline_ek_ols")

modelsummary(ek_fullctrls, 
             stars=TRUE, 
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law|class",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/baseline_ek_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Reforms - Upper House",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis.",
                          "Results for upper house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  kableExtra::kable_styling(latex_options = "hold_position",
                            font_size = 9) 

#saveRDS(ek_fullctrls, "./figures/ek_fullctrls.RDS")

### Harnas: Died Shortly After Vote (2 Yr)

#### Settings for modelsummary
coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "harnasTRUE" = "Died Within 2 Years",
                 "log(1 + wealth_timevote):harnasTRUE" = "Wealth x Died Within 2 Years",
                 "strikes" = "Amount of Strikes",
                 "rk_pct" = "% Catholics in district",
                 "industry_share" = "Share Industrial",
                 "tvs" = "Vote Share (% Total)",
                 "socialistdum1" = "Competed Against Socialist",
                 "tenure" = "Tenure"
)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
  "Party + Law Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(19, 20, 21)

df <- df %>%
    mutate(harnas = (date_of_death - einde_periode) < 730)

h2model1 <- lm(data = df %>%
                 filter(house == "Tweede Kamer"), formula = vote ~ log(1+wealth_timevote) + harnas + log(1+wealth_timevote)*harnas + class + law)
h2model2 <- update(h2model1, . ~ . + strikes)
h2model3 <- update(h2model2, . ~ . + rk_pct)
h2model4 <- update(h2model3, . ~ . + industry_share)
h2model5 <- update(h2model3, . ~ . + tvs)
h2model6 <- update(h2model5, . ~ . + socialistdum)
h2model7 <- update(h2model6, . ~ . + tenure)

harnas2 <- list("(1)" = h2model1,
                "(2)" = h2model2,
                "(3)" = h2model3,
                "(4)" = h2model4,
                "(5)" = h2model5,
                "(6)" = h2model6,
                "(7)" = h2model7)

## Harnas: Died Shortly After Vote (5 Yr)
df <- df %>%
  mutate(harnas = (date_of_death - einde_periode) < 1825) 

h5model1 <- lm(data = df %>%
                 filter(house == "Tweede Kamer"), formula = vote ~ log(1+wealth_timevote) + harnas + log(1+wealth_timevote)*harnas + class + law)
h5model2 <- update(h5model1, . ~ . + rk_pct)
h5model3 <- update(h5model2, . ~ . + industry_share)
h5model4 <- update(h5model2, . ~ . + tvs)
h5model5 <- update(h5model4, . ~ . + socialistdum)
h5model6 <- update(h5model5, . ~ . + tenure)

harnas5 <- list("(1)" = h5model1,
                "(2)" = h5model2,
                "(3)" = h5model3,
                "(4)" = h5model4,
                "(5)" = h5model5,
                "(6)" = h5model6)

knitr::opts_current$set(label = "harnas2")

modelsummary(harnas2, 
             stars=TRUE, 
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law|class",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/harnas2.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Fiscal Reforms - Endogeneity Test",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  kableExtra::kable_styling(latex_options = "hold_position",
                            font_size = 9) 


### Instrumental Variables

#### First, some data wrangling
parwealth <- readxl::read_xlsx("./data/polid_data/instrumental_variable_est.xlsx") %>%
    mutate(across(c(wealth_father, wealth_mother, wealth_misc), ~ as.numeric(.))) %>%
    mutate(par_wealth = pmax(wealth_father, wealth_mother, wealth_misc, na.rm = TRUE),
    )

#### Deflate them
deflator <- readr::read_csv("./data/polid_data/deflator.csv") %>%
  janitor::clean_names()

deflate <- function(parwealthdf){
  
  parwealthdf <- parwealthdf %>%
    mutate(a = as.numeric(stringr::str_extract(dod_father, "\\d{4}")))
  
  parwealthdf <- parwealthdf %>%
    mutate(index = purrr::map_dbl(parwealthdf$a, function(x) match(x, deflator$year, nomatch = 999)))
  
  parwealthdf <- parwealthdf %>%
    mutate(deflator = purrr::map_dbl(parwealthdf$index, function(x) deflator$deflator[x]))
  
  parwealthdf <- parwealthdf %>%
    mutate(par_wealth = par_wealth * deflator)
  
  return(parwealthdf)
  
}


parwealth <- deflate(parwealth)

### Get the other instruments
workingclass <- c("Bakker, Predikant, Schipper", "Lijfwacht", "Ambtenaar",
                  "Kleermaker", "Rentmeester", "Horlogemaker","Musicus",
                  "Secretaris", "Leraar","Inspecteur", "Onderwijzer/Rabbijn",
                  "Boer", "Machinist", "Artiest")

instruments <- parwealth %>%
    mutate(profdummy1 = case_when(father_profession == "Jurist" ~ 1,
                                father_profession == "Politicus" ~ 1,
                                TRUE ~ 0),
           profdummy2 = case_when(father_profession == "Jurist" ~ 1,
                                  TRUE ~ 0),
           profdummy3 = case_when(father_profession == "Politicus" ~ 1,
                                  TRUE ~ 0),
           profdummy4 = case_when(father_profession == "Jurist" ~ 1,
                                   father_profession == "Politicus" ~ 1,
                                   father_profession == "Ambtenaar" ~ 1,
                                   TRUE ~ 0),
           profdummy5 = case_when(is.element(father_profession, workingclass) ~ 1,
                                  TRUE ~ 0)
           ) %>%
  select(polid, par_wealth, profdummy1, profdummy2, profdummy3, profdummy4, profdummy5)

ivdata <- df %>%
    filter(house == "Tweede Kamer") %>%
    left_join(instruments,
              by = c("b1_nummer"="polid"))

### Check - distribution of dummy per party

ivdata %>%
  group_by(class) %>%
  summarize(mean = mean(profdummy1, na.rm = TRUE), sd = sd(profdummy1, na.rm = TRUE))


saveRDS(ivdata, "./figures/ivdata.RDS")

#firststage_reg
firststage_reg <- lm(data = ivdata, 
                     formula = log(1+wealth_timevote) ~ log(1+par_wealth))
firststage_reg <- lm(data = ivdata, 
                     formula = log(1+wealth_timevote) ~ profdummy1)


#ivreg - couple of different baselines
baseline <- ivreg(data = ivdata %>%
                    mutate(wealth_timevote = wealth_timevote / 100000,
                           tenure = tenure/10000), 
                  formula = vote ~ log(1+wealth_timevote) + class + law | profdummy1 + class + law)

# All models
model2 <- update(baseline, . ~ . + strikes | . + strikes) 
model3 <- update(model2, . ~ . + rk_pct | . + rk_pct)
model4 <- update(model3, . ~ . + industry_share | . + industry_share)
model5 <- update(model3, . ~ . + tvs | . + tvs)
model6 <- update(model5, . ~ . + tenure | . + tenure)
model7 <- update(model6, . ~ . + socialistpercentage | . + socialistpercentage)


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
                 "rk_pct" = "% Catholics in district",
                 "industry_share" = "Share Industrial",
                 "tvs" = "Vote Share (% Total)",
                 "tenure" = "Tenure",
                 "socialistpercentage" = "Socialist Vote Last Election (% Total)"
)

description <- tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
  "Party + Law Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "F-Stat. First stage", fstats[1], fstats[2], fstats[3], fstats[4], fstats[5], fstats[6], fstats[7])

attr(description, 'position') <- c(15,16,17)

knitr::opts_current$set(label = "ivresults")
modelsummary(ivresults, 
             stars=TRUE, 
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law|class",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/iv_results.tex",
             title = "IV Estimates of Wealth on the Propensity to Vote for Fiscal Reforms",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death), and instrumented by Fathers profession.",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
  kableExtra::kable_styling(latex_options = "hold_position",
                            font_size = 9) 



# other iv models - from here proceed tomorrow

model6 <- update(baseline, . ~ . + class + law | . + class + law)
model7 <- update(model6, . ~ . + tenure | . + tenure)
model8 <- update(model7, . ~ . + age_of_vote | . + age_of_vote)
model9 <- update(model8, . ~ . + age_of_entrance | . + age_of_entrance)

stargazer(baseline, model6, model7, model8, model9, type = "text")

# combination
model10 <- update(model5, . ~ . + strikes | . + strikes) 
model11 <- update(model6, . ~ . + rk_pct | . + rk_pct)
model12 <- update(model7, . ~ . + agricul_share | . + agricul_share)

stargazer(baseline, model10, model11, model12, type = "text")

# yet other models
model13 <- update(baseline, . ~ . + law + age_of_vote | . + law + age_of_vote)
model14 <- update(model13 , . ~ . + turnout | . + turnout)
model15 <- update(model14, . ~ . + hervormd_pct | . + hervormd_pct)
model16 <- update(model15, . ~ . + industry_share | . + industry_share)

stargazer(baseline, model13, model14, model15, model16, type = "text")

### Analysis: Second order effects
model_re <- lm(data = df,
   formula = vote ~ share_re + class)
model_re2 <- lm(data = df,
                formula = vote ~ share_re + class + law)
model_foreign <- lm(data = df,
                    formula = vote ~ share_foreign + class)
model_foreign2 <- lm(data = df,
                    formula = vote ~ share_foreign + class + law)
model_shares <- lm(data = df,
                   formula = vote ~ share_shares + class)
model_shares2 <- lm(data = df,
   formula = vote ~ share_shares + class + law)

secondorder <- list(model_re, model_re2,
                    model_foreign, model_foreign2,
                    model_shares, model_shares2)

saveRDS(secondorder, "./figures/second_order_regs.RDS")

