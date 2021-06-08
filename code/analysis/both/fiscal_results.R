# take the data file

source("./code/data_treatment/data_treatment.R")

ihs <- function(x) {
    y <- log(x + sqrt(x ^ 2 + 1))
    return(y)
}

## Simple districtive statistics table

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
saveRDS(descr, "./figures/descr.RDS")

## Baseline results
model_begin <- lm(data = df,
                  formula = vote ~ log(1 + wealth_timevote))
model0 <- lm(data = df,
             formula = vote ~ log(1+wealth_timevote) + class)
model1 <- lm(data = df %>%
       filter(house == "Tweede Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class)
model2 <- lm(data = df %>%
       filter(house == "Eerste Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class) 

first_regs <- list(model_begin, model0, model1, model2)

stargazer(first_regs, type = "text")
saveRDS(first_regs, "./figures/first_regs.RDS")

## Baseline Results - Full controls

# Both
# Tweede Kamer
model3 <- lm(data = df %>%
       filter(house == "Tweede Kamer"),
   formula = vote ~ log(1+wealth_timevote) + law + class)

model4 <- update(model3, . ~ . + strikes) 
model5 <- update(model4, . ~ . + rk_pct)
model6 <- update(model5, . ~ . + agricul_share)

tk_fullctrls <- list(model3, model4, model5, model6)
stargazer(tk_fullctrls, type = "text")

saveRDS(tk_fullctrls, "./figures/tk_fullctrls.RDS")

# Eerste Kamer
modelek1 <- lm(data = df %>%
       filter(house == "Eerste Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class)

modelek2 <- update(modelek1, . ~ . + tenure)
modelek3 <- update(modelek2, . ~ . + age_of_vote)
modelek4 <- update(modelek3, . ~ . + age_of_entrance)


ek_fullctrls <- list(modelek1, modelek2, modelek3, modelek4)
stargazer(ek_fullctrls, type = "text")
saveRDS(ek_fullctrls, "./figures/ek_fullctrls.RDS")

## Robustness: Died Shortly After Vote
harnas_2yr <- df %>%
    mutate(harnas = (date_of_death - einde_periode) < 730) %>%
    #filter(harnas == TRUE) %>%
    lm(formula = vote ~ log(1+wealth_timevote):harnas + class) 

harnas_2yr_ctrls <- df %>%
    mutate(harnas = (date_of_death - einde_periode) < 730) %>%
    #filter(harnas == TRUE) %>%
    lm(formula = vote ~ log(1+wealth_timevote):harnas + class + law) 

harnas_5yr <- df %>%
    mutate(harnas = (date_of_death - einde_periode) < 1825) %>%
    #filter(harnas == TRUE) %>%
    lm(formula = vote ~ log(1+wealth_timevote):harnas + class) 

harnas_5yr_ctrls <- df %>%
    mutate(harnas = (date_of_death - einde_periode) < 1825) %>%
    #filter(harnas == TRUE) %>%
    lm(formula = vote ~ log(1+wealth_timevote):harnas + class + law) 

harnas <- list(harnas_2yr, harnas_2yr_ctrls, harnas_5yr, harnas_5yr_ctrls)

stargazer(harnas, type = "text")
saveRDS(harnas, "./figures/harnas.RDS")

## Instrumental Variables

parwealth <- readxl::read_xlsx("./data/polid_data/instrumental_variable_est.xlsx") %>%
    mutate(across(c(wealth_father, wealth_mother, wealth_misc), ~ as.numeric(.))) %>%
    mutate(par_wealth = pmax(wealth_father, wealth_mother, wealth_misc, na.rm = TRUE),
    )

### Deflate them
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

### Get the other instrument

test <- parwealth %>%
    mutate(profdummy1 = case_when(father_profession == "Jurist" ~ 1,
                                father_profession == "Politicus" ~ 1,
                                TRUE ~ 0),
           profdummy2 = case_when(father_profession == "Jurist" ~ 1,
                                  TRUE ~ 0),
           profdummy3 = case_when(father_profession == "Politicus" ~ 1,
                                  TRUE ~ 0)
           ) %>%
    filter(!is.na(father_profession)) ### This can be removed once we have all obs

ivdata <- df %>%
    filter(house == "Tweede Kamer") %>%
    left_join(parwealth,
              by = c("b1_nummer"="polid"))

ivdata <- df %>%
    filter(house == "Tweede Kamer") %>%
    left_join(test,
              by = c("b1_nummer"="polid"))

### Check - wat te doen met deze obs?

setdiff(df %>%
            filter(house == "Tweede Kamer", !is.na(w_deflated)) %>%
            .$b1_nummer, parwealth$polid)

library(ivreg) 

firststage_plot <- ggplot(data = ivdata, 
       aes(y = log(1+wealth_timevote), 
           x = log(par_wealth))) + 
    geom_point(alpha = 0.5) +
    ggtitle("Instrument relevance") + 
    xlab("Log (Parental Wealth)") + 
    ylab("Log (Politician Wealth)")

saveRDS(ivdata, "./figures/ivdata.RDS")

#firststage_reg
firststage_reg <- lm(data = ivdata, 
                     formula = log(1+wealth_timevote) ~ log(1+par_wealth))

firststage_reg1 <- lm(data = ivdata, 
                     formula = log(1+wealth_timevote) ~ profdummy1)
firststage_reg2 <- lm(data = ivdata, 
                     formula = log(1+wealth_timevote) ~ profdummy2)
firststage_reg3 <- lm(data = ivdata, 
                     formula = log(1+wealth_timevote) ~ profdummy3)

stargazer(firststage_reg1, firststage_reg2, firststage_reg3, type = "text")

#ivreg
baseline <- ivreg(data = ivdata, 
                       formula = vote ~ ihs(wealth_timevote) + class | ihs(par_wealth) + class)

baseline <- ivreg(data = ivdata, 
                  formula = vote ~ ihs(wealth_timevote) + class | profdummy1 + class)

baseline <- ivreg(data = ivdata %>%
                    mutate(wealth_timevote = wealth_timevote / 100000,
                           par_wealth = par_wealth / 100000), 
                  formula = vote ~ wealth_timevote + class | par_wealth + class)

baseline <- ivreg(data = ivdata, 
                  formula = vote ~ log(1+wealth_timevote) + class | log(1+par_wealth) + class)

model2 <- update(baseline, . ~ . + law | . + law)
model3 <- update(model2, . ~ . + strikes | . + strikes) 
model4 <- update(model3, . ~ . + rk_pct | . + rk_pct)
model5 <- update(model4, . ~ . + agricul_share | . + agricul_share)

ivresults <- list(firststage_plot, firststage_reg, iv_reg_prelim)
saveRDS(ivresults, "./figures/ivresults.RDS")

stargazer(baseline, model2, model3, model4, model5, type = "text")

# other models
baseline <- ivreg(data = ivdata, 
                  formula = vote ~ ihs(wealth_timevote) + class | ihs(par_wealth) + class)

baseline <- ivreg(data = ivdata %>%
                    mutate(wealth_timevote = wealth_timevote / 100000,
                           par_wealth = par_wealth / 100000), 
                  formula = vote ~ wealth_timevote + class | par_wealth + class)

baseline <- ivreg(data = ivdata %>%
                      mutate(wealth_timevote = wealth_timevote / 100000), 
                  formula = vote ~ wealth_timevote + class | profdummy1 + class)

model2 <- update(baseline, . ~ . + class + law | . + class + law)
model3 <- update(model2, . ~ . + tenure | . + tenure)
model4 <- update(model3, . ~ . + age_of_vote | . + age_of_vote)
model5 <- update(model4, . ~ . + age_of_entrance | . + age_of_entrance)

stargazer(baseline, model2, model3, model4, model5, type = "text")

# combination
model6 <- update(model5, . ~ . + strikes | . + strikes) 
model7 <- update(model6, . ~ . + rk_pct | . + rk_pct)
model8 <- update(model7, . ~ . + agricul_share | . + agricul_share)

stargazer(baseline, model6, model7, model8, type = "text")

model9 <- update(baseline, . ~ . + law + age_of_vote | . + law + age_of_vote)
model10 <- update(model9 , . ~ . + turnout | . + turnout)
model11 <- update(model10, . ~ . + hervormd_pct | . + hervormd_pct)
model12 <- update(model11, . ~ . + industry_share | . + industry_share)

stargazer(baseline, model9, model10, model11, model12, type = "text")

## Alternative iv specification: parent already died before first vote:
ivdata2 <- left_join(df, parwealth %>%
              mutate(dod_father = lubridate::ymd(dod_father)),
          by = c("b1_nummer"="polid")) %>%
    mutate(intr = date > dod_father) %>%
    filter(!is.na(intr))

firststage_plot <- ggplot(data = ivdata2, 
                          aes(y = log(1+wealth_timevote), 
                              x = intr)) + 
    geom_point() +
    ggtitle("Instrument relevance") + 
    xlab("Log (Parental Wealth)") + 
    ylab("Log (Politician Wealth)")

iv_reg_prelim <- ivreg(data = ivdata2, 
                       formula = vote ~ log(1+wealth_timevote) + class | intr + class)

summary(iv_reg_prelim)

## Analysis: Second order effects
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

## Analysis: Other Laws
