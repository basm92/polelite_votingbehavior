#fiscal law tk results
library(tidyverse);library(readxl);library(lubridate)

source("./code/data_treatment/get_data_from_voting_records_df.R")
source("./code/data_treatment/get_polid_tk_and_get_polid_ek.R")
files <- list.files("./data/voting_behavior/lowerhouse/fiscal/")

files <- paste0("./data/voting_behavior/lowerhouse/fiscal/", 
                list.files("./data/voting_behavior/lowerhouse/fiscal/"))

# Source them (get the dataframes)
lapply(files, source)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
data <- bind_rows(dfs)

data <- data %>%
    group_split(law) %>%
    lapply(get_polid_tk) %>%
    purrr::reduce(bind_rows)

#kieswet 1896 werkt niet - debug nog

data <- data %>%
    get_dataset_from_initial_voting_records()

## Descriptive Statistics

descr_tk <- data %>%
    group_split(law) %>%
    lapply(group_by, vote) %>%
    lapply(summarize, law = law[1], 
           median = median(wealth_timevote, na.rm = T),
           sd = sd(wealth_timevote, na.rm = T)) %>%
    lapply(pivot_wider, names_from = vote, 
           values_from = c(median,sd)) %>%
    purrr::reduce(bind_rows) %>%
    relocate(law,contains("0")) %>%
    rename("Median No" = median_0,
           "Sd No" = sd_0,
           "Median Yes" = median_1,
           "Sd Yes" = sd_1) 

## Regression - baseline

model_begin <- lm(data = data,
                  formula = vote ~ log(1 + wealth_timevote))
model0 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class)
model1 <- lm(data = data %>%
                 filter(house == "Tweede Kamer"),
             formula = vote ~ log(1+wealth_timevote) + class + law)

stargazer(model_begin, model0, model1,
          covariate.labels = c("Wealth"),
          dep.var.labels = "Vote",
          omit = c("law", "class"),
          omit.stat = c("adj.rsq", "ser","f"),
          add.lines = list(c("Controls", "None", "Party", "Party+Law")
          ),
          #          notes.append = T,
          #          notes = c("Robust standard errors in parentheses"),
          header = F,
                    font.size = "small",
          column.sep.width = "1pt",
          title = "Baseline regressions")


## Regression - dummy
dum <- data %>%
    mutate(wealthdum = if_else(wealth_timevote > 50000, 1, 0)) 

model_begin <- lm(data = dum,
                  formula = vote ~ wealthdum)
model0 <- lm(data = dum,
             formula = vote ~ wealthdum+ class)
model1 <- lm(data = dum %>%
                 filter(house == "Tweede Kamer"),
             formula = vote ~ wealthdum + class + law)

stargazer(model_begin, model0, model1,
          covariate.labels = c("Wealth"),
          dep.var.labels = "Vote",
          omit = c("law", "class"),
          omit.stat = c("adj.rsq", "ser","f"),
          add.lines = list(c("Controls", "None", "Party", "Party+Law")
          ),
          #          notes.append = T,
          #          notes = c("Robust standard errors in parentheses"),
          header = F,
                    font.size = "small",
          column.sep.width = "1pt",
          title = "Baseline regressions")


## Full controls
model3 <- lm(data = data, 
             formula = vote ~ log(1+wealth_timevote) + class + law)

model4 <-  lm(data = data, 
              formula = vote ~ log(1+wealth_timevote) + class + law + strikes)

model5 <- lm(data = data, 
             formula = vote ~ log(1+wealth_timevote) + class + law + 
                 strikes + rk_pct)

model6 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + law +
                 strikes + rk_pct  + tenure)

model7 <- lm(data = data, 
             formula = vote ~ log(1+wealth_timevote) + class + law + 
                 strikes + rk_pct + tenure + agricul_share)

model8 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + law + 
                 strikes + rk_pct + agricul_share + socialistpercentage
             )

model9 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + law +
                 strikes + rk_pct  + tenure + ncm
)

model10 <- lm(data = data,
              formula = vote ~ log(1+wealth_timevote) + class + law +
                  strikes + rk_pct  + tenure + ncm + kiesgerechtigden + age_of_entrance
)

# Two summary tables
stargazer(model3, model4, model5, model6,
          covariate.labels = c("Wealth"),
          dep.var.labels = "Vote",
          omit = c("class", "law", "strikes",
                   "rk_pct", "tenure"),
          omit.stat = c("adj.rsq", "ser","f"),
          add.lines = list(c("Controls", "Party+law", "2+Strikes",
                             "3+Religion", "4+Electoral")
          ),
          #          notes.append = T,
          #          notes = c("Robust standard errors in parentheses"),
          header = F,
          font.size = "small",
          column.sep.width = "1pt",
          title = "Regressions incl. controls (i)")

stargazer(model7, model8, model9, model10, 
          covariate.labels = c("Wealth"),
          dep.var.labels = "Vote",
          omit = c("class", "law", "strikes",
                   "rk_pct", "tenure", "agricul_share", "tenure", 
                   "kiesgerechtigden", "age_of_entrance", 
                   "socialistpercentage", "ncm"),
          omit.stat = c("adj.rsq", "ser","f"),
          add.lines = list(c("Controls", "P,L,R, Strikes", "1+Socialism",
                             "1+E+D", "1+E+D")
          ),
          #          notes.append = T,
          #          notes = c("Robust standard errors in parentheses"),
          header = F,
          font.size = "small",
          column.sep.width = "1pt",
          title = "Regressions incl. controls (ii)")

