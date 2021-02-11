#electorallaw_results
library(stargazer)
library(tidyverse)
library(readxl)
library(stringr)


source("./code/data_treatment/get_data_from_voting_records_df.R")
source("./code/data_treatment/get_polid_tk_and_get_polid_ek.R")
files <- list.files("./data/voting_behavior/lowerhouse/electoral_law/")

files <- paste0("./data/voting_behavior/lowerhouse/electoral_law/", 
                list.files("./data/voting_behavior/lowerhouse/electoral_law/"))

# Source them (get the dataframes)
lapply(files, source)

data <- bind_rows(kieswet1872, kieswet1887, kieswet1892, kieswet1896, kieswet1918)

data <- data %>%
    group_split(law) %>%
    lapply(get_polid_tk) %>%
    purrr::reduce(bind_rows)

#kieswet 1896 werkt niet - debug nog

data <- data %>%
    get_dataset_from_initial_voting_records()

#readr::write_csv(data, "./electoral_law_lh_votes_entire_dataset.csv")

# write list of laws
list_of_laws_elections <- data %>%
    distinct(law)

saveRDS(list_of_laws_elections, "./figures/list_of_laws_elections.RDS")
## Descriptive statistics

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
#          font.size = "tiny",
          column.sep.width = "1pt",
          title = "Baseline regressions")

## More extensive analysis
model3 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + law + strikes)
model4 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + law + strikes + rk_pct)
model5 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + 
                 law + strikes + rk_pct + agricul_share)
model6 <- lm(data = data,
                       formula = vote ~ log(1+wealth_timevote) + class + law + 
                 strikes + rk_pct + agricul_share + ncm)

#elect_law_res_tk <- list(model3, model4, model5, model6)
#saveRDS(elect_law_res_tk, "./figures/electoral_law_regressions.RDS")

stargazer(model3, model4, model5, model6,
          covariate.labels = c("Wealth"),
          dep.var.labels = "Vote",
          omit = c("law", "class", "strikes", "rk_pct", "agricul_share", "ncm"),
          omit.stat = c("adj.rsq", "ser","f"),
          add.lines = list(c("Controls", "Strikes", "1+Religion", "2+Economy", "3+Electoral")
          ),
          #          notes.append = T,
          #          notes = c("Robust standard errors in parentheses"),
          header = F,
          #          font.size = "tiny",
          column.sep.width = "1pt",
          title = "Controls included")
