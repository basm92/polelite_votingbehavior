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

model1 <- lm(data = data %>%
                 filter(house == "Tweede Kamer") %>%
                 mutate(tenure = tenure/365),
             formula = vote ~ log(1+wealth_timevote) + class + law)


## More extensive analysis
model2 <- update(model1, . ~ . + strikes)
model3 <- update(model2, . ~ . + rk_pct)
model4 <- update(model3, . ~ . + industry_share)
model5 <- update(model3, . ~ . + tvs)
model6 <- update(model5, . ~ . + turnout)
model7 <- update(model6, . ~ . + tenure)

electorallaw_ols <- list("(1)" = model1, 
                         "(2)" = model2, 
                         "(3)" = model3, 
                         "(4)" = model4, 
                         "(5)" = model5, 
                         "(6)" = model6, 
                         "(7)" = model7)

# Save for the comparison of coefficients
saveRDS(electorallaw_ols, "./figures/electoral_law_regressions.RDS")

gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2)


description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
    "Party + Law Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(15, 16, 17)

coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "log(1 + wealth_timevote):harnasTRUE" = "Wealth x Died Within 2 Years",
                 "strikes" = "Amount of Strikes",
                 "rk_pct" = "% Catholics in district",
                 "industry_share" = "Share Industrial",
                 "tvs" = "Vote Share (% Total)",
                 "turnout" = "Electoral Turnout (%)",
                 "tenure" = "Tenure"
)

modelsummary(electorallaw_ols, 
             stars=TRUE, 
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law|class",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/electorallaw_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Suffrage Expansion",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 
