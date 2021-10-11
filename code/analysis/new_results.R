# new_results
## New table-creating file
library(ivreg); library(modelsummary); library(tidyverse); library(stargazer)
library(xtable) ; library(kableExtra); library(viridis); library(robomit)

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
  filter(category == "fisc", law != "Staatsschuldwet 1914", house != "Eerste Kamer") %>%
  mutate(lawkind = if_else(str_detect(law, "Successie"), "Successie", "Inkomsten"))
suffrage <- datasets %>%
  filter(category == "suffrage")
govtint <- datasets %>%
  filter(category == "govtint")
fiscal_iv <- datasets %>%
  filter(category == "fisc_iv", law != "Staatsschuldwet 1914") %>%
  mutate(lawkind = if_else(str_detect(law, "Successie"), "Successie", "Inkomsten"))


## Models fiscal laws

model1 <- lm(data = fiscal, formula = vote ~ law + class)
model2 <- lm(data = fiscal, formula = vote ~ log(1+wealth_timevote):lawkind + law + class)
model3 <- lm(data = fiscal, formula = vote ~ log(1+wealth_timevote) + 
               strikes + tvs + turnout + tenure + socialistdum + rk_pct + class + law)
model4 <- lm(data = fiscal, formula = vote ~ log(1+wealth_timevote):lawkind + 
               strikes + tvs + turnout + tenure + socialistdum + hervormd_pct +
             industry_share + percentage_aangesl + class)


## Models suffrage laws
model5 <- lm(data = suffrage, formula = vote ~ law + class)
model6 <- lm(data = suffrage, formula = vote ~ log(1+wealth_timevote) + law + class)
model7 <- lm(data = suffrage, formula = vote ~ log(1+wealth_timevote) + 
               strikes + tvs + turnout + tenure + socialistdum + rk_pct + class + law)
model8 <- lm(data = suffrage, formula = vote ~ log(1+wealth_timevote):law + 
               strikes + tvs + turnout + tenure + socialistdum + rk_pct +
               agricul_share + percentage_aangesl + class + law)


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
