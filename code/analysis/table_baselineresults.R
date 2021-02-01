# Analysis Script
library(tidyverse)
library(readxl);library(readr)
library(plm);library(stargazer);library(sandwich)


source("./code/helpers/find_demographics_tk.R")
source("./code/helpers/find_demographics_ek.R")
source("./code/helpers/find_district_acode.R")
source("./code/helpers/find_religion.R")
source("./code/helpers/find_strikes.R")
source("./code/helpers/find_econcontrols.R")

source("./code/helpers/find_wealth_timevote.R")

## First, merge all the necessary data

### Politicians and Wealth
df <- read_csv("./data/voting_behavior/votingbehavior_together.csv")

wealth <- read_csv("./data/polid_data/wealth_politicians.csv") %>%
    janitor::clean_names() %>%
    mutate(across(c(re:w_deflated2), as.numeric))

df <- left_join(df, 
          wealth,
          by = c("b1_nummer" = "indexnummer"))

### Politicians and Party
polparty_key <- read_csv("./data/polid_data/key_politicalparty_category.csv")


### Politicians and Demographics
#### Tweede Kamer
polparty_tk <- read_xlsx("./data/polid_data/tk_1815tot1950uu.xlsx")

df_tk <- left_join(df %>%
              filter(house == "Tweede Kamer"), 
          polparty_tk %>%
              janitor::clean_names(),
          by = "b1_nummer") %>%
    select(-c(achternaam:geslacht)) %>%
    left_join(polparty_key,
              by = c("partij_en_fractie_s" = "partys")) %>%
    select(-partij_en_fractie_s)

df_tk <- find_demographics_tk(df_tk)

#### Eerste Kamer
polparty_ek <- read_xlsx("./data/polid_data/ek_1815tot1950uu.xlsx")


df_ek <- left_join(df %>%
              filter(house == "Eerste Kamer"),
          polparty_ek %>%
              janitor::clean_names(),
          by = "b1_nummer") %>%
    select(-c(achternaam:geslacht)) %>%
    left_join(polparty_key,
              by = c("partij_en_fractie_s" = "partys")) %>%
    select(-partij_en_fractie_s)

df_ek <- find_demographics_ek(df_ek)


##So: df_tk = dataframe with all TK laws
####: df_ek = dataframe with all EK laws
# Now, consider all laws which have districts (TK laws except successie 1921)

### Politicians and District controls (TK only)
districtvotes_tk <- df_tk %>%
    filter(law != "Successiewet 1921") %>%
    group_split(law) %>%
    lapply(find_district) %>%
    bind_rows()

#### Find strikes
districtvotes_tk <- districtvotes_tk %>%
    group_split(law) %>%
    lapply(find_strikes) %>%
    bind_rows()


#### Find religion, economy variables (also on the basis of district)

#### Religion
districtvotes_tk <- districtvotes_tk %>%
    group_split(law) %>%
    lapply(find_religion) %>%
    bind_rows()

#### Econ controls
districtvotes_tk <- districtvotes_tk %>%
    group_split(law) %>%
    lapply(find_econcontrols) %>%
    bind_rows()

#### Final variable: electoral controls

#districtvotes_tk %>%
#    group_split(law) %>%
#    lapply(find_elec_controls) %>%
#    bind_rows()

## Now go back to all laws again:
### Calculate the shares, and correct for portfolio distribution
### Bind the data frames together

df <- bind_rows(districtvotes_tk,
            df_tk %>%
                filter(law == "Successiewet 1921"),
            df_ek)

#### Calculate the shares at the time of voting per law/house (df)
df <- df %>%
    group_split(house, law) %>%
    lapply(find_wealth_timevote) %>%
    bind_rows()

## Simple districtive statistics table

## Then, simple analysis, no controls

model1 <- lm(data = df %>%
       filter(house == "Tweede Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class)

model2 <- lm(data = df %>%
       filter(house == "Eerste Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class) 

model3 <- lm(data = df %>%
       filter(house == "Tweede Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class + law)

model4 <- lm(data = df %>%
       filter(house == "Eerste Kamer"),
   formula = vote ~ log(1+wealth_timevote) + class + law) 


stargazer(model1, model2, model3, model4, 
          omit = c("law", "class"),
          omit.labels = c("hoi", "bas"))
## Analysis: All Controls

## Analysis: Separate laws

## Analysis: Second order effects

## Analysis: Other Laws
