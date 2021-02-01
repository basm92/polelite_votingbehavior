#testscript analyze memories
# en maak de deflator beter

setwd("~/Documents/git/polelite_votingbehavior/")
old_wealth <- read_csv("./data/polid_data/archived/wealth_politicians.csv")

old_wealth_w_deflated <- old_wealth %>%
    filter(!W_DEFLATED == "Err:520") %>%
    mutate(across(everything(), ~ str_replace(., "#N/A", "0"))) %>%
    mutate(across(Re:W_DEFLATED2, ~ as.numeric(.)))

old_wealth_clean <- old_wealth_w_deflated %>%
    select(-c(W_DEFLATED, W_DEFLATED2))

#year_of_death <-
tk_yod <- readxl::read_xlsx("./data/polid_data/tk_1815tot1950uu.xlsx", sheet = 2) %>%
    filter(rubriek == 3020) %>%
    janitor::clean_names() %>%
    select(b1_nummer, datum)

ek_yod <-readxl::read_xlsx("./data/polid_data/ek_1815tot1950uu.xlsx", sheet = 2) %>%
    filter(rubriek == 3020) %>%
    janitor::clean_names() %>%
    select(b1_nummer, datum)

year_of_death <- rbind(tk_yod, ek_yod) %>%
    distinct(b1_nummer, .keep_all = TRUE) %>%
    mutate(datum = str_extract(datum, "[0-9]{4}") %>%
               as.numeric())
    

# Add year for deflator
old_wealth_clean <- old_wealth_clean %>%
    left_join(year_of_death,
              by = c("Indexnummer" = "b1_nummer")) %>%
    rename(year_for_deflator = "datum")
    

## Import new data
library(readODS)
setwd("~/Downloads")


# Dataset
new_data <- readODS::read_ods("memories_invoer.ods")

# Var names
code <- readODS::read_ods("memories_invoer.ods", sheet = 2) %>%
    mutate(number = as.character(number))

# Deflator
deflator <- readODS::read_ods("memories_invoer.ods", sheet = 3)

# Make a dataframe
new_data <- new_data %>%
    group_by(Indexnummer, code) %>%
    summarize(total = sum(value, na.rm =T)) %>%
    left_join(code,
              by = c("code" = "number")) %>%
    select(-code) %>%
    pivot_wider(names_from = variable,
                values_from = total) %>%
    mutate(across(Re:Dugobo, ~ replace_na(., 0)))

new_wealth <- bind_rows(old_wealth_clean, new_data)

# Now, compute the deflated wealth in new_wealth:

new_wealth <- new_wealth %>%
    janitor::clean_names() %>%
    rowwise() %>%
    mutate(deflator = deflator$Deflator[match(year_for_deflator, deflator$Year)]) %>%
    mutate(w_deflated = nw0*deflator) 

setwd("~/Documents/git/polelite_votingbehavior/")

readr::write_csv(new_wealth, "./data/polid_data/wealth_politicians.csv")
