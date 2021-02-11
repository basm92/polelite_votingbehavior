# table to make an acceptance rate per party table
library(readr)
library(readxl)
library(tidyverse)

# import the voting file
votingresults <- read_csv("./data/voting_behavior/votingbehavior_together.csv")

# merge with the politicians data
poldata_tk <-read_xlsx("./data/polid_data/tk_1815tot1950uu.xlsx") %>%
    janitor::clean_names()

poldata_ek <-read_xlsx("./data/polid_data/ek_1815tot1950uu.xlsx") %>%
    janitor::clean_names()

votes_party <- votingresults %>%
    left_join(poldata_tk,
              by = "b1_nummer")

votes_party <- votes_party %>%
    left_join(poldata_ek,
              by = "b1_nummer")

## Remove some unnecessary variables
votes_party <- votes_party %>%
    select(politician:b1_nummer, partij_en_fractie_s.x, partij_en_fractie_s.y) %>%
    mutate(partij = coalesce(partij_en_fractie_s.x, partij_en_fractie_s.y)) %>%
    select(-c(partij_en_fractie_s.x, partij_en_fractie_s.y))


# merge the party data with the "key"
key <- read_csv("./data/polid_data/key_politicalparty_category.csv")

votingdata_w_party <- votes_party %>%
    left_join(key,
              by = c("partij" = "partys"))

# generate the table
## lower house
votingdata_w_party %>%
    filter(house == "Tweede Kamer") %>%
    group_by(law, class) %>%
    summarize(howmanyyes = sum(vote), n = n(), share = howmanyyes/n) %>%
    select(class, share) %>% # SELECT WHICH VARIABLES HERE
    pivot_wider(names_from = class, values_from = share) %>%
    relocate(law, 
             contains("confessional"), 
             contains("liberal"), 
             contains("socialist")) %>%
    arrange(stringr::str_extract(law, "[0-9]+")) %>%
    xtable(caption = "Lower House", row.numbers = F) %>%
    print(include.rownames = FALSE, 
          file = "./tables/descriptive_stats_per_party_lh.tex")

## upper house
votingdata_w_party %>%
    filter(house == "Eerste Kamer") %>%
    group_by(law, class) %>%
    summarize(howmanyyes = sum(vote), n = n(), share = howmanyyes/n) %>%
    select(class, share) %>% # SELECT WHICH VARIABLES HERE
    pivot_wider(names_from = class, values_from = share) %>%
    relocate(law, 
             contains("confessional"), 
             contains("liberal"), 
             contains("socialist"))  %>%
    arrange(stringr::str_extract(law, "[0-9]+")) %>%
    xtable(caption = "Upper House", row.numbers = F) %>%
    print(include.rownames = FALSE, 
          file = "./tables/descriptive_stats_per_party_uh.tex")


## Write this to a small dataset
a <- votingdata_w_party %>%
    filter(house == "Tweede Kamer") %>%
    group_by(law, class) %>%
    summarize(howmanyyes = sum(vote), n = n(), share = howmanyyes/n) %>%
    pivot_wider(names_from = class, values_from = c(howmanyyes, n, share)) %>%
    relocate(law, 
             contains("confessional"), 
             contains("liberal"), 
             contains("socialist")) %>%
    arrange(stringr::str_extract(law, "[0-9]+")) %>%
    mutate(house = "Tweede Kamer")

b <- votingdata_w_party %>%
    filter(house == "Eerste Kamer") %>%
    group_by(law, class) %>%
    summarize(howmanyyes = sum(vote), n = n(), share = howmanyyes/n) %>%
    pivot_wider(names_from = class, values_from = c(howmanyyes, n, share)) %>%
    relocate(law, 
             contains("confessional"), 
             contains("liberal"), 
             contains("socialist"))  %>%
    arrange(stringr::str_extract(law, "[0-9]+")) %>%
    mutate(house = "Eerste Kamer")

bind_rows(a,b) %>%
    write_csv("./data/voting_behavior/voting_behavior_shares_and_amounts.csv")
