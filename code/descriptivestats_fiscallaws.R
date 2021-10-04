library(tidyverse)
library(stringr)
library(stringdist)
library(lubridate)
library(knitr)
library(xtable)


# This file makes the basic dataset from which we in other scripts extract the 
# variables and perform the analysis
# Here is the place to add new laws to the analysis and supplement them with b1_nummers

# first, lower house

# List files
files <- paste0("data/voting_behavior/lowerhouse/fiscal/", 
                list.files("data/voting_behavior/lowerhouse/fiscal/"))

# Source them (get the dataframes)
lapply(files, source)

# Separately: merge them with data files to get a polid
#df should contain 'date' and 'politician'
get_polid_tk <- function(df){
    
    # get a list of all lower house politicians
    polidlist <- readxl::read_xlsx("./data/polid_data/tk_1815tot1950uu.xlsx", 
                                      sheet = 1) %>%
        janitor::clean_names() %>%
    mutate(begin_periode = ymd(begin_periode), 
           einde_periode = ymd(einde_periode)) %>%
        filter(begin_periode < df$date[1], einde_periode > df$date[1]) %>%
        mutate(voorlachternaam = str_c(voorletters, " ", achternaam),
               achternaam = str_squish(achternaam))
    
    # Just focus on the column of politicians, and make extra columns on that basis
    namestomatch <- df %>%
        select(politician) # This variable should not be mutated, because it is the id
    
    # match them as accurately as possible
    ## first step: if there are dots, match on initials and last name
    ## Order: politician, match, achternaam, b1_nummer
    part1 <- namestomatch %>%
        mutate(politician_int = str_squish(politician)) %>%
        filter(str_detect(politician_int, "\\.")) %>%
        mutate(match = stringdist::amatch(politician_int, 
                                                  polidlist$voorlachternaam, 
                                                  method = "jw",
                                                  maxDist = 5)) %>%
        rowwise() %>%
        mutate(polidlist[match, "achternaam"], polidlist[match, "b1_nummer"])
    
    ## second step: remove suffixes and match on last name
    part2 <- namestomatch %>%
        mutate(politician_int = str_squish(politician)) %>%
        filter(!str_detect(politician_int, "\\.")) %>%
        mutate(politician_int = str_replace(politician, 
                                "Van De |Van Der |van de |van der |van den |van |Van der |Van |de ", "")) %>%
        mutate(match = stringdist::amatch(politician_int, 
                                          polidlist$achternaam,
                                          maxDist = 10)
               ) %>%
        rowwise() %>%
        mutate(polidlist[match, "achternaam"], polidlist[match, "b1_nummer"])
        
        
    # merge them and the matches back into the df
    left_join(df, 
          bind_rows(part1, part2),
          by = "politician"
    ) %>%
        select(-c(politician_int, match, achternaam))
        
}

data <- list(inkomstenbelasting1872, inkomstenbelasting1893, inkomstenbelasting1914, 
             staatsschuldwet1914, successiewet1878, successiewet1911,
             successiewet1916, successiewet1921)

laws <- lapply(data, get_polid_tk)
lowerhouse <- purrr::reduce(laws, bind_rows) %>%
    mutate(vote = as.numeric(vote)) 

# Some descriptives
lowerhouse %>%
    group_by(law) %>%
    summarize(infavor = sum(vote), howmany = n(), percentage = infavor/howmany, 
              date = unique(date)) %>%
    arrange(date) %>%
    xtable(caption = "Lower House", row.numbers = F) %>%
    print(include.rownames = FALSE, 
          file = "./tables/descriptive_stats_fiscal_tk.tex")

## Now, do upper house
# Load in the dataframes
files <- paste0("data/voting_behavior/upperhouse/fiscal/", 
                list.files("data/voting_behavior/upperhouse/fiscal/"))

# Source them (get the dataframes)
lapply(files, source)


# Define function for ek
get_polid_ek <- function(df){
    
    # get a list of all lower house politicians
    polidlist <- readxl::read_xlsx("./data/polid_data/ek_1815tot1950uu.xlsx", 
                                   sheet = 1) %>%
        janitor::clean_names() %>%
        mutate(begin_periode = ymd(begin_periode), 
               einde_periode = ymd(einde_periode)) %>%
        filter(begin_periode < df$date[1], einde_periode > df$date[1]) %>%
        mutate(voorlachternaam = str_c(voorletters, " ", achternaam),
               achternaam = str_squish(achternaam))
    
    # Just focus on the column of politicians, and make extra columns on that basis
    namestomatch <- df %>%
        select(politician) # This variable should not be mutated, because it is the id
    
    # match them as accurately as possible
    ## first step: if there are dots, match on initials and last name
    ## Order: politician, match, achternaam, b1_nummer
    part1 <- namestomatch %>%
        mutate(politician_int = str_squish(politician)) %>%
        filter(str_detect(politician_int, "\\.")) %>%
        mutate(match = stringdist::amatch(politician_int, 
                                          polidlist$voorlachternaam, 
                                          method = "jw",
                                          maxDist = 5)) %>%
        rowwise() %>%
        mutate(polidlist[match, "achternaam"], polidlist[match, "b1_nummer"])
    
    ## second step: remove suffixes and match on last name
    part2 <- namestomatch %>%
        mutate(politician_int = str_squish(politician)) %>%
        filter(!str_detect(politician_int, "\\.")) %>%
        mutate(politician_int = str_replace(politician, 
                                            "Van De |Van Der |van de |van der |van den |van |Van der |Van |de ", "")) %>%
        mutate(match = stringdist::amatch(politician_int, 
                                          polidlist$achternaam,
                                          maxDist = 10)
        ) %>%
        rowwise() %>%
        mutate(polidlist[match, "achternaam"], polidlist[match, "b1_nummer"])
    
    
    # merge them and the matches back into the df
    left_join(df, 
              bind_rows(part1, part2),
              by = "politician"
    ) %>%
        select(-c(politician_int, match, achternaam))
    
}

data <- list(inkomstenbelasting1893_ek, inkomstenbelasting1914_ek, 
             staatsschuldwet1914_ek, successiewet1878_ek, successiewet1911_ek,
             successiewet1916_ek, successiewet1921_ek)

laws <- lapply(data, get_polid_ek)
upperhouse <- purrr::reduce(laws, bind_rows)

# Some descriptives
upperhouse %>%
    mutate(vote = as.numeric(vote)) %>%
    group_by(law) %>%
    summarize(infavor = sum(vote), howmany = n(), percentage = infavor/howmany, 
              date = unique(date)) %>%
    arrange(date) %>%
    xtable(caption = "Upper House", row.numbers = F) %>%
    print(include.rownames = FALSE, 
          file = "./tables/descriptive_stats_fiscal_ek.tex")

# Write a csv file with the data

voting_fiscal_tk_ek <- bind_rows(lowerhouse, upperhouse)
readr::write_csv(voting_fiscal_tk_ek, file = "./data/voting_behavior/votingbehavior_together.csv")
