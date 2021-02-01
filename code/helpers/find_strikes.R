#### Helper, find amount of strikes in a district in a given year
library(readr); library(tidyverse)


## Function to extract the strikes
## In the data frame should be a district column (varname: kiesdistrict) and a date (date of vote)
## Then, I can find the right year

# This should also be executed per dataframe

find_strikes <- function(df){
    
    strikes <- read_csv("./data/district_data/strikes_municipality_year.csv")
    key <- read_csv("./data/district_data/Municipalities_and_districts.csv") %>%
        select(-1)
    # First, find the appropriate year
    date_of_vote <- df$date[1] %>%
        str_extract("[0-9]{4}") %>%
        as.numeric()
    
    #yearsofkey <- colnames(key)[2:5] %>%
    #    lapply(as.numeric) %>%
    #    unlist()
    
    #column_in_key <- which.min(abs(date_of_vote - yearsofkey)) + 1
    #colnames(key)[column_in_key] <- "year"
    
    #newkey <- key %>%
    #    select(gemeente, year, district) %>%
    #    filter(year == 1)
    
    strikes <- strikes %>%
        janitor::clean_names() %>%
        filter(jaar == date_of_vote - 1) %>%
        group_by(district) %>%
        summarize(strikes = sum(howmuch))
    
    
    # How many strikes in which district?
    finalstrikes <- df %>%
        select(kiesdistrict) %>%
        mutate(kiesdistrict_corrected = gsub(" IX| III| II| IV| V| VIII| VII| VI| I",
                                             "",
                                             kiesdistrict)) %>%
        left_join(strikes, 
                  by = c("kiesdistrict_corrected" =  "district")) %>%
        mutate(strikes = if_else(is.na(strikes), 0, strikes)) %>%
        distinct(kiesdistrict ,.keep_all = TRUE)
    
    df %>%
        left_join(finalstrikes,
                  by = c("kiesdistrict" = "kiesdistrict")) %>%
        select(-kiesdistrict_corrected)
    
}
