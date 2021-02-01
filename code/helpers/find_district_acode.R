# Find the district of a politician on the basis of a b1_nummer

## df should have a column called `b1_nummer` and `date`
## this should happen separately for each roll call vote
## i.e. rollcallvotes %>%
## split(law) %>%
## lapply(find_distrcit) ## So apply find_district to each separate df

## Return: a data frame together with the 
## district name and acode (for map-making purposes)
library(lubridate)
library(tidyverse)
library(stringr)

## Helper functions to find the correct district from a character string such as:
## "1888-1891 voor het kiesdistrict Veendam, 1891-1894 voor het kiesdistrict Zutphen"

robust_str_replace <- function(string) {
    stringr::word(string, start = -3, end = -1) %>%
        stringr::str_replace(pattern = "het kiesdistrict ",
                             replacement = "") %>%
        stringr::str_replace(pattern = "kiesdistrict ", 
                             replacement = "")
}


## This is for the case of a comma
helper_extract_correct_from_string2 <- function(column, date) {
    
    # Small extra helper
    find_dist <- function(numero) {
        abs(numero-date)
    }
    
    newcolumn <- vector(mode = "double", length = length(column))
    
    for(i in 1:length(column)){
        
        if(is.na(column[i])){
            newcolumn[i] <- "hoi"
            next
        }
        
        if(str_detect(column[i], ",") == FALSE){
            newcolumn[i] <- "hoi"
        } else {
        
        number <- stringr::str_split(column[i], pattern = ",")[[1]] %>%
            lapply(str_extract, "[0-9]{4}-[0-9]{4}") %>%
            as.character() %>%
            lapply(str_split, pattern = "-") %>%
            unlist() %>%
            lapply(as.numeric) %>%
            unlist() %>%
            lapply(find_dist) %>%
            unlist() %>%
            which.min()
        
        if(number > 6){
            newcolumn[i] <- stringr::str_split(column[i], 
                                               pattern = ",")[[1]] %>%
                as.character() %>%
                .[4] %>%
                robust_str_replace()
        } else if(number > 4){
            newcolumn[i] <- stringr::str_split(column[i], 
                                               pattern = ",")[[1]] %>%
                as.character() %>%
                .[3] %>%
                robust_str_replace()
        } else if(number > 2){
            newcolumn[i] <- stringr::str_split(column[i], 
                                               pattern = ",")[[1]] %>%
                as.character() %>%
                .[2] %>%
                robust_str_replace()
                
        } else{
            newcolumn[i] <- stringr::str_split(column[i], 
                                               pattern = ",")[[1]] %>%
                as.character() %>%
                .[1] %>%
                robust_str_replace()
        }
        
        }
        
    }
    
    return(newcolumn)
    
}

# Another helper to find a robust string replacer


find_district <- function(df) {
    
    date <- df$date[1] %>%
        str_extract("[0-9]{4}") %>%
        as.numeric()
    
    ## Find the districts in the official tk database
    official_distr_data <- readxl::read_xlsx("./data/polid_data/tk_1815tot1950uu.xlsx", 
                      sheet = 2) %>%
       janitor::clean_names() %>%
        separate(datum, sep = "/", into = c("from", "to")) %>%
        mutate(
            from = lubridate::dmy(from), 
            to = lubridate::dmy(to)
            ) %>%
        filter(is.element(b1_nummer, df$b1_nummer)) %>%
        filter(from < df$date[1], 
               to > df$date[1],
               grepl("Tweede Kamer",
                     waarde))
    
    ## Extract the correct district
    temp <- official_distr_data %>%
        mutate(kiesdistrict = dplyr::case_when(
            is.na(toelichting) ~ tidyr::replace_na(toelichting),
            !grepl(",", toelichting) ~ robust_str_replace(toelichting),
           
            TRUE ~  helper_extract_correct_from_string2(toelichting, date)
        )
    ) %>%
        filter(!is.na(toelichting))## Attempt to match them as well as possible 
    ## with the districts municipality data
    
    updated_df <- left_join(df,temp %>%
                            select(b1_nummer, kiesdistrict),
                        by = "b1_nummer"
    )
    
    return(updated_df)
    
}

