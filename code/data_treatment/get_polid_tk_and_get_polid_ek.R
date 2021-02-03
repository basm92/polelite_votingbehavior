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