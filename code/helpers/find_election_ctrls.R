# Helper: find_election_controls
## Out: df with election control variables
## In: Dataframe from tweede kamer

## Again, run per dataframe (so per law)
## Additional arguments: minist, decreases sensitivity

find_election_ctrls <- function(df){
    print(df$law[1])
    ## Read in the datasets
    #politicians3 <- read_excel("./Data/tk_1815tot1950uu.xlsx")
    allcandidates <- read.csv("./data/elections/functiondata_allcandidates.csv") %>%
        janitor::clean_names() %>%
        mutate(election_date = ymd(date), 
               district = str_replace_all(district, c("Den Haag" = "'s-Gravenhage",
                                                      "Den Bosch" = "'s-Hertogenbosch"))
               ) %>%
        select(-date)
    
    allelections <- read.csv("./data/elections/functiondata_allelections.csv") %>%
        janitor::clean_names() %>%
        mutate(main = ymd(main), side = ymd(side))
    
    # convert the date to real date
    date <- df$date[1]
    
    # Look up the turnout etc. per district
    ## This part of the function is district-specific
   districtfilter <- allelections %>%
        filter(regio %in% df$kiesdistrict) %>%
        mutate(kandidaat = as.character(kandidaat)) %>%
        group_by(regio, side) %>% # mss hier main, maar mss even testen, no i think this is right
        filter(any(kandidaat != "")) %>%
        group_by(regio) %>%
        mutate(diff = date - side) %>%
        filter(diff > 0) %>% #Because it has to be an election in the past
        slice_min(diff)
   
   district_data <- districtfilter %>%
       filter(regio_uitslag == "Kiesgerechtigden" | regio_uitslag == "Opkomst") %>%
       select(regio, regio_uitslag, aantal_stemmen, side) %>%
       pivot_wider(names_from = regio_uitslag, values_from = aantal_stemmen, values_fn = mean) %>%
       mutate(Turnout = Opkomst/Kiesgerechtigden) %>%
       janitor::clean_names()
   
   
   district_data <- left_join(district_data, allcandidates %>%
       filter(district %in% df$kiesdistrict) %>% #smallcase district and smallcase date are function arguments
       group_by(district) %>%
       mutate(diff = date - election_date) %>%
       filter(diff > 0) %>% # Because the election has to be in the past
       slice_min(diff) %>%
       mutate(name = str_trim(name)) %>%
       group_by(district) %>%
       mutate(socialistdum = ifelse(any(grepl("*.?SDAP*.?", aanbevolen_door)), "1", "0"),
              socialistpercentage = sum(percentage[grepl("*.?SDAP*.?", aanbevolen_door)])) %>%
       select(district, socialistdum, socialistpercentage) %>%
       distinct(),
       by = c("regio" = "district"))
   
   # These can be merged with the df via kiesdistrict = regio 
    
   # nearest competitor margin, amount of votes, total margin of votes
   # This is a politician-level variable: match on the basis of b1_nummer and stringmatch
   #From politicus_naam to all matches in allelections dataset
   # From all matches in allelections to most recent election before df$date
   # from this election and match: compute nearest comp marg, am of votes, 
   # total margin
   
   #From politicus_naam to all matches in allelections dataset
   politicus_naam <- read_xlsx("./data/polid_data/tk_1815tot1950uu.xlsx") %>%
       janitor::clean_names() %>%
       filter(b1_nummer %in% df$b1_nummer) %>%
       unite("truename", voorletters, prepositie, achternaam, 
            na.rm = TRUE, sep = " ") %>%
       select(b1_nummer, truename) 
    
   # Match all politicians and retreive the necessary info within the for loop
   politician_vars <- vector(mode = "list", length = nrow(politicus_naam))
   
   for(i in 1:nrow(politicus_naam)){
       print(politicus_naam[i,2])
       #Create all distances
       distances_to_politician <- stringdist::afind(allelections$kandidaat,
                         politicus_naam[i,2])
           
       a <-  which(distances_to_politician$distance == min(distances_to_politician$distance))
       b <- allelections[a,] 
       
       # From all matches in allelections to most recent election before df$date
       closest_elections_before_date <- allelections %>%
           filter(regio %in% b$regio, 
                  main %in% b$main,
                  side %in% b$side) %>%
           mutate(diff = date - side) %>%
           filter(diff > 0) %>%
           slice_min(diff)
       
       if(nrow(closest_elections_before_date) < 1){
           next
       }
       
       # from this election and match: compute nearest comp marg, am of votes, 
       # total votes
       politician_row <- which.min(stringdist::afind(closest_elections_before_date$kandidaat, 
                         politicus_naam[i,2])$distance)
       
       politician_name <- closest_elections_before_date[politician_row,"kandidaat"]
           
       as <- closest_elections_before_date[politician_row,] %>%
           select(aantal_stemmen)
       
       # nearest competitor margin
       total <- closest_elections_before_date %>%
           filter(kandidaat != "") %>%
           summarize(ncm = sum(aantal_stemmen)) #This is not yet the ncm, but the name
       #needs to be here
       
       temp <- closest_elections_before_date %>%
           filter(kandidaat != "") %>%
           arrange(desc(aantal_stemmen))
       
       ncm <- (temp[which(temp$kandidaat==politician_name),]$aantal_stemmen -
           temp[which(temp$kandidaat==politician_name)+1,]$aantal_stemmen)/total
    
       # total vote share
       tvs = temp[which(temp$kandidaat==politician_name),]$aantal_stemmen/total %>%
           pull()
       
       #combine everything
       politician_vars[[i]] <- data.frame(politicus_naam[i,1], politicus_naam[i,2], as, ncm, tvs)
   }
   
   politician_vars <- politician_vars %>%
       purrr::reduce(bind_rows) %>%
       distinct(b1_nummer, .keep_all = T) %>%
       mutate(ncm = replace_na(ncm, 0))
   
   
   df %>%
       left_join(district_data, 
                 by = c("kiesdistrict" = "regio")) %>%
       left_join(politician_vars,
                 by = "b1_nummer")
           
}
