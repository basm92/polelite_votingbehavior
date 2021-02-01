# find_wealth_timevote(df)

## Function on df ,so every law/house combination separately (via lapply)
## Return df with portfolio shares and wealth at the time of vote

find_wealth_timevote <- function(df) {
    
    # First, compute shares:
    df <- df %>%
        rowwise() %>%
        mutate(total = sum(across(re:misc))) %>%
        mutate(across(re:misc, ~ ./total,  .names = "share_{.col}")) %>%
        mutate(share_re = re / total,
               share_bonds = sum(dugobo, fogobo, duprbo, foprbo)/total,
               share_shares = sum(dush, fosh)/ total, 
               share_domestic = sum(dugobo, duprbo, dush) / total,
        share_foreign = sum(fogobo, foprbo, fosh) / total)
    
    
    # Import the RoRE Dataset and clean it
    roroe <- read_xlsx("./data/roroe/rateofreturnoneveryting_data.xlsx", sheet = "Data") %>%
        group_split(iso == "NLD") %>%
        lapply(select, year, country, iso, bond_rate, housing_rent_rtn, ltrate, eq_tr, safe_tr) %>%
        lapply(mutate, eq_tr = coalesce(eq_tr, safe_tr)) 
    
    # make a foreign portfolio
    weights_20 <- c("DEU", "FRA")
    weights_10 <- c("BEL", "USA", "GBR", "ITA")
    weights_2 <- c("AUS", "CAN", "DNK", "FIN", "JPN", "NOR", "PRT", "ESP", "SWE", "CHE")
    
    hi <- list(weights_20, weights_10, weights_2)
    
    bonds <- map(hi, ~ paste("bond_rate_", .x, sep = ""))
    housing <- map(hi, ~ paste("housing_rent_rtn_", .x, sep = "")) #doesnt exist, so doesnt matter
    prbonds <- map(hi, ~ paste("ltrate_", .x, sep = ""))
    shares <- map(hi, ~ paste("eq_tr_", .x, sep = ""))
    
    
    #compute the foreign returns
    foreign <- roroe[[1]] %>%
        select(-country) %>%
        pivot_wider(names_from = iso, 
                    values_from = c(bond_rate:safe_tr)) %>%
        group_by(year) %>%
        summarize(foreign_bond_ret = 
                      (0.2 * sum(across(bonds[[1]])) + 
                           0.1 * sum(across(bonds[[2]])) +
                           0.05 * sum(across(bonds[[3]]), na.rm = T)),# I think this should be 0.02, change later
                  foreign_prbonds = 
                      ((0.2 * sum(across(prbonds[[1]]), na.rm = T) +
                            0.1 * sum(across(prbonds[[2]]), na.rm = T) +
                            0.05 * sum(across(prbonds[[3]]), na.rm = T)) / 100),
                  
                  foreign_shares = 
                      (0.2 * sum(across(shares[[1]]), na.rm = T) +
                           0.1 * sum(across(shares[[2]]), na.rm = T) +
                           0.05 * sum(across(shares[[3]]), na.rm = T))
        )
    
    #domestic
    domestic <- roroe[[2]] %>%
        select(-country)
    
    #find date of death and compute the YEARS between death and vote
    ## extract date of vote
    date_of_vote <- df %>%
        ungroup() %>%
        select(date) %>%
        pull() %>%
        .[1] %>%
        str_extract("[0-9]{4}") %>%
        as.numeric()
    
    
    years <- df %>% # the data frame is still rowwise
        select(b1_nummer, date_of_death) %>%
        mutate(date_of_death = str_extract(date_of_death, "[0-9]{4}")) %>%
        mutate(diff = list(
            seq(
                from = date_of_vote, # Here is the difference between date and death
                to = date_of_death)
        )
        ) %>%
        select(-date_of_death) %>%
        distinct(b1_nummer, .keep_all = TRUE)
    
    # Merge this with the input dataframe
    temp <- df %>%
        left_join(years, by = "b1_nummer") # Still a rowwise dataframe
    
    temp <- temp %>%
        mutate(foreign_bond_ret = sum(foreign$foreign_bond_ret[which(foreign$year %in% diff)], na.rm = T),
               foreign_shares_ret = sum(foreign$foreign_shares[which(foreign$year %in% diff)], na.rm = T),
               foreign_prbond_ret = sum(foreign$foreign_prbonds[which(foreign$year %in% diff)], na.rm =T),
               dutch_re_ret = sum(domestic$housing_rent_rtn[which(domestic$year %in% diff)], na.rm = T),
               dutch_bond_ret = sum(domestic$bond_rate[which(domestic$year %in% diff)], na.rm = T),
               dutch_prbond_ret = sum(domestic$ltrate[which(domestic$year %in% diff)], na.rm = T)/100,
               dutch_sh_ret = sum(domestic$eq_tr[which(domestic$year %in% diff)], na.rm = T))
    
    ## Finally, compute the net return on the portfolio using the composition variables
    ## Use compound interest exp^-(previously calculated sum) to calculate back in time to the vote
    ## Use NW0 to find the value
    temp <- temp %>%
        ungroup() %>%
        mutate(re_v =  share_re * nw0 * exp(-dutch_re_ret),
               dubo_v = share_dugobo * nw0 * exp(-dutch_bond_ret),
               duprbo_v = share_duprbo * nw0 * exp(-dutch_prbond_ret),
               dush_v = share_dush * nw0 * exp(-dutch_sh_ret),
               fobo_v = share_fogobo * nw0 * exp(-foreign_bond_ret),
               foprbo_v = share_foprbo * nw0 * exp(-foreign_prbond_ret),
               fosh_v = share_fosh * nw0 * exp(-foreign_shares_ret),
               cash_v = share_cash * nw0,
               misc_v = share_misc * nw0, 
               wealth_timevote = rowSums(across(ends_with("_v"), na.rm = T))
        )
    
    #This is the final df
    temp %>%
        select(c(politician:share_foreign), wealth_timevote)
        
    
}
