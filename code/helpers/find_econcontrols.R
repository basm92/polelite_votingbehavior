## find_econcontrols(df)

## Function
## Merge two dataframes to the df on the basis of district
## The econ databases have 's-Gravenhage and 's-Hertogenbosch
## But they have caps lock, so the df$kiesdistrict variable
## should be transformed to upper case

# This function should also be run on each separate roll call dataframe

# Just take 2 years in the 1st dataset, aandeel in rijksbelastingen (1889) 
# and % aangeslagen Rijksinkomstenbelasting (1933)

# Just take 1 year *1899* in the second dataset

# Note: this function only considers municipalities and no municipality-aggregation per district

find_econcontrols <- function(df) {
    
    ## Load dataset 1
    ds1 <- readr::read_csv("./data/district_data/econ_controls_1.csv") %>%
        filter(year == 1889 | year == 1933) %>%
        select(-c(cbsnr,year, main.cat, acode)) %>%
        janitor::clean_names() %>%
        group_by(naam) %>%
        summarize(
            aandeel_gem = mean(
                aandeel_der_gemeente_in_s_rijks_personele_belasting, 
                na.rm =T),
            percentage_aangesl = mean(
                percent_aangeslagenen_rijksinkomstenbel_33_in_percent_beroepsbev_30,
                na.rm = T)
        )
    
    # Merge with dataframe
    temp <- df %>%
        select(kiesdistrict) %>%
        mutate(kiesdistrict_corrected = toupper(kiesdistrict),
               kiesdistrict_corrected = sub(" IX| III| II| IV| V| VIII| VII| VI| I",
                                  "",
                                  kiesdistrict_corrected)) %>%
        left_join(ds1, by = c("kiesdistrict_corrected" = "naam"))
    
    # Load dataset 2
    ds2 <- readr::read_csv("./data/district_data/econ_controls_2.csv") %>%
        filter(year == 1899) %>%
        select(naam, contains("share")) %>%
        mutate(naam = str_replace(naam, "STAD-", ""))
    # Merge with dataframe
    
    final <- temp %>%
        left_join(ds2,
                  by = c("kiesdistrict_corrected" = "naam")) %>%
        mutate(aandeel_gem = aandeel_gem/100000,
               percentage_aangesl = percentage_aangesl/100) %>%
        distinct(kiesdistrict_corrected, .keep_all = T)
    
    
    df <- df %>%
        left_join(final,
                  by = c("kiesdistrict" = "kiesdistrict"))
    
    return(df)
}
