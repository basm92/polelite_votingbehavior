## Helper find_religion(df)

## This function helps to find the religion share on the basis of a dateframe

find_religion <- function(df){
    
    # First, we import the religion dataset and we find the right year
    ## The standard in this dataset is "Den Haag", "Den Bosch"
    ## Change to 's-Gravenhage, 's-Hertogenbosch
    religion_inhabitants <- readr::read_csv("./data/district_data/religion_inhabitants_per_district.csv") %>%
        select(-1) %>%
        janitor::clean_names() %>%
        filter(jaar == 1888) %>%
        mutate(districtname = str_replace_all(districtname, c("Den Haag" = "'s-Gravenhage",
                                                              "Den Bosch" = "'s-Hertogenbosch")
                                              )
               ) %>%
        select(districtname, contains("pct"))
    
    # Now, we turn df$kiesdistrict into a variable matchable with the actual districts
    temp <- df %>%
        select(kiesdistrict) %>%
        mutate(kiesdistrict_corrected = gsub(" IX| III| II| IV| V| VIII| VII| VI| I",
                                             "",
                                             kiesdistrict)) %>%
        left_join(religion_inhabitants,
                  by = c("kiesdistrict_corrected" = "districtname")) %>%
        distinct(kiesdistrict, .keep_all = TRUE)
    
    df %>%
        left_join(temp,
                  by = c("kiesdistrict" = "kiesdistrict")) %>%
        select(-kiesdistrict_corrected)
    
}
