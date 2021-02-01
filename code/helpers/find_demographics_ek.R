# helper: find_demographics_ek

## Return: dataframe with all demographic variables

find_demographics_ek <- function(df_ek) {
    
    career <- read_excel("./data/polid_data/ek_1815tot1950uu.xlsx", sheet = 2)
    
    career <- career %>%
        filter(rubriek == "3010" |rubriek == "3020") %>%
        dplyr::select(c(1:2, 4)) %>%
        janitor::clean_names() %>%
        dplyr::group_split(rubriek)
    
    together <- left_join(df_ek, career[[1]],
                          by = "b1_nummer"
    ) %>% # Birthdate
        left_join(career[[2]],  # Deathdate
                  by = "b1_nummer") %>%
        select(-contains("rubriek")) %>%
        rename("date_of_birth" = "datum.x",
               "date_of_death" = "datum.y") %>% # Make dates with lubridate
        mutate(across(contains("periode"), ymd), across(contains("date_"), dmy),
               date = ymd(date))
    
    data <- together %>%
        mutate(tenure = date - begin_periode,
               age_of_death = date_of_death-date_of_birth,
               age_of_entrance = begin_periode - date_of_birth,
               age_of_vote = date - date_of_birth,
               long_elec_horiz = einde_periode - date
        )
    
    return(data)
    
}
