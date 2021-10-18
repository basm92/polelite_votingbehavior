# create political family dataset
library(tidyverse)
data <- readxl::read_excel("./data/polid_data/tk_1815tot1950uu.xlsx") %>%
    bind_rows(readxl::read_excel("./data/polid_data/ek_1815tot1950uu.xlsx")) %>%
    mutate(begin_period = lubridate::ymd(`begin periode`),
           end_period = lubridate::ymd(`einde periode`)) %>%
    filter(begin_period > '1848-01-01',
           end_period < '1940-01-01')

polfams <- data %>%
    group_by(achternaam) %>%
    count() %>%
    arrange(desc(n)) %>%
    mutate(polfam = if_else(n > 1, 1, 0))

left_join(data, polfams) %>%
    select(`b1-nummer`, n, polfam) %>%
    janitor::clean_names() %>%
    write_csv("./data/polid_data/political_families.csv")
