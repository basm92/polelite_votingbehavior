library(tidyverse); library(lubridate)

wealth <- readr::read_csv("./data/polid_data/wealth_politicians.csv")

pols <- readxl::read_excel("./data/polid_data/tk_1815tot1950uu.xlsx") %>%
    janitor::clean_names() %>%
    mutate(begin_periode = lubridate::ymd(begin_periode), 
           einde_periode = lubridate::ymd(einde_periode))

date1 <- ymd("1878-06-04")
date2 <- ymd("1893-09-28")
date3 <- ymd("1911-05-18")
date4 <- ymd("1914-12-18")
date5 <- ymd("1914-12-23")
date6 <- ymd("1916-11-29")
date7 <- ymd("1921-06-09")



pols <- pols %>%
    filter(date1 %within% interval(begin_periode, einde_periode) |
               date2 %within% interval(begin_periode, einde_periode) | 
               date3 %within% interval(begin_periode, einde_periode) |
               date4 %within% interval(begin_periode, einde_periode) |
               date5 %within% interval(begin_periode, einde_periode) |
               date6 %within% interval(begin_periode, einde_periode) |
               date7 %within% interval(begin_periode, einde_periode))


intersect(pols$b1_nummer, wealth$indexnummer) %>%
    length()

