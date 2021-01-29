# graph to show govt expenses and gdp growth
# and hopefully get govt expenditure per capita

library(readxl)
library(tidyverse)
library(tabulizer)

# first, figure of government revenues
## source Bos (2006)
data <- readxl::read_xlsx("./data/govt_finances/Langetijdreeksen-overheidsfinancien-mev2018_publicatie.xlsx") %>%
    janitor::remove_empty("rows") %>%
    select(-1) %>%
    pivot_longer(-1, names_to = "year", values_to = "value") %>%
    rename("variable" = "...2") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    janitor::clean_names()

fig1 <- data %>%
    select(year, non_tax_revenue, tax_premium_burden_1814_1939_only_taxes) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year > 1825, year < 1927) %>%
    pivot_longer(-year) %>%
    ggplot(aes(x = year, y = value, group = name, color = name)) + 
    geom_line() +
    ggtitle("Government Revenues 1825-1927") +
    labs(color = "Revenue Source (% GDP)", 
         x = "Year", 
         y = "Revenue (% GDP)") +
    scale_color_discrete(labels = c("Non-Tax Revenue", "Tax Revenue"))


# second, figure of government expenditures per capita and gdp per capita
## Sources: Clio Infra, Smits et al., Dutch GNP 

gdpcap <- Clio::clio_get("GDP per capita") %>%
    filter(country.name == "Netherlands", year > 1825, year < 1927) %>%
    janitor::clean_names() %>%
    select(year, gdp_per_capita)

## parse the population from Smits et al.
url <- "https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.197.6992&rep=rep1&type=pdf"
tableaux <- tabulizer::extract_tables(url, pages = c(122:124), output = "data.frame")

make_nice_df <- function(df){
    df %>%
    slice(-c(1:3)) %>%
        separate(1, into = c('year', 'population', 'no_births', 'no_deaths',
                             'surplus_births', 'birth_rate',
                             'death_rate', 'net_surplus'), sep=" ")
}

population <- lapply(tableaux, make_nice_df) %>%
    purrr::reduce(bind_rows) %>%
    select(year, population) %>%
    mutate(year = as.numeric(year))

# Merge the data and create the graph (figure 2)
data_graph2 <- data %>%
    mutate(year = as.numeric(year)) %>%
    full_join(dplyr::full_join(gdpcap, population), by = "year")

data_graph2 %>%
    select(year, gross_government_expenditure,
           gdp_per_capita,
           population) %>%
    mutate(gross_government_expenditure = 
               as.numeric(gross_government_expenditure),
           population = 
               as.numeric(population)/10^8,
           gge_per_capita = gross_government_expenditure/population
           ) %>%
    select(-c(population, gross_government_expenditure)) %>%
    pivot_longer(-year, names_to = "var", values_to = "value") %>%
    filter(year > 1825, year < 1927) %>%
    ggplot(aes(x = year, y = value, group = var, color = var)) + geom_line()


# Figure 3: differential tax income (smits et al. again)

## Parse the tables

## graph
