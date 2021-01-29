# graph to show govt expenses and gdp growth
# and hopefully get govt expenditure per capita

library(readxl)
library(tidyverse)
library(tabulizer)
library(gridExtra)

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
    ggtitle("Government Revenues By Source (1825-1927)") +
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

fig2 <- data_graph2 %>%
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
    filter(year > 1860, year < 1913) %>%
    ggplot(aes(x = year, y = value, group = var, color = var)) + 
    geom_line() +
    ggtitle("GDP and Gross Gov't Expenditures (1860 - 1913)") +
    labs(color = "Variable", 
         x = "Year", 
         y = "Value (guilders)") +
    scale_color_discrete(labels = c("GDP Per Capita", "Gov't Expenditures Per Capita"))


# Figure 3: differential tax income (Source: Smits et al. again)

## Parse the tables
url <- "https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.197.6992&rep=rep1&type=pdf"
taxes <- tabulizer::extract_tables(url, pages = c(189:191), output = "data.frame")

### Clean the data
taxes[[3]] <- taxes[[3]][-c(1:5),] %>%
    separate(2, into = c('re_wealth', 'transaction_duties',
                         'excises', 'customs_duties',
                         'total_revenue'),
             sep = " ") %>%
    rename("year" = "X") %>%
    mutate(across(everything(), ~ as.numeric(.)))

taxes[[2]] <- taxes[[2]] %>%
    colnames() %>%
    rbind(taxes[[2]]) %>%
    rename('year' = 'X1832', 
           're_wealth' = 'X12.5',
           'transaction_duties'='X3.7',
           'excises'='X16.8',
           'customs_duties' = 'X3.1',
           'total_revenue'='X36.2') %>%
    mutate(across(everything(), ~ str_replace(., "X", "")),
           across(everything(), ~ as.numeric(.)))
 
taxes[[1]] <- taxes[[1]] %>%
    colnames() %>%
    rbind(taxes[[1]]) %>%
    rename('year' = 'X1806',
           're_wealth' = 'X12.1',
           'transaction_duties' = 'X1.6',
           'excises' = 'X20.3',
           'customs_duties' = 'X0.5',
           'total_revenue' = 'X34.4') %>%
    mutate(across(everything(), ~ str_replace(., "X", "")),
           across(everything(), ~ as.numeric(.)))

data_graph3 <- purrr::reduce(taxes, bind_rows)
## graph

fig3 <- data_graph3 %>%
    select(year, re_wealth, total_revenue) %>%
    filter(year > 1860) %>%
    pivot_longer(-year, names_to = "var", values_to = "value") %>%
    ggplot(aes(x = year, y = value, group = var, color = var)) + 
    geom_line() + 
    geom_vline(xintercept = 1893) +
    geom_vline(xintercept = 1911) +
    ggtitle("Government Revenues from Taxes (1860 - 1913)") + 
    labs(color = "Variable", 
         x = "Year", 
         y = "Value (mln guilders)") +
    scale_color_discrete(labels = c("Real Estate & Wealth", "Total Revenue"))

fig1
fig2
fig3

# save the figures
listoffigures <- list(fig1, fig3, fig2)
saveRDS(listoffigures, "./figures/figures_presentation.RDS")

#grid.arrange(fig1, fig3,fig2, nrow = 3)
#plot <- gridExtra::arrangeGrob(fig1, fig3, fig2, nrow = 3)
#ggsave(plot, file = "./figures/govt_setting.png", width = 8, height = 12)

plot <- cowplot::plot_grid(fig1, fig3, fig2,
                   ncol = 1,
                   align = "v")

cowplot::save_plot("./figures/govt_setting.png",
                   ncol = 1,
                   plot, base_height = 12, base_width = 8)
