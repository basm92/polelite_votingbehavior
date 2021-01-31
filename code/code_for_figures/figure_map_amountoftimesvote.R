# maak een kaart van districten en willingness to accept fiscal laws 
# of course, this can only be done for Tweede Kamer
library(readr)
library(readxl)
library(tidyverse)
library(stringr)
library(stringdist)
library(rgdal)
library(ggmap)
library(sf)
library(maptools)

source("./code/helpers/find_district_acode.R")

## find the districts of the politicians
df <- read_csv("./data/voting_behavior/votingbehavior_together.csv") %>%
    filter(house == "Tweede Kamer")

df <- df %>%
    group_split(law) %>%
    lapply(find_district) %>%
    purrr::reduce(rbind) %>%
    filter(law != "Successiewet 1921")

## Create the spacial distribution for successiewetten

succesiewet1878 <- df %>%
    filter(law == "Successiewet 1878") %>%
    group_by(kiesdistrict) %>%
    summarize(vote = sum(vote)/ n())

## First, the 1878 one
### Import the district-to-municipality data
munic_distr <- readr::read_csv("./data/district_data/Municipalities_and_districts.csv") %>%
    select(-1) %>%
    select(gemeente, district, `1850`) %>%
    mutate(district = 
               str_replace_all(district,
                               c("Den Haag" = "'s-Gravenhage", "Den Bosch" = "'s-Hertogenbosch")
               )
    ) %>%
    filter(!is.na(`1850`))

### Import the religion per district data
religion_per_dist <- readr::read_csv("./data/district_data/religion_inhabitants_per_district.csv") %>%
    select(-1) %>%
    select(jaar, RK_pct, districtname) %>%
    filter(jaar == "1878") %>%
    mutate(districtname = 
               str_replace_all(districtname,
                               c("Den Haag" = "'s-Gravenhage", "Den Bosch" = "'s-Hertogenbosch")
                               )
    )

## find the map on municipality level
shapefile <- readOGR("./gisfiles/original/gemeenten (update 26-05-2009)/nl_1878.shp")

# Transform to sf (much easier)
sf_shp <- st_as_sf(shapefile)

# Join with first the municipality to district key
sf_shp <- sf_shp%>% 
    left_join(munic_distr, by = c("GM_NAAM" ="gemeente"))

# Join second with district to vote dataset
sf_shp <- sf_shp%>% 
    left_join(succesiewet1878, by = c("district" = "kiesdistrict"))

# Join third with religion per municipality dataset
sf_shp <- sf_shp %>%
    left_join(religion_per_dist, by = c("district" = "districtname"))

# Plot the results
plot_sw1878 <- sf_shp %>%
    ggplot() + geom_sf(aes(fill = vote), size = 0.2) + viridis::scale_fill_viridis()

plot_rel1878 <- sf_shp %>%
    ggplot() + geom_sf(aes(fill = RK_pct), size = 0.2) + viridis::scale_fill_viridis()

# embellish the figure
plot_sw1878 <- plot_sw1878 +
    ggtitle("Successiewet 1878") + theme_minimal() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )

plot_rel1878 <- plot_rel1878 +
    ggtitle("Religious composition in districts (1878)") + theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )


plot_1878 <- cowplot::plot_grid(plot_sw1878, plot_rel1878, nrow = 1)
cowplot::save_plot("./figures/plot_1878.png", plot_1878, base_height = 5, base_width = 10)
saveRDS(plot_1878, "./figures/presentation_map_1878.RDS")





## Now, second successiewet 1911
ib1893 <- df %>%
    filter(law == "Inkomstenbelasting 1893") %>%
    group_by(kiesdistrict) %>%
    summarize(vote = sum(vote)/ n())


## Import the municipality-district key
munic_distr <- readr::read_csv("./data/district_data/Municipalities_and_districts.csv") %>%
    select(-1) %>%
    select(gemeente, district, `1888`) %>%
    mutate(district = 
               str_replace_all(district,
                               c("Den Haag" = "'s-Gravenhage", "Den Bosch" = "'s-Hertogenbosch")
               )
    ) %>%
    filter(!is.na(`1888`))


### Import the religion per district data
religion_per_dist <- readr::read_csv("./data/district_data/religion_inhabitants_per_district.csv") %>%
    select(-1) %>%
    select(jaar, RK_pct, districtname) %>%
    filter(jaar == "1888") %>%
    mutate(districtname = 
               str_replace_all(districtname,
                               c("Den Haag" = "'s-Gravenhage", "Den Bosch" = "'s-Hertogenbosch")
               )
    )

## find the map on municipality level
shapefile <- readOGR("./gisfiles/original/gemeenten (update 26-05-2009)/nl_1878.shp")
# Transform to sf (much easier)
sf_shp <- st_as_sf(shapefile)
# Join with first the municipality to district key
sf_shp <- sf_shp%>% 
    left_join(munic_distr, by = c("GM_NAAM" ="gemeente"))
# Join second with district to vote dataset
sf_shp <- sf_shp%>% 
    left_join(ib1893, by = c("district" = "kiesdistrict"))

# Join third with religion per municipality dataset
sf_shp <- sf_shp %>%
    left_join(religion_per_dist, by = c("district" = "districtname"))

# Plot the results
plot_ib1893 <- sf_shp %>%
    ggplot() + geom_sf(aes(fill = vote), size = 0.2) + viridis::scale_fill_viridis()

plot_rel1893 <- sf_shp %>%
    ggplot() + geom_sf(aes(fill = RK_pct), size = 0.2) + viridis::scale_fill_viridis()


## Embellish and combine
plot_ib1893 <- plot_ib1893 + 
    ggtitle("Income Tax 1893") + theme_minimal() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )

plot_rel1893 <- plot_rel1893 + 
    ggtitle("Religious composition (1888)") + theme_minimal() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )

plot_1893 <- cowplot::plot_grid(plot_ib1893, plot_rel1893, nrow = 1)
cowplot::save_plot("./figures/plot_1893.png", plot_1893, base_height = 5, base_width = 10)
saveRDS(plot_1893, "./figures/presentation_map_1893.RDS")


