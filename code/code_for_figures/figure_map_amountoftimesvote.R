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

succesiewet1878 <- df %>%
    filter(law == "Successiewet 1878") %>%
    group_by(kiesdistrict) %>%
    summarize(vote = sum(vote)/ n())

### Import the district-to-municipality data
munic_distr <- readr::read_csv("./data/district_data/Municipalities_and_districts.csv") %>%
    select(-1) %>%
    select(gemeente, district, `1850`) %>%
    filter(!is.na(`1850`))

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


# Plot the results
plot_sw1878 <- sf_shp %>%
    ggplot() + geom_sf(aes(fill = vote)) + viridis::scale_fill_viridis()

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


# Tomorrow: repeat this procedure for all other laws

