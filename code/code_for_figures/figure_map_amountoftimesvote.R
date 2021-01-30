# maak een kaart van districten en willingness to accept fiscal laws 
# of course, this can only be done for Tweede Kamer
library(readr)
library(readxl)
library(tidyverse)
library(stringr)
library(stringdist)

source("./code/helpers/find_district_acode.R")

## find the districts of the politicians
df <- read_csv("./data/voting_behavior/votingbehavior_together.csv") %>%
    filter(house == "Tweede Kamer")

df %>%
    group_split(law) %>%
    lapply(find_district)

## find the map on municipality level

## merge the districts to municipalities
## use the conversion from district to muniicpality table

# 