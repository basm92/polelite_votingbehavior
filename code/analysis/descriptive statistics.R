# Descriptive statistics Table
## Import all dataset
library(tidyverse); library(modelsummary)

electoral <- readRDS("./data/datasets/electoral_lower.RDS") %>%
    mutate(category = "Suffrage Extension")
fiscal <- readRDS("./data/datasets/fiscal_lowerandupper.RDS") %>%
    mutate(category = "Fiscal Legislation")

social <- readRDS("./data/datasets/social_lower.RDS") %>%
    mutate(category = "Gov't Intervention")

datasets <- list(electoral, fiscal, social)

datasets <- purrr::map_df(datasets, ~ .x %>%
                  mutate(across(everything(), ~ as.character(.x)))) %>%
    type_convert()

datasets2 <- datasets %>%
    filter(house == "Tweede Kamer", class != "neutral") %>%
    mutate(law = fct_reorder(law, as.numeric(stringr::str_extract(law, "\\d{4}"))))

# Table with percentages
modelsummary::datasummary(data = datasets2, category*law ~ class * vote*Mean *DropEmpty())

# Table with dissent
dissent <- function(x) {
    mean_value <- mean(x, na.rm = TRUE)
    min(length(x[x < mean_value])/length(x), length(x[x >= mean_value])/length(x))
} 

notes <- "Percentage of politicians of each faction having voted against the party line."
knitr::opts_current$set(label = "descriptivestats_dissent")

modelsummary::datasummary(data = datasets2, 
                          (`Category` = category)*(`Law` = law) ~ N*DropEmpty() + vote*(`Dissent` = dissent) *class * DropEmpty(),
                          notes = notes, 
                          title = "Dissent in Voting Behavior in Key Laws",
                          out = "kableExtra",
                          output = "./tables/descriptivestats_dissent.tex"
                          ) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 


## Now the "regular" descriptive statistics
modelsummary::datasummary(data = datasets2, 
                          category*law ~ N * DropEmpty())
