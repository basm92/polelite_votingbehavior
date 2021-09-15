# Descriptive statistics Table
## Import all dataset
library(tidyverse); library(modelsummary); library(gt); library(kableExtra)

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
    mutate(law = fct_reorder(law, as.numeric(stringr::str_extract(law, "\\d{4}"))),
           year_law = stringr::str_extract(law, "[0-9]+"),
           category = factor(category, levels = c("Suffrage Extension", "Gov't Intervention", "Fiscal Legislation")),
           law = forcats::fct_relabel(law, ~ gsub("\\d{4}", "", .x)),
           tenure = tenure/365,
           long_elec_horiz = long_elec_horiz/365,
           age_of_vote = age_of_vote/365,
           age_of_entrance = age_of_entrance/365,
           rk_pct = rk_pct/100,
           hervormd_pct = hervormd_pct/100,
           gereformeerd_pct = gereformeerd_pct/100,
           aandeel_gem = aandeel_gem/100,
           percentage_aangesl = percentage_aangesl/100,
           socialistpercentage = socialistpercentage/100)

# Table with percentages
modelsummary::datasummary(data = datasets2, category*(year_law*law) ~ class * vote*Mean * DropEmpty())

# Table with dissent
dissent <- function(x) {
    out <- 999
    if(!is.na(mean(x, na.rm = TRUE))) {
        mean_value <- mean(x, na.rm = TRUE)
        return(min(length(x[x < mean_value])/length(x), length(x[x >= mean_value])/length(x)))
    }
    return(out)
} 

custom_mean <- function(x){ 
    out <- "999"
    if(!is.na(mean(x, na.rm = TRUE))){ return(mean(x, na.rm = TRUE))}
    return(out)
}

dissent <- function(x) {
        mean_value <- mean(x, na.rm = TRUE)
        min(length(x[x < mean_value])/length(x), length(x[x >= mean_value])/length(x))
} 

notes <- list("Dissent is defined as the percentage of politicians of each faction having voted against the party line.",
              "Party Line is defined as the median vote per party.")

knitr::opts_current$set(label = "descriptivestats_dissent")

modelsummary::datasummary(data = datasets2, 
                          (`Category` = category)*(`Year` = year_law)*(`Law` = law) ~ N*DropEmpty() + 
                              (vote * (`Party Line` = Median)*Arguments(fmt="%.0f") + #* Arguments(fmt="%.0f") 
                                vote * (`Dissent` = dissent))*DropEmpty(empty="-") * class ,
                          notes = notes, 
                          title = "Dissent in Voting Behavior in Key Laws",
                          out = "kableExtra",
                          output = "./tables/descriptivestats_dissent.tex"
                          ) %>%
    #pack_rows("Suffrage Extensions", 1, 5, latex_align = "c") %>%
    #pack_rows("Government Intervention", 6, 18, latex_align = "c") %>%
    #pack_rows("Fiscal Legislation", 18, 22, latex_align = "c") %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))


## Now the "regular" descriptive statistics
### Fiscal
knitr::opts_current$set(label = "descriptives_fiscal")
modelsummary::datasummary(data = datasets2 %>%
                              filter(category == "Fiscal Legislation"), 
                          (wealth_timevote + 
                               agricul_share + industry_share + services_share + aandeel_gem + percentage_aangesl + strikes +
                                     tvs+socialistdum + socialistpercentage + turnout + ncm +
                               tenure + long_elec_horiz + age_of_vote + age_of_entrance + rk_pct + hervormd_pct + gereformeerd_pct) ~ 
                              N * DropEmpty() 
                          + (Mean + Median + SD + Min + Max)* DropEmpty(),
                          title = "Descriptive Statistics: Fiscal Legislation",
                          out = "kableExtra",
                          output = "./tables/descriptivestats_fiscal.tex"
                          ) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 

### Suffrage
knitr::opts_current$set(label = "descriptives_suffrage")
modelsummary::datasummary(data = datasets2 %>%
                              filter(category == "Suffrage Extension"), 
                          (wealth_timevote + 
                               agricul_share + industry_share + services_share + aandeel_gem + percentage_aangesl + strikes +
                               tvs+socialistdum + socialistpercentage + turnout + ncm +
                               tenure + long_elec_horiz + age_of_vote + age_of_entrance + rk_pct + hervormd_pct + gereformeerd_pct) ~ 
                              N * DropEmpty() 
                          + (Mean + Median + SD + Min + Max)* DropEmpty(),
                          title = "Descriptive Statistics: Suffrage Extension",
                          out = "kableExtra",
                          output = "./tables/descriptivestats_suffrage.tex"
                          ) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 

### Govt Intervention

knitr::opts_current$set(label = "descriptives_govtintervention")
modelsummary::datasummary(data = datasets2 %>%
                              filter(category == "Gov't Intervention"), 
                          (wealth_timevote + 
                               agricul_share + industry_share + services_share + aandeel_gem + percentage_aangesl + strikes +
                               tvs+socialistdum + socialistpercentage + turnout + ncm +
                               tenure + long_elec_horiz + age_of_vote + age_of_entrance + rk_pct + hervormd_pct + gereformeerd_pct) ~ 
                              N * DropEmpty() 
                          + (Mean + Median + SD + Min + Max)* DropEmpty(),
                         title = "Descriptive Statistics: Government Intervention",
                          out = "kableExtra",
                         output = "./tables/descriptivestats_govtintervention.tex"
) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 


## Panel Table With All Three of Them


fiscal2 <- modelsummary::datasummary(data = datasets2 %>%
                              filter(category == "Fiscal Legislation"), 
                          (wealth_timevote + 
                               agricul_share + industry_share + services_share + aandeel_gem + percentage_aangesl + strikes +
                               tvs+socialistdum + socialistpercentage + turnout + ncm +
                               tenure + long_elec_horiz + age_of_vote + age_of_entrance + rk_pct + hervormd_pct + gereformeerd_pct) ~ 
                              N * DropEmpty() 
                          + (Mean + Median + SD + Min + Max)* DropEmpty(),
                          title = "Descriptive Statistics: Fiscal Legislation",
                          out = "gt"
) 

suffrage2 <- modelsummary::datasummary(data = datasets2 %>%
                              filter(category == "Suffrage Extension"), 
                          (wealth_timevote + 
                               agricul_share + industry_share + services_share + aandeel_gem + percentage_aangesl + strikes +
                               tvs+socialistdum + socialistpercentage + turnout + ncm +
                               tenure + long_elec_horiz + age_of_vote + age_of_entrance + rk_pct + hervormd_pct + gereformeerd_pct) ~ 
                              N * DropEmpty() 
                          + (Mean + Median + SD + Min + Max)* DropEmpty(),
                          title = "Descriptive Statistics: Suffrage Extension",
                          out = "gt")

govtintervention2 <- modelsummary::datasummary(data = datasets2 %>%
                              filter(category == "Gov't Intervention"), 
                          (wealth_timevote + 
                               agricul_share + industry_share + services_share + aandeel_gem + percentage_aangesl + strikes +
                               tvs+socialistdum + socialistpercentage + turnout + ncm +
                               tenure + long_elec_horiz + age_of_vote + age_of_entrance + rk_pct + hervormd_pct + gereformeerd_pct) ~ 
                              N * DropEmpty() 
                          + (Mean + Median + SD + Min + Max)* DropEmpty(),
                          title = "Descriptive Statistics: Government Intervention",
                          out = "gt")


summarytogether <- gt(rbind(fiscal2$`_data`, govtintervention2$`_data`, suffrage2$`_data`)) %>%
    tab_row_group(label = "Panel C: Suffrage Extension",
                  rows = 39:57) %>%
    tab_row_group(label = "Panel B: Government Intervention",
                  rows = 20:38) %>%
    tab_row_group(label = "Panel A: Fiscal Legislation",
                  rows = 1:19) %>%
    gt::tab_header(title = "Summary Statistics") %>%
    gt::tab_options(table.font.size = "footnotesize")


gtsave(summarytogether, './tables/summary_all_laws_together.tex')    



custom_mean <- function(x){ 
    out <- "N.A."
    if(!is.na(mean(x, na.rm = TRUE))){ return(mean(x, na.rm = TRUE))}
    return(out)
}

datasummary(data = mtcars %>%
                mutate(carb = as.factor(carb), vs = as.factor(vs)), formula = carb ~ vs * wt*custom_mean)


