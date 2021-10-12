# new_descriptive_stats
electoral <- readRDS("./data/datasets/electoral_lower.RDS") %>%
    mutate(category = "Suffrage Extension")
fiscal <- readRDS("./data/datasets/fiscal_lowerandupper.RDS") %>%
    mutate(category = "Fiscal Legislation")

datasets <- list(electoral, fiscal)

datasets <- purrr::map_df(datasets, ~ .x %>%
                              mutate(across(everything(), ~ as.character(.x)))) %>%
    type_convert()

datasets2 <- datasets %>%
    filter(house == "Tweede Kamer", class != "neutral", law != "Staatsschuldwet 1914") %>%
    mutate(check = law,
           law = fct_reorder(law, as.numeric(stringr::str_extract(law, "\\d{4}"))),
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
## Dissent function
dissent <- function(x) {
    mean_value <- mean(x, na.rm = TRUE)
    min(length(x[x < mean_value])/length(x), length(x[x >= mean_value])/length(x))
} 
## median in words function
custom_median <- function(x){
    step1 <- median(x, na.rm = TRUE)
    out <- if_else(step1 == 1, "Pro", if_else(step1 == 0.5, "-", "Con"))
    
    return(out)
}

## accepted or rejected
Status <- function(x){
    step1 <- mean(x, na.rm = TRUE)
    out <- if_else(step1 > 0.5, "Accepted", "Rejected")
    
    return(out)
}

notes <- list("Dissent is defined as the percentage of politicians of each faction having voted against the party line.",
              "Party Line is defined as the median vote per party: 'Pro' if in favor, 'Con' if against. '-' if N.A. or equally split.")

knitr::opts_current$set(label = "descriptivestats_dissent")

modelsummary::datasummary(data = datasets2, 
                          (`Category` = category)*(`Law` = law)*(`Year` = year_law) ~  N*DropEmpty() +
                              (` `=vote*Status)*DropEmpty() + 
                              (vote * (`Party Line` = custom_median) + #*Arguments(fmt="%.0f") + #* Arguments(fmt="%.0f") 
                                   vote * (`Dissent` = dissent))*DropEmpty(empty="-") * class ,
                          sparse_header = TRUE,
                          notes = notes, 
                          title = "Dissent in Voting Behavior in Key Laws",
                          out = "kableExtra",
                          output = "./tables/descriptivestats_dissent.tex"
) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))



## Now the "regular" descriptive statistics
source("./code/analysis/results/get_data.R")

suffrage_iv <- left_join(suffrage, fiscal_iv %>%
                             select(b1_nummer, profdummy3, par_wealth, exp_inherit), 
                         by = "b1_nummer") %>%
    distinct() %>%
    mutate(profdummy3 = profdummy3.y, 
           harnas = if_else((date_of_death - einde_periode)/365 < 2, 1, 0),
           harnas5 = if_else((date_of_death - einde_periode)/365 < 5, 1, 0),
           exp_inherit = exp_inherit.y,
           par_wealth = par_wealth.y)

datasets2 <- bind_rows(fiscal_iv, suffrage_iv) %>%
    filter(category == "fisc_iv" | category == "suffrage", 
           house == "Tweede Kamer",
           class != "neutral") %>%
    mutate(
        countsoc = if_else(class == "socialist", 1, 0),
        countconf = if_else(class == "confessional", 1, 0),
        countlib = if_else(class == "liberal", 1, 0), 
        harnas = if_else(harnas == TRUE, 1, 0),
        category = dplyr::case_when(
        category == "suffrage" ~ "Suffrage Extension",
        category == "fisc_iv" ~ "Fiscal Legislation"),
        category = factor(category, 
                          levels = c("Suffrage Extension", 
                                     "Fiscal Legislation")))

nn <- function(x, fmt = 0){
    
    if(length(x[!is.na(x)]) > 0){
        length(x[!is.na(x)]) %>%
            format(digits = fmt)
    } else{
        return("-")
    }
    
}
knitr::opts_current$set(label = "descriptives_all")
modelsummary::datasummary(data = datasets2 ,
                          ((`Vote` = vote) + (`Personal Wealth`=wealth_timevote) + 
                               (`Liberal` = countlib) +
                               (`Confessional` = countconf) +
                               (`Socialist` = countsoc) + 
                               (`% District in Agriculture` =agricul_share) + 
                               (`% District in Industry` = industry_share) + 
                               (`% District in Services` = services_share) + 
                               (`Share District in Tot. Taxes`= aandeel_gem)+ 
                               (`Share Tax Liable in District` = percentage_aangesl) + 
                               (`Number of Strikes`=strikes) + 
                               (`Vote Share` = tvs) + 
                               (`Competed Against Socialist`=socialistdum)+ 
                               (`% Socialist Vote in District`=socialistpercentage) + 
                               (`Turnout` = turnout) + 
                               (`Margin to Nearest Competitor`=ncm) +
                               (`Tenure`=tenure) + 
                               (`Long Electoral Horizon`=long_elec_horiz) +
                               (`Age at Time of Vote`=age_of_vote) + 
                               (`Age at Entry`=age_of_entrance) + 
                               (`% Catholic` = rk_pct) + 
                               (`% Dutch Reformed (Hervormd)` = hervormd_pct) + 
                               (`% Dutch Reformed (Geref.)` = gereformeerd_pct) +
                              (`Harnas 2` = harnas) +
                               (`Harnas 5` = harnas5) +
                               (`Father Politician` = profdummy3) +
                               (`Expected Inheritance` = exp_inherit)
                               ) ~ 
                              category*((`N`=nn*Arguments(fmt = 0)) + Mean + Median + SD)* DropEmpty(),
                          title = "Descriptive Statistics",
                          out = "kableExtra",
                          output = "./tables/descriptivestats_all.tex"
) %>%
    kableExtra::kable_styling(latex_options = c("hold_position","scale_down"))
