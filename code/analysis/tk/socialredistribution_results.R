#socialintervention results
source("./code/data_treatment/get_data_from_voting_records_df.R")
source("./code/data_treatment/get_polid_tk_and_get_polid_ek.R")

files <- list.files("./data/voting_behavior/lowerhouse/social_redistribution/")

files <- paste0("./data/voting_behavior/lowerhouse/social_redistribution/", 
                list.files("./data/voting_behavior/lowerhouse/social_redistribution/"))


lapply(files, source)

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))
data <- bind_rows(dfs)

data <- data %>%
    group_split(law) %>%
    lapply(get_polid_tk) %>%
    purrr::reduce(bind_rows)

data <- data %>%
    get_dataset_from_initial_voting_records()

## save laws
#list_of_laws_social <- data %>%
data$law %>%
    unique()

#saveRDS(list_of_laws_social, "./figures/list_of_laws_social.RDS")
## Descriptive Statistics

descr_tk <- data %>%
    group_split(law) %>%
    lapply(group_by, vote) %>%
    lapply(summarize, law = law[1], 
           median = median(wealth_timevote, na.rm = T),
           sd = sd(wealth_timevote, na.rm = T)) %>%
    lapply(pivot_wider, names_from = vote, 
           values_from = c(median,sd)) %>%
    purrr::reduce(bind_rows) %>%
    relocate(law,contains("0")) %>%
    rename("Median No" = median_0,
           "Sd No" = sd_0,
           "Median Yes" = median_1,
           "Sd Yes" = sd_1) 


## Regression - baseline

#### Tweede Kamer
baseline <- lm(data = data%>%
                   filter(house == "Tweede Kamer"),
               formula = vote ~ log(1+wealth_timevote) +law + class)

model1 <- update(baseline, . ~ . + strikes) 
model2 <- update(model1, . ~ . + rk_pct)
model3 <- update(model2, . ~ . + industry_share)
model4 <- update(model2, . ~ . + tvs)
model5 <- update(model4, . ~ . + socialistdum)
model6 <- update(model5, . ~ . + tenure)

socred_tk_fullctrls <- list("(1)" = baseline, 
                     "(2)" = model1, 
                     "(3)" = model2, 
                     "(4)" = model3, 
                     "(5)" = model4,
                     "(6)" = model5,
                     "(7)" = model6)

# Save for the comparison of coefficients
saveRDS(socred_tk_fullctrls, "./figures/social_redistribution_regressions.RDS")

gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",      "N",             0,
    "adj.r.squared","Adj. R2", 2)

coefconvert <- c("log(1 + wealth_timevote)" = "Personal Wealth",
                 "strikes" = "Amount of Strikes",
                 "rk_pct" = "% Catholics in district",
                 "industry_share" = "Share Industrial",
                 "tvs" = "Vote Share (% Total)",
                 "socialistdum1" = "Competed Against Socialist",
                 "tenure" = "Tenure"
)

description <- tribble(
    ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7,
    "Party + Law Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
attr(description, 'position') <- c(15,16,17)

knitr::opts_current$set(label = "socialred_ols")

modelsummary(socred_tk_fullctrls, 
             stars=TRUE, 
             vcov = vcovHC,
             gof_map = gm,
             coef_map = coefconvert,
             coef_omit = "Intercept|law|class",
             out = "kableExtra",
             add_rows = description,
             output = "./tables/socialred_ols.tex",
             title = "OLS Estimates of Wealth on the Propensity to Vote for Social Redistribution - Controls",
             notes = list("Heteroskedasticity-robust standard errors in parenthesis. Results for lower house voting outcomes.",
                          "Personal Wealth is defined as log(1+Wealth at Death).",
                          "Vote is defined as 1 if the politician is in favor of the reform, 0 otherwise."
             )) %>%
    kableExtra::kable_styling(latex_options = "hold_position",
                              font_size = 9) 


