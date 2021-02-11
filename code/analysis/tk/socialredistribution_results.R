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
    distinct(law) 

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

model_begin <- lm(data = data,
                  formula = vote ~ log(1 + wealth_timevote))
model0 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class)
model1 <- lm(data = data %>%
                 filter(house == "Tweede Kamer"),
             formula = vote ~ log(1+wealth_timevote) + class + law)

stargazer(model_begin, model0, model1,
          covariate.labels = c("Wealth"),
          dep.var.labels = "Vote",
          omit = c("law", "class"),
          omit.stat = c("adj.rsq", "ser","f"),
          add.lines = list(c("Controls", "None", "Party", "Party+Law")
          ),
          #          notes.append = T,
          #          notes = c("Robust standard errors in parentheses"),
          header = F,
          #          font.size = "tiny",
          column.sep.width = "1pt",
          title = "Baseline regressions")

## More extensive analysis
model3 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + law + strikes)
model4 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + law + strikes + rk_pct)
model5 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + 
                 law + strikes + rk_pct + agricul_share)
model6 <- lm(data = data,
             formula = vote ~ log(1+wealth_timevote) + class + law + 
                 strikes + rk_pct + agricul_share + ncm)

#presdata <- list(model3, model4, model5, model6)
#saveRDS(presdata, "./figures/social_redistribution_regressions.RDS")

stargazer(model3, model4, model5, model6,
          covariate.labels = c("Wealth"),
          dep.var.labels = "Vote",
          omit = c("law", "class", "strikes", "rk_pct", "agricul_share", "ncm"),
          omit.stat = c("adj.rsq", "ser","f"),
          add.lines = list(c("Controls", "Strikes", "1+Religion", "2+Economy", "3+Electoral"),
                           c("House", "TK", "TK", "TK", "TK")
          ),
          #          notes.append = T,
          #          notes = c("Robust standard errors in parentheses"),
          header = F,
          #          font.size = "tiny",
          column.sep.width = "1pt",
          title = "Controls included")
