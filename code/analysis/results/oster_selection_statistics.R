# oster_selection_statistics
library(tidyverse); library(robomit); library(ivreg)

ihs <- function(x) { log(x + sqrt(x^2 + 1))}
model_iv <- readRDS("./figures/model_iv2.RDS")
model_iv_data <- readRDS("./figures/model_iv2_data.RDS") 
    
frisch_waugh_lovell <- function(data, ivs, dv, controls, trans = function(x) x){
    
    # make the variable of interest
    dataset <- data %>%
        filter(!is.na( get(dv) ) ) %>%
        mutate(variable_of_interest = trans(!!rlang::sym(ivs[1])))
    
    # filter the dataset so that all obs are available
    for(i in ivs){
        dataset <- dataset %>%
            filter(!is.na(!!rlang::sym(i)))
    }
        
    
    for(i in controls){
        dataset <- dataset %>%
            filter(!is.na(!!rlang::sym(i)))
    }
    
    # create the residuals
    formula_partialout_y <- paste(dv, "~", paste(controls, collapse="+"))
    resid_y <- lm(formula = as.formula(formula_partialout_y), data = dataset)$residuals
    
    # put the residuals in a data.frame
    out <- data.frame(resid_y = resid_y)
    
    # create the residuals for first indep. var
    formula_partialout_varofint <- paste('variable_of_interest', "~", paste(controls, collapse="+"))
    resid_var_of_interest <- lm(formula = as.formula(formula_partialout_varofint), data = dataset)$residuals
    
    out['resid_var_of_interest'] <- resid_var_of_interest
    
    # create the residuals for all other indep. vars
    for(i in ivs[-1]){
        formula_partialout_iv <- paste(i, "~", paste(controls, collapse="+"))
        resid_iv <- lm(formula = as.formula(formula_partialout_iv), data = dataset)$residuals
        out[paste('resid_iv_', i, sep = "")] <- resid_iv
    }
    
    return(out)
    
}

# also make this procedure into a function: implement robomit with FWL - IV version

# estimate the first stage regression
step1 <- model_iv_data %>%
    lm(formula = ihs(wealth_timevote) ~ profdummy3 + law + class)
# save and merge fitted values
step2 <- step1$fitted.values
step3 <- merge(model_iv_data, step2, by.x = 0, by.y = 0)

# Make all the variables orthogonal to the law and class fixed effects
step4 <- frisch_waugh_lovell(step3,
                             ivs = c("y",
                                     "tvs", "turnout", "ncm", "tenure", "socialistpercentage", "rk_pct"),
                             dv = "vote",
                             controls = c("law", "class"))
# use robomit o_delta on the partialed out data - same coefficients and relative r-sq. changes
step5 <- robomit::o_delta(y = "resid_y",
                          x = "resid_var_of_interest",
                          con = "resid_iv_tvs+resid_iv_turnout+resid_iv_ncm+resid_iv_tenure+resid_iv_socialistpercentage+resid_iv_rk_pct",
                          R2max = 0.8,
                          type="lm",
                          data=step4)

## also make this procedure into a function: use robomit o_delta for linear models
# make all variables orthogonal to law and class fixed effects
step1 <- frisch_waugh_lovell(model_iv_data, 
                    ivs = c("wealth_timevote", "tvs", "strikes", "tenure", "turnout", "rk_pct"),
                    dv = "vote",
                    controls = c("law", "class"),
                    trans = ihs)
step2 <- robomit::o_delta(y = "resid_y",
                          x = "resid_var_of_interest",
                          con = "resid_iv_tvs+resid_iv_strikes+resid_iv_tenure+resid_iv_turnout+resid_iv_rk_pct",
                          R2max = 0.8,
                          type = "lm",
                          data=step1)


           