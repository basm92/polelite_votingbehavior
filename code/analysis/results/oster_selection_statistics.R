# oster_selection_statistics
library(tidyverse); library(robomit); library(ivreg)

ihs <- function(x) { log(x + sqrt(x^2 + 1))}
logtrans <- function(x) {log (1+x)}
    
frisch_waugh_lovell <- function(data, ivs, dv, controls, trans = function(x) x){
    
    # make the variable of interest
    dataset <- data %>%
        filter(!is.na( get(dv) ) ) %>%
        mutate(variable_of_interest = trans(!!rlang::sym(ivs[1]))) %>%
        filter(!is.na(variable_of_interest))
    
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

## New implementation
# the max R2 should be x% of the residual variation of about 65%: 1*0.35 + R2max*0.65 = 0.7
compute_delta <- function(dataframe, 
                          main_iv, 
                          other_ivs, 
                          instrument,
                          dv,
                          trans_iv,
                          prtlout, 
                          first_stage = "small",
                          ...){
    
    step1 <- frisch_waugh_lovell(dataframe,
                                 ivs = c(main_iv, other_ivs, instrument),
                                 dv = dv,
                                 trans = trans_iv, 
                                 controls = prtlout)
    
    instrument_and_ivs <- paste("resid_iv_", c(instrument, other_ivs), sep = "")
    
    if(first_stage == "small"){
        
        firststage <- lm(data = step1, formula = as.formula(
            paste("resid_var_of_interest ~ ", instrument_and_ivs[1]))) %>%
            predict()
        
        
    } else {
    
    firststage <- lm(data = step1,
                     formula = as.formula(
                         paste("resid_var_of_interest ~ ",
                               paste(instrument_and_ivs, collapse="+")
                         )
                     )
    ) %>% predict() }
    
    step1$hat_w <- firststage[match(row.names(step1), names(firststage))]
    
    step1 <- step1 %>%
        mutate(hat_w = if_else(is.na(resid_var_of_interest), NaN, hat_w))
    
    out <-  robomit::o_delta(y = "resid_y",
                             x = "hat_w",
                             con = paste(instrument_and_ivs[-1], collapse="+"),
                             type="lm",
                             data=step1,
                             ...)
    
    return(out)
    
    
}

