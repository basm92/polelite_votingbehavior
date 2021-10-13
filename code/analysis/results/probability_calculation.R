# probability_calculation

## Two functions, one for scaling the probabilities
## One for giving wealth bonuses and simulating counterfactuals

library(poibin)

ihs <- function(x) { log(x + sqrt(x^2 + 1))}

winsorize <- function(x){
    x[x< 0.05] <- 0.05
    x[x > 0.95] <- 0.95
    return(x)
}

# Load only the models which deliver spectacular predictions
model_ols <- readRDS("./figures/model_ols.RDS")
model_ols_data <- readRDS("./figures/model_ols_data.RDS") %>%
    filter(class != "neutral")

model_endog_ols <- readRDS("./figures/model_endog_ols.RDS")
model_endog_ols_data <- readRDS("./figures/model_endog_ols_data.RDS") %>%
    filter(class != "neutral")

get_probabilities <- function(model, data, lawname, alpha_start = 1, alpha_end = 10){
    
    out <- data.frame(wealth = seq(alpha_start, alpha_end, 0.5),
               probability = rep(0, length(seq(alpha_start, alpha_end, 0.5))))
    
    alpha <- alpha_start 
    
    while(alpha <= alpha_end){
    
    probs <- modelr::add_predictions(data %>%
                                filter(law == lawname) %>%
                                mutate(wealth_timevote = alpha*wealth_timevote), model) %>%
            select(pred) %>%
            pull() %>%
        winsorize()
    
    probs <- probs[!is.na(probs)]
    
    n <- model$model %>%
        filter(law == lawname) %>%
        nrow()
    k <- dplyr::if_else(n %% 2 == 0, n/2 + 1, round(n/2, 0))

    out[out$wealth == alpha, 2] <- 1 - poibin::ppoibin(kk = k, pp = probs)
    
    alpha <- alpha + 0.5
    
    }
    
    return(out)
}

get_probabilities2 <- function(model, data, lawname, beta_start = 1e5, beta_end = 1e6){
    
    out <- data.frame(wealth = seq(beta_start, beta_end, 5e4),
                      probability = rep(0, length(seq(beta_start, beta_end, 5e4))))
    
    beta <- beta_start
    
    while(beta <= beta_end){
        
        probs <- modelr::add_predictions(data %>%
                                             filter(law == lawname) %>%
                                             mutate(wealth_timevote = beta + wealth_timevote), model) %>%
            select(pred) %>%
            pull() %>%
            winsorize()
        
        probs <- probs[!is.na(probs)]
        
        n <- model$model %>%
            filter(law == lawname) %>%
            nrow()
        k <- dplyr::if_else(n %% 2 == 0, n/2 + 1, round(n/2, 0))
        
        out[out$wealth == beta, 2] <- 1 - poibin::ppoibin(kk = k, pp = probs)
        
        beta <- beta + 5e4
        
    }
    
    return(out)
}


