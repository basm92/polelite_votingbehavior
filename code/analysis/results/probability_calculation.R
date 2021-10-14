# probability_calculation

## Two functions, one for scaling the probabilities
## One for giving wealth bonuses and simulating counterfactuals

library(poibin); library(tidyverse); library(latex2exp)

ihs <- function(x) { log(x + sqrt(x^2 + 1))}

winsorize <- function(x){
    x[x< 0.05] <- 0.05
    x[x > 0.95] <- 0.95
    return(x)
}

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

create_dataframe <- function(model, data, set_laws){
    
    prob_and_law <- function(loi) {
        get_probabilities(model, data, loi) %>%
            mutate(law = loi)
    }
    
    return(map_df(set_laws, ~ prob_and_law(.x)))
    
}

create_dataframe2 <- function(model, data, set_laws){
    
    prob_and_law <- function(loi) {
        get_probabilities2(model, data, loi) %>%
            mutate(law = loi)
    }
    
    return(map_df(set_laws, ~ prob_and_law(.x)))
    
}


# Load only the models which deliver spectacular predictions
validlaws <- c("Inkomstenbelasting 1872",
  "Inkomstenbelasting 1893",
  "Inkomstenbelasting 1914",
  "Successiewet 1878",
  "Successiewet 1911",
  "Successiewet 1916")

model_endog_ols <- readRDS("./figures/model_endog_ols.RDS")
model_endog_ols_data <- readRDS("./figures/model_endog_ols_data.RDS") %>%
    filter(class != "neutral")

model_iv <- readRDS("./figures/model_iv2.RDS")
model_iv_data <- readRDS("./figures/model_iv2_data.RDS") %>%
    filter(class != "neutral")

model_iv_log <- readRDS("./figures/model_iv2_log.RDS")
model_iv_log_data <- readRDS("./figures/model_iv2_log_data.RDS") %>%
  filter(class != "neutral")

df <- create_dataframe(model_iv, model_iv_data %>%
                           mutate(wealth_timevote = if_else(wealth_timevote > 0,
                                                            wealth_timevote,
                                                            0)), validlaws)

df2 <- create_dataframe(model_endog_ols, model_endog_ols_data %>%
                            mutate(
                                wealth_timevote = if_else(wealth_timevote > 0,
                                                      wealth_timevote,
                                                      0)), validlaws)
           
p1 <- df %>%
    ggplot(aes(x = wealth, y = probability, group = law, shape = law)) + 
    geom_line() + 
    geom_point() +
    xlab(TeX("Scaled Wealth $\\alpha$")) +
    ylab(TeX("P($Vote=1 | \\alpha W, X$)")) +
    scale_shape_discrete(name="Law") + 
    theme_bw() + 
    theme(legend.position = "none") + 
    ggtitle("Panel A: Ihs(Wealth)")

df3 <- create_dataframe(model_iv_log, model_iv_log_data %>%
                           mutate(wealth_timevote = if_else(wealth_timevote > 0,
                                                            wealth_timevote,
                                                            0)), validlaws)
p2 <- df3 %>%
    ggplot(aes(x = wealth, y = probability, group = law, shape = law)) + 
    geom_line() + 
    geom_point() +
    xlab(TeX("Scaled Wealth $\\alpha$")) +
    ylab(TeX("P($Vote=1 | \\alpha W, X$)")) +
    scale_shape_discrete(name="Law") +
    theme_bw() +
    theme(legend.title = element_text(size=8), legend.text=element_text(size=8)) + 
    ggtitle("Panel B: Log(Wealth)")

fig <- cowplot::plot_grid(p1, p2, ncol = 2, rel_widths = c(0.42, 0.58))
cowplot::save_plot(filename = "./figures/interpretation.pdf", fig, base_width= 10, base_height = 5)

