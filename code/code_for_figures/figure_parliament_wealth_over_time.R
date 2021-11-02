# read wealth and lh
library(readxl)
library(tidyverse)
library(janitor)
library(cowplot)

wealth <- read_csv("./data/polid_data/wealth_politicians.csv") %>%
    clean_names()

lh_parliaments <- read_csv("./data/polid_data/lh_parliaments.csv") %>%
    clean_names() %>%
    select(-1)

lh_parliaments <- left_join(lh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

meanmedw_lh <- lh_parliaments %>%
    group_by(parliament) %>%
    summarize(p50 = median(w_deflated, na.rm = T),
              p25 = quantile(w_deflated, 0.25, na.rm = T),
              p75 = quantile(w_deflated, 0.75, na.rm = T),
              p90 = quantile(w_deflated, 0.90, na.rm = T),
              count = sum(!is.na(w_deflated)))

p1 <- meanmedw_lh %>%
    pivot_longer(c(p25, p50, p75, p90),
                 names_to = "Statistic", 
                 values_to = "Wealth") %>%
    ggplot(aes(x = parliament, 
               y = Wealth, 
               group = Statistic, 
               linetype = Statistic)) + 
    geom_line() + 
    geom_point() +
    theme_minimal() +
    xlab("Parliament") +
    ylab("Wealth (defl. 1900 guilders)") +
        theme(axis.text.x = element_text(angle = 45), 
          text = element_text(size=13),
          legend.position = c(0.9, 0.8),
          #panel.border = element_rect(colour = "black", fill=NA),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")
    ) +
    ggtitle("Distribution of Politicians Avg. Wealth (Lower House)") + 
    scale_y_continuous(labels = scales::number_format(accuracy = 1),
                       limits=c(0,12e5)) +
    guides(linetype=guide_legend(title="Quantile"))

ggplot2::ggsave("./figures/wealth_parl.pdf", p1, width = 8, height = 5)
