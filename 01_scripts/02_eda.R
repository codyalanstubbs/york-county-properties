library(googlesheets4)
library(googledrive)

table <- "https://docs.google.com/spreadsheets/d/1Kig0CtHJrGr-cl5rHW0I8PqsqhEBFq_IVNAV9YGSTZ0/edit?usp=sharing"

data <- read_sheet(table)

nobldng <- data %>% filter(res_living_area == 0, price < 5000)
nobldng_classf <- nobldng %>% filter(class == "F") 
nobldng_classr <- nobldng %>% filter(class == "R") 

# Explore ---
nobldng %>%
        group_by(class, utility) %>% 
        count(.)

# Visualizations ----

nobldng %>% filter(utility == "Well, Septic")

nobldng_classf %>% 
        filter(acres <= 20, acres >= 10) %>% 
        pivot_longer(cols = c("aprtotal", "acres"), names_to = "metrics", values_to = "values") %>% 
        ggplot(aes(y = values)) +
        geom_histogram() +
        facet_wrap(. ~ metrics, scales = "free_y")
