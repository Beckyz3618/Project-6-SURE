library(tidyverse)
library(skimr)
library(ggplot2)
install.packages("hexbin")
library(hexbin)

covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_hospitalizations.csv")

skim(covid_hospitalizations)


vars <- c("icu_load","vent_ratio","icu_slack","med_slack",
          "covid_patients","vents_use","vents")

mat <- covid_hospitalizations |>       
  
filter(icu_total > 0,
       covid_patients > 0,
       med_total > 0) |>               
  
mutate(
  icu_load   = covid_icu   / icu_total,
  date       = as.Date(sub("T.*", "", date)),
  vent_ratio = covid_vents / covid_patients,
  icu_slack  = icu_avail   / icu_total,
  med_slack  = med_avail   / med_total
)|>
select(all_of(vars)) |>
  mutate(across(everything(), ~replace(.x, is.infinite(.x), NA_real_))) |>
  drop_na()


sapply(mat, function(v) sum(!is.finite(v)))  


pca <- prcomp(as.matrix(mat), center = TRUE, scale. = TRUE)

summary(pca)    
biplot(pca, scale = 0)

round(pca$rotation[, 1:2], 2)


clean <- covid_hospitalizations |>
  filter(icu_total       > 0,
         covid_patients  > 5) |>           
  mutate (icu_slack  = icu_avail / icu_total,
    vent_ratio = covid_vents / covid_patients
  ) |>
  drop_na(icu_slack, vent_ratio)


line_data <- clean |> 
  mutate(slack_bin = cut(icu_slack,
                         breaks = seq(0, 0.50, by = 0.02),   
                         include.lowest = TRUE)) |> 
  group_by(slack_bin) |>
  summarise(
    mid_slack = mean(icu_slack),                 
    median_v  = median(vent_ratio),
    q25       = quantile(vent_ratio, .25),
    q75       = quantile(vent_ratio, .75),
    n         = n(),
    .groups = "drop"
  )

p_line <- ggplot(line_data, aes(mid_slack, median_v)) +
  geom_line(colour = "firebrick", linewidth = 1) +
  geom_point(size = 2, colour = "firebrick") +
  scale_x_reverse(labels = scales::percent_format(accuracy = 1),
                  limits = c(0.50, 0)) +              
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.15)) +
  labs(
    title = "Median ventilator share climbs when ICU slack drops",
    x = "ICU slack (available / total ICU beds)",
    y = "Ventilators per COVID patient"
  ) +
  theme_minimal(base_size = 11)

print(p_line)





















  







