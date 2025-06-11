library(tidyverse)
library(skimr)
library(ggplot2)
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





























  







