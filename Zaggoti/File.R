library(tidyverse)
library(skimr)
library(ggplot2)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_hospitalizations.csv")

skim(covid_hospitalizations)


covid_hospitalizations <- covid_hospitalizations |> 
  mutate(
    date      = as.Date(sub("T.*", "", date)),         
    vent_ratio = covid_vents / covid_patients,
    icu_load   = covid_icu   / icu_total,               
    icu_slack  = icu_avail   / icu_total,               
    med_slack  = med_avail   / med_total
  )
  
Allegheny <- covid_hospitalizations |>
  filter(county == "Allegheny")

ggplot(Allegheny, aes(date, icu_load)) +
  geom_line(color = "steelblue") +
  labs(title = "ICU load – Allegheny County",
       y = "% ICU beds filled by COVID")



















  







