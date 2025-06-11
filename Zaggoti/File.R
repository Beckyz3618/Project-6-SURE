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





covid_clean <- covid_hospitalizations |>
  filter(!is.na(icu_load), icu_total > 0)          

monthly_peak <- covid_clean |>
  mutate(month = as.Date(format(date, "%Y-%m-01"))) |>
  group_by(county, month) |>
  summarise(
    peak_icu = max(icu_load),                          
    latitude = first(latitude),
    .groups  = "drop"
  )


county_order <- monthly_peak |> 
  distinct(county, latitude) |>
  arrange(latitude) |> 
  pull(county)


heat_geo <- ggplot(monthly_peak,
                   aes(month,
                       factor(county, levels = county_order),
                       fill = peak_icu)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Monthly peak ICU load",
                       limits = c(0, 1), labels = scales::percent) +
  labs(title = "COVID ICU pressure reached SE PA first",
       x = NULL, y = NULL,
       subtitle = "Counties ordered south (bottom) to north (top)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 6))

heat_geo        















  







