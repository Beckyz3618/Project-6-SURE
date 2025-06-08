library(tidyverse)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_hospitalizations.csv")
head(covid_hospitalizations)
view(covid_hospitalizations)

covid_hospitalizations |>
  ggplot(aes(x=date, y= covid_patients_mean))+
  geom_line()

dim(covid_hospitalizations)
class(covid_hospitalizations)
head(covid_hospitalizations)

table(covid_hospitalizations$county)
summary(covid_hospitalizations$date)

filter(covid_hospitalizations, date == 2022-01-01 & county == Philadelphia)
