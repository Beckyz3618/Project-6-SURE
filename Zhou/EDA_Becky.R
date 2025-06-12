library(tidyverse)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_hospitalizations.csv")

View(covid_hospitalizations)
select(covid_hospitalizations)
spec(covid_hospitalizations)

slice_head(covid_hospitalizations)
slice_tail(covid_hospitalizations)

covid_hospitalizations |> 
  ggplot(aes(x = covid_icu, y = icu_avail)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "ICU Availability vs. COVID ICU Patients",
       x = "COVID ICU Patients",
       y = "Available ICU Beds")

icu_summary <- covid_hospitalizations |>
  group_by(county) |>
  summarize(
    avg_adult_icu = mean(icu_total, na.rm = TRUE),
    avg_ped_icu = mean(pic_total, na.rm = TRUE)
  )

icu_summary <- covid_hospitalizations |>
  group_by(county) |>
  summarize(
    avg_adult_icu = mean(icu_total, na.rm = TRUE),
    avg_ped_icu = mean(pic_total, na.rm = TRUE)
  )

summary()

#covid hospitalizations visualization test


library(tidyverse)
covid_hospitalizations |> 
  group_by(date) |> 
  summarize(avg_covid_patients = mean(covid_patients, na.rm = TRUE)) |> 
  ggplot(aes(x = date, y = avg_covid_patients)) +
  geom_line(color = "steelblue") +
  labs(title = "Average COVID-19 Hospitalizations Over Time (PA Counties)",
       x = "Date",
       y = "Avg. COVID-19 Patients Hospitalized")

# Columns we want to exclude from the NA check
excluded_cols <- c("latitude", "longitude", "county_fips")

covid_hospitalizations_clean <- covid_hospitalizations |>
  group_by(county) |>
  filter(!all(is.na(across(-all_of(excluded_cols))))) |>
  ungroup()

# extracting rows with at least one missing value
one_NA = covid_hospitalizations[!complete.cases(covid_hospitalizations), ] 
view(one_NA)

vis_miss(one_NA, warn_large_data = FALSE) #15.5% missing

dim(covid_hospitalizations)
dim(one_NA)
dim(covid_hospitalizations) - dim(one_NA) # number of rows with no NA values
(dim(covid_hospitalizations) - dim(one_NA))/dim(covid_hospitalizations) *100 # percentage of rows with no NA values

# no_NA = covid_hospitalizations[complete.cases(covid_hospitalizations), ]
# view(no_NA)

# removing columns with >50% of null values
NA_perc # null values percentage
df_cleaned_column = subset(covid_hospitalizations, select = -c(pic_percent, ped_percent))
view(df_cleaned_column)

# ============================================================================
#  COVID HOSPITALIZATION VISUALIZATION
# ============================================================================

# Create a comprehensive visualization of the COVID hospitalization data
library(ggplot2)

# 1. Time series of COVID patients over time
time_series_plot <- covid_hospitalizations |>
  group_by(date) |>
  summarize(
    total_covid_patients = sum(covid_patients, na.rm = TRUE),
    total_icu_patients = sum(covid_icu, na.rm = TRUE),
    avg_icu_availability = mean(icu_avail, na.rm = TRUE)
  ) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = total_covid_patients, color = "COVID Patients"), size = 1) +
  geom_line(aes(y = total_icu_patients, color = "ICU Patients"), size = 1) +
  scale_color_manual(values = c("COVID Patients" = "red", "ICU Patients" = "blue")) +
  labs(title = "COVID-19 Hospitalizations Over Time",
       subtitle = "Total patients across all PA counties",
       x = "Date",
       y = "Number of Patients",
       color = "Patient Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom")

print(time_series_plot)

# 2. ICU capacity vs COVID patients scatter plot
capacity_plot <- covid_hospitalizations |>
  ggplot(aes(x = covid_patients, y = icu_total, size = icu_avail, alpha = 0.6)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_size_continuous(name = "Available ICU Beds") +
  labs(title = "ICU Capacity vs COVID Patients",
       subtitle = "Relationship between COVID patients and total ICU capacity",
       x = "COVID Patients",
       y = "Total ICU Beds",
       caption = "Point size shows available ICU beds. Red line shows trend.") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12))

print(capacity_plot)

# 3. Summary statistics
summary_stats <- covid_hospitalizations |>
  summarize(
    total_counties = n_distinct(county),
    total_observations = n(),
    avg_covid_patients = round(mean(covid_patients, na.rm = TRUE), 1),
    avg_icu_total = round(mean(icu_total, na.rm = TRUE), 1),
    avg_icu_available = round(mean(icu_avail, na.rm = TRUE), 1),
    max_covid_patients = max(covid_patients, na.rm = TRUE),
    max_icu_capacity = max(icu_total, na.rm = TRUE)
  )

print(summary_stats)

#------------------------------------------------------------------------------
# cleaning
# Install and load the naniar package
install.packages("naniar")
library(naniar)

# Visualize missing data
vis_miss(df_cleaned_column, warn_large_data = FALSE)

# df_cleaned_column |>
#   group_by(county) |>
#   summarise(across(everything(),mean()))

View(df_cleaned_column)

ls()


dim(df_cleaned_column)


df_with_60_na <- df_cleaned_column[rowMeans(is.na(df_cleaned_column)) > .6, ]
view(df_with_60_na)
dim(df_with_60_na) #7035
# view(count_NA(df_with_60_na))

# df_with_40_60_na <- df_cleaned_column[rowMeans(is.na(df_cleaned_column)) >= .4 & rowMeans(is.na(df_cleaned_column)) <= .6, ]
# view(df_with_40_60_na)
# view(count_NA(df_with_40_60_na))
# dim(df_with_40_60_na) #730

df_with_40_na = df_cleaned_column[rowMeans(is.na(df_cleaned_column)) > .4, ]
(dim(df_with_60_na)/dim(df_cleaned_column))*100 # 10.44% NA values
view(df_with_40_na)

df_clean = df_cleaned_column[rowMeans(is.na(df_cleaned_column)) < 0.4, ]
view(df_clean)
view(count_NA(df_clean))
#------------------------------------------------------------------------------



