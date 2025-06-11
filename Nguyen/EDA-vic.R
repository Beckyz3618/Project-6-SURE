#------------------------------------------------------------------------------
# reading and previewing data
library(dplyr)

library(tidyverse)
covid_hospitalizations <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/covid_hospitalizations.csv")
covid_hospitalizations

view(covid_hospitalizations)

covid_hospitalizations |>
  slice_head(n=6)

covid_hospitalizations |>
  slice_tail(n=6)

str(covid_hospitalizations) # column name, data type, inputs for column
summary(covid_hospitalizations) # descriptive stats of all columns


dim(covid_hospitalizations) #rows, columns#

names(covid_hospitalizations) # column names
# https://data.pa.gov/Covid-19/COVID-19-Aggregate-Hospitalizations-Current-Weekly/kayn-sjhx/about_data #


# covid_hospitalizations |> summarise_all() # look at later, errors pops up, what does this code do?

# more summaries
# install.packages("skimr")
library(skimr)
view(skim(covid_hospitalizations))
# view(skim(df_cleaned_column))

#------------------------------------------------------------------------------
# na values

#NA count function
count_NA <- function(df) {
  df %>% summarise_all(~ sum(is.na(.)))}
# to call use "count_NA(df)"

NA_count_og = count_NA(covid_hospitalizations)
view(NA_count_og)

# raw covid_hospitalizations 
count_og = covid_hospitalizations |> summarise_all(~ n())
view(count_og)

# percentage of NA values in each column
NA_perc = NA_count_og/count_og*100
view(NA_perc)

#-------------------------------
####NA values visualized####

# install.packages("visdat")
library(visdat)
vis_miss(covid_hospitalizations, warn_large_data = FALSE) # raw df # NA 14.5% missing

typeof(is.na(covid_hospitalizations))

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

# more summaries
# install.packages("skimr")
library(skimr)
view(skim(covid_hospitalizations))

view(skim(df_cleaned_column))

#------------------------------------------------------------------------------
# grouping by counties 
df_county = df_cleaned_column %>%
  group_by(county) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

#------------------------------------------------------------------------------
# cleaning
vis_miss(df_cleaned_column, warn_large_data = FALSE)

# df_cleaned_column |>
#   group_by(county) |>
#   summarise(across(everything(),mean()))

view(df_county)


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
df_covid_icu = df_clean |>
  select(county, date, covid_icu)

view(df_covid_icu)

vis_miss(df_covid_icu, warn_large_data = FALSE)


  group_by(county) |>
  summarise()

view(df_clean |>
  group_by(county, date)) |>
  summarise(count = n())
  
  
  df_county = df_cleaned_column %>%
    group_by(county) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))