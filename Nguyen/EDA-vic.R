#------------------------------------------------------------------------------
# reading and previewing data

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


covid_hospitalizations |> summarise_all()

#------------------------------------------------------------------------------
# na values
NA_count = covid_hospitalizations %>% summarise_all(~ sum(is.na(.)))
print(NA_count, width = Inf)
view(NA_count)

# raw covid_hospitalizations 
raw_count = covid_hospitalizations |> summarise_all(~ n())
view(raw_count)

# percentage of NA values in each column
NA_perc = NA_count/raw_count*100
view(NA_perc)

#-------------------------------
'''NA values visualized'''
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

#------------------------------------------------------------------------------
# data manipulation with dplyr

# summarize_all()
