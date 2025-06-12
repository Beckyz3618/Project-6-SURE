# cleaning NA values in df_covid_icu
view(df_covid_icu)

vis_miss(df_covid_icu, warn_large_data = FALSE)
dim(df_covid_icu)

# last 10 rows that have NA value 
df_covid_icu[tail(which(is.na(df_covid_icu$covid_icu)), 10),]

tail(which(is.na(df_covid_icu$covid_icu)),10) #11820 is last one
# which(is.na(df$my_column)) # find rows with NA
# last_10_na_rows <- tail(na_rows, 10) # get index or row num of last 10 NA values of column
# df[last_10_na_rows, ] # view those rows

view(df_covid_icu[11815:11830,]) # after 2020-10-23 has covid_icu count

# grab nrow after 2020-10-23 or row num 11821
df_covid_icu = df_covid_icu[11821:nrow(df_covid_icu),]
view(df_covid_icu)

#------------------------------------------------------------------------------

covid_icu_date = df_covid_icu |>
  group_by(date) |>
  summarize( covid_icu_sum = sum(covid_icu))
# covid icu over time in PA graph
ggplot(covid_icu_date, aes(x = date, y = covid_icu_sum)) +
  geom_line() + 
  ggtitle("COVID-19 Patients in Intensive Care in Pennsylvania Over Time") +
  labs(y = "Number of COVID-19 ICU Patients", x = "Date") 

ggsave("ggsave.png")
?ggsave



