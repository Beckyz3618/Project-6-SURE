library(tidyverse)
library(scales)   

clean <- covid_hospitalizations |> 
  filter(icu_total > 0,
         icu_avail / icu_total > 0.01,
         covid_patients > 5) |>
  mutate(
    icu_slack  = icu_avail / icu_total,
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
    n         = n(),
    .groups = "drop"
  )

p <- ggplot(line_data, aes(mid_slack, median_v)) +
  annotate("rect", xmin = 0.00, xmax = 0.20,
           ymin = 0, ymax = 0.15,
           fill = "firebrick", alpha = .08) +
  geom_hline(yintercept = median(line_data$median_v), linetype = "dashed",
             colour = "grey50", linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.5,
              colour = "firebrick", se = FALSE, linewidth = 1.2) +
  geom_point(colour = "firebrick", size = 2) +
  scale_x_reverse(
    limits  = c(0.50, 0),     
    breaks  = seq(0, 0.50, by = 0.10),
    labels  = percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    limits  = c(0, 0.15),
    breaks  = seq(0, .15, by = .03),
    labels  = percent_format(accuracy = 1)
  ) +
  labs(
    title    = "Ventilator share increases as ICU slack drops",
    subtitle = "Hospitals in Pennsylvania, Apr 2020 – Dec 2022",
    x        = "ICU slack  (free ICU beds ÷ total ICU beds)",
    y        = "Ventilators per COVID patient"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

 
print(p)
