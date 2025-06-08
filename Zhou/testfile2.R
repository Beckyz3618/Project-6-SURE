library(tidyverse)
theme_set(theme_light())
install.packages("taylor")

library(taylor)
names(taylor_all_songs)

taylor_all_songs <- taylor_all_songs |> 
  mutate(duration = duration_ms / 60000)

summary(taylor_all_songs$duration)

sd(taylor_all_songs$duration, na.rm = TRUE)

taylor_all_songs |> 
  ggplot(aes(x = duration)) +
  geom_boxplot() +
  theme(axis.text.y = element_blank())

taylor_all_songs |>
  ggplot(aes(x = duration)) +
  geom_histogram()

install.packages("ggbeeswarm")
library(ggbeeswarm)
taylor_all_songs |> 
  ggplot(aes(x = duration, y = "")) +
  geom_beeswarm(cex = 2)

taylor_all_songs |>
  ggplot(aes(x = duration, y = ""))+
  geom_violin()

taylor_all_songs |>
  ggplot(aes(x = duration, y = "")) +
  geom_violin() +
  geom_boxplot(width = 0.4)

taylor_all_songs |>
  ggplot(aes(x = duration)) +
  stat_ecdf()

taylor_all_songs |>
  ggplot(aes(x = duration)) +
  geom_rug(alpha = 0.5)

taylor_all_songs |>
  ggplot(aes(x = d))

taylor_album_songs |>
  ggplot(aes(x = loudness, y = energy)) +
  geom_point( color = "darkred", size =4, alpha = 0.5)
  geom_smooth()+
  geom_smooth(method = "lm", linewidth = 2)
  
  taylor_all_songs |> 
    ggplot(aes(x = loudness, y = energy)) +
    geom_point(color = "darkred", size = 4, alpha = 0.5) +
    geom_smooth(method = "lm", linewidth = 2)
  
  install.packages("GGally")
  library(GGally)
  taylor_all_songs |> 
    select(danceability, energy, loudness, tempo) |> 
    ggpairs()
  
  taylor_all_songs |> 
    filter(album_name %in% c("Lover", "folklore", "evermore", "Midnights")) |>
    ggplot(aes(x = duration, y = album_name)) +
    geom_violin() +
    geom_boxplot(width = 0.4)
  
 taylor_all_songs |>
   filter(album_name %in% c("Lover", "folklore", "evermore", "Midnights")) |>
   ggplot(aes(x = duration, color = album_name)) +
   stat_ecdf(linewidth = 1) +
   scale_color_albums() +
   theme(legend.position = "bottom")

 install.packages("ggridges") 
 library(ggridges)
 taylor_all_songs |> 
   filter(album_name %in% c("Lover", "folklore", "evermore", "Midnights")) |>
   ggplot(aes(x = duration, y = album_name)) +
   geom_density_ridges(scale = 1)
 scale_fill_albums()

 taylor_all_songs |> 
   filter(album_name %in% c("Lover", "folklore", "evermore", "Midnights")) |>
   ggplot(aes(x = duration, fill = album_name)) +
   geom_histogram(alpha = 0.6, bins = 15) +
   scale_fill_albums()

 taylor_all_songs |> 
   filter(album_name %in% c("Lover", "folklore", "evermore", "Midnights")) |>
   ggplot(aes(x = duration)) +
   geom_histogram(bins = 15) +
   facet_wrap(~ album_name, nrow = 1) 
 