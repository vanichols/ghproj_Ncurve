# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)
#               6/17/2020 look at soils data

library(tidyverse)
library(plotly)
library(scales)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  mutate(rotation2 = ifelse(rotation == "sc", "cs", rotation))


rawdat %>% 
  ggplot(aes(annual_rain_mm, drainage_mm, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

rawdat %>% 
  filter(site_id != "klad") %>% #--klad is the only negative slope
  ggplot(aes(inseason_rain_mm, drainage_mm, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

#--% of inseason rain that leaves as drainage
rawdat %>% 
  mutate(pct_drain = drainage_mm/annual_rain_mm) %>% 
  ggplot(aes(site_id, pct_drain)) + 
  geom_boxplot(aes(fill = rotation2)) + 
  labs(x = NULL,
       y = "Percentage of Annual Rain\nLeaving as Drainage",
       title = "Corn Grown After Soybeans Has Less Drainage Compared to Corn After Corn") + 
  scale_y_continuous(labels = scales::label_percent())

ggsave("01_proc-raw-outs/fig_pctdrain-vs-rot.png")                    

#--drainage vs inseason or annual rain
rawdat %>%
  select(site_id, year, annual_rain_mm, inseason_rain_mm, drainage_mm) %>% 
  pivot_longer(annual_rain_mm:inseason_rain_mm) %>% 
  ggplot(aes(value, drainage_mm, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_grid(.~name, scales = "free") + 
  labs(title = "In-season rain less related to drainage\ncompared to annual")

ggsave("01_proc-raw-outs/fig_drain-vs-rain.png")


#--leaching vs inseason or annual rain
rawdat %>%
  select(site_id, year, nrate_kgha, annual_rain_mm, inseason_rain_mm, leaching_kgha) %>% 
  pivot_longer(annual_rain_mm:inseason_rain_mm) %>% 
  ggplot(aes(value, leaching_kgha, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_grid(name ~ nrate_kgha, scales = "free")

ggsave("01_proc-raw-outs/fig_leach-vs-rain.png")

nrates <- rawdat %>% select(nrate_kgha) %>% distinct() %>% pull()

rawdat %>%
  filter(nrate_kgha == nrates[5]) %>% 
  select(site_id, year, nrate_kgha, annual_rain_mm, inseason_rain_mm, leaching_kgha) %>% 
  pivot_longer(annual_rain_mm:inseason_rain_mm) %>% 
  ggplot(aes(value, leaching_kgha, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_grid(name ~ nrate_kgha, scales = "free")


# #--something definitely wrong w/hoff
# rawdat %>% 
#   ggplot(aes(annual_rain_mm, inseason_rain_mm, group = site_id, color = site_id)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm", se = F)

ggplotly(p = ggplot2::last_plot())
