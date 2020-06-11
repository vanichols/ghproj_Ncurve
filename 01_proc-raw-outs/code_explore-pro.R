# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)

library(tidyverse)
library(plotly)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv")


rawdat %>% 
  ggplot(aes(annual_rain_mm, drainage_mm, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

rawdat %>% 
  ggplot(aes(inseason_rain_mm, drainage_mm, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

#--drainage vs inseason or annual rain
rawdat %>%
  select(site_id, year, annual_rain_mm, inseason_rain_mm, drainage_mm) %>% 
  pivot_longer(annual_rain_mm:inseason_rain_mm) %>% 
  ggplot(aes(value, drainage_mm, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_grid(.~name, scales = "free")

ggsave("01_proc-raw-outs/fig_drain-vs-rain.png")

ggplotly(p = ggplot2::last_plot())

#--leaching vs inseason or annual rain
rawdat %>%
  select(site_id, year, nrate_kgha, annual_rain_mm, inseason_rain_mm, leaching_kgha) %>% 
  pivot_longer(annual_rain_mm:inseason_rain_mm) %>% 
  ggplot(aes(value, leaching_kgha, group = site_id, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_grid(name ~ nrate_kgha, scales = "free")

ggsave("01_proc-raw-outs/fig_leach-vs-rain.png")


#--something definitely wrong w/hoff
rawdat %>% 
  ggplot(aes(annual_rain_mm, inseason_rain_mm, group = site_id, color = site_id)) + 
  geom_jitter() + 
  geom_smooth(method = "lm", se = F)

ggplotly(p = ggplot2::last_plot())
