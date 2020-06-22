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

#--how much do these curves vary within a site

rawdat %>% 
  ggplot(aes(nrate_kgha, leaching_kgha)) + 
  geom_line(aes(color = as.factor(year)) ) + 
  facet_grid(rotation~site_id) + 
  labs(title = "Even with the variance of years, site provides info?")

#--do some sites just get more rain?

rawdat %>% 
  select(site_id, annual_rain_mm, year) %>% 
  distinct() %>% 
  ggplot(aes(reorder(site_id, -annual_rain_mm), annual_rain_mm)) + 
  geom_jitter(aes(color = site_id), width = 0.2) +
  stat_summary(fun.data = "mean_cl_boot")

#--create an order
rain_rank <- 
  rawdat %>% 
  select(site_id, annual_rain_mm, year) %>% 
  distinct() %>% 
  group_by(site_id) %>% 
  summarise(mn = mean(annual_rain_mm)) %>% 
  arrange(-mn) %>% 
  pull(site_id)
  
#--is the intercept related to rain amount? No. 
rawdat %>% 
  filter(nrate_kgha == 0) %>% 
  mutate(site_id = factor(site_id, levels = rain_rank)) %>% 
  ggplot(aes(as.factor(nrate_kgha), leaching_kgha)) + 
  geom_point() + 
  facet_grid(rotation~site_id) + 
  labs(title = "Rain rank doesn't affect intercept value")


#--clay amount? No. 

soi <- read_csv("01_proc-raw-outs/pro_soils-60cm.csv")
clay_rank <- 
  soi %>% 
  arrange(-clay_pct) %>% 
  pull(site_id)

rawdat %>% 
  filter(nrate_kgha == 0) %>% 
  mutate(site_id = factor(site_id, levels = clay_rank)) %>% 
  ggplot(aes(as.factor(nrate_kgha), leaching_kgha)) + 
  geom_point() + 
  facet_grid(rotation~site_id) + 
  labs(title = "Clay % doesn't affect intercept value")

#--paw? No. 

rawdat %>% 
  filter(nrate_kgha == 0) %>% 
  mutate(site_id = factor(site_id, 
                          levels =
                            soi %>%
                            arrange(-paw_mm) %>%
                            pull(site_id))
         ) %>% 
  ggplot(aes(as.factor(nrate_kgha), leaching_kgha)) + 
  geom_point() + 
  facet_grid(rotation~site_id) + 
  labs(title = "PAW doesn't affect intercept value")

# #--something definitely wrong w/hoff
# rawdat %>% 
#   ggplot(aes(annual_rain_mm, inseason_rain_mm, group = site_id, color = site_id)) + 
#   geom_jitter() + 
#   geom_smooth(method = "lm", se = F)

ggplotly(p = ggplot2::last_plot())
