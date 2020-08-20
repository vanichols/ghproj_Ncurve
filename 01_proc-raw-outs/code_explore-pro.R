# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)
#               6/17/2020 look at soils data
#               8/20/2020 (removed sutherland, checking)

library(tidyverse)
library(plotly)
library(scales)
library(ggridges)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  mutate(rotation2 = ifelse(rotation == "sc", "cs", rotation))


# start exploring ---------------------------------------------------------

#--dist of leaccing at each n rate
rawdat %>% 
  filter(crop == "corn") %>% 
  mutate(nrateF = as.factor(nrate_kgha),
         nrateF2 = factor(nrateF, levels = rev(levels(nrateF)))) %>%
  group_by(nrateF2, rotation2) %>% 
  mutate(mnleach = mean(leaching_kgha),
         rotation3 = recode(rotation2,
                            "cc" = "Corn",
                            "cs" = "Soybean")) %>% 
  ggplot(aes(x = leaching_kgha, y = nrateF2, fill = rotation3)) +
  geom_density_ridges(alpha = 0.5, ) +
  geom_point(aes(x = mnleach, y = nrateF2, color = rotation3), pch = 22, size = 3) +
  scale_color_manual(values = c("gold2", "dodgerblue4")) +
  scale_fill_manual(values = c("gold2", "dodgerblue4")) +
  labs(y = "N Fert Rate (kg/ha)",
         fill = "Previous Crop",
         color = "Previous Crop",
       title = "Leaching from Corn Phase, 159 Site-Year Simulations")


#--compare soy and corn in cs rotation
rawdat %>% 
  filter(rotation2 == "cs") %>% 
  mutate(nrateF = as.factor(nrate_kgha),
         nrateF2 = factor(nrateF, levels = rev(levels(nrateF)))) %>%
  group_by(nrateF2, crop) %>% 
  mutate(mnleach = mean(leaching_kgha)) %>% 
  ggplot(aes(x = leaching_kgha, y = nrateF2, fill = crop)) +
  geom_density_ridges(alpha = 0.5, ) +
  geom_point(aes(x = mnleach, y = nrateF2, color = crop), pch = 22, size = 3) +
  scale_color_manual(values = c("gold2", "green4")) +
  scale_fill_manual(values = c("gold2", "green4")) +
  labs(y = "N Fert Rate (kg/ha)",
       title = "Leaching from Corn/Soybean Phase of CS Rotation, 159 Site-Year Simulations")

#--compare all phases
rawdat %>% 
  mutate(nrateF = as.factor(nrate_kgha),
         nrateF2 = factor(nrateF, levels = rev(levels(nrateF)))) %>%
  group_by(nrateF2, crop, rotation2) %>% 
  mutate(mnleach = mean(leaching_kgha)) %>% 
  ggplot(aes(x = leaching_kgha, y = nrateF2, fill = interaction(crop, rotation2))) +
  geom_density_ridges(alpha = 0.5, ) +
  geom_point(aes(x = mnleach, y = nrateF2), pch = 22, size = 3) +
  labs(title = "Leaching from 159 Site-Year Simulations",
       y = "N Fert Rate (kg/ha)") 

ggsave("01_proc-raw-outs/fig_leach-ggridges.png")
ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_leaching_dist.png")


#--compare yields at different n rates
rawdat %>% 
  filter(crop == "corn") %>% 
  mutate(nrateF = as.factor(nrate_kgha),
         nrateF2 = factor(nrateF, levels = rev(levels(nrateF)))) %>%
  group_by(nrateF2, crop, rotation2) %>% 
  mutate(mnyld = mean(yield_maize_buac),
         rotation3 = recode(rotation2,
                            "cc" = "Corn",
                            "cs" = "Soybean")) %>% 
  ggplot(aes(x = yield_maize_buac, y = nrateF2, fill = rotation3)) +
  geom_density_ridges(alpha = 0.5, ) +
  geom_point(aes(x = mnyld, y = nrateF2), pch = 22, size = 3) +
  scale_color_manual(values = c("gold2", "dodgerblue4")) +
  scale_fill_manual(values = c("gold2", "dodgerblue4")) +
  labs(y = "N Fert Rate (kg/ha)",
       fill = "Previous Crop",
       color = "Previous Crop",
       title = "Corn Yields, 159 Site-Year Simulations")


ggsave("01_proc-raw-outs/fig_yield-ggridges.png")
ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_cornyields_dist.png")


#--calc CVs
rawdat %>% 
  select(year, leaching_kgha, site_id, rotation2) %>% 
  group_by(site_id, rotation2) %>% 
  summarise(mn = mean(leaching_kgha),
            sd = sd(leaching_kgha),
            cv = sd/mn*100,
            stab = 1/cv) %>% 
  ungroup() %>% 
  group_by(rotation2) %>% 
  mutate(stab_rel = stab/max(stab)) %>% 
  ggplot(aes(reorder(site_id, stab_rel), stab_rel)) + 
  geom_col() + 
  coord_flip() +
  facet_grid(.~rotation2) + 
  labs(title = "leaching stability")


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
  ggplot(aes(reorder(site_id, -pct_drain), pct_drain)) + 
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
  geom_jitter(aes(color = site_id), width = 0.2, size = 3, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", size = 1) + 
  labs(x = NULL,
       title = "Some sites just have more rain")

rawdat %>% 
  ggplot(aes(reorder(site_id, -leaching_kgha), leaching_kgha)) + 
  geom_jitter(aes(color = site_id), width = 0.2) +
  stat_summary(fun.data = "mean_cl_boot") + 
  labs(x = NULL,
       title = "Some sites just have less leaching")


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

#--this relationship isn't strong
rawdat %>% 
  ggplot(aes(annual_rain_mm, leaching_kgha)) + 
  geom_point(aes(color = site_id)) + 
  geom_smooth(aes(color = site_id), method = "lm", se = F) + 
  facet_grid(.~rotation)

#--try centering the rainfall
#--it had similar amounts of variation, that's good!
rawdat %>% 
  select(site_id, annual_rain_mm, year) %>% 
  distinct() %>% 
  group_by(site_id) %>% 
  mutate(cannual_rain_mm = scale(annual_rain_mm, center = T)) %>% 
  ggplot(aes(reorder(site_id, -cannual_rain_mm), cannual_rain_mm)) + 
  geom_jitter(aes(color = site_id), width = 0.2) +
  stat_summary(fun.data = "mean_cl_boot")

#--what do the soils data look like?

soi <- read_csv("01_proc-raw-outs/pro_soils-60cm.csv")

soi %>% 
  pivot_longer(bd:paw_mm) %>% 
  ggplot(aes(site_id, value)) + 
  geom_segment(aes(x = site_id, xend = site_id, y = 0, yend = value,
                   color = site_id == "klad")) + 
  geom_point(aes(color = site_id == "klad")) + 
  facet_wrap(~name, scales = "free") + 
  coord_flip()


#--clay amount? No. 

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
rawdat %>%
  ggplot(aes(annual_rain_mm, inseason_rain_mm, group = site_id, color = site_id)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F)


#--leahing vs yield

rawdat %>% 
  ggplot(aes(yield_maize_buac, leaching_kgha)) + 
  geom_point() + 
  facet_wrap(~nrate_kgha)

rawdat %>% 
  ggplot(aes(leaching_kgha, yield_maize_buac)) + 
  geom_point() + 
  facet_wrap(~nrate_kgha)

library(ggbeeswarm)

rawdat %>% 
  ggplot(aes(nrate_kgha, leaching_kgha)) + 
  geom_beeswarm(aes(color = rotation)) +
  coord_flip()


rawdat %>% 
  ggplot(aes(yield_maize_buac, leaching_kgha)) + 
  geom_point(alpha = 0.5, aes(fill = (nrate_kgha)), pch = 21, size = 3) + 
  scale_fill_viridis_c() 


#ggplotly(p = ggplot2::last_plot())

# no3 conc ----------------------------------------------------------------

rawdat %>% 
  select(year, nitrateflow_mg_l, yield_maize_buac, rotation, site_id, rotation2, nrate_kgha) %>% 
  ggplot(aes(nrate_kgha, nitrateflow_mg_l)) + 
  geom_point() + 
  facet_grid(.~rotation2)

#--I guess do the same thing as w/the leaching


# yield scaled leaching ---------------------------------------------------

rawdat %>% 
  mutate(yld_scaled = yield_maize_buac/leaching_kgha,
         leach_scaled = leaching_kgha/yield_maize_buac) %>% 
  ggplot(aes(nrate_kgha, yld_scaled)) + 
  geom_line(aes(group = interaction(site_id, year), color= site_id)) + 
  facet_grid(.~rotation2, scales = "free")


rawdat %>% 
  mutate(yld_scaled = yield_maize_buac/leaching_kgha,
         leach_scaled = leaching_kgha/yield_maize_buac) %>% 
  ggplot(aes(nrate_kgha, leach_scaled)) + 
  geom_line(aes(group = interaction(site_id, year), color= site_id)) + 
  facet_grid(.~rotation2, scales = "free")
