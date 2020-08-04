# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)
#               6/17/2020 look at soils data

library(tidyverse)
library(plotly)
library(scales)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv")
yldprms <- read_csv("02_fit-curves/fc_blin-yield-parms.csv")
leach_prms <- read_csv("02_fit-curves/fc_blin-leach-parms.csv")


# A param -----------------------------------------------------------------

leach_prms %>% 
  filter(term == "a") %>% 
  ggplot(aes(reorder(site_id, -estimate), estimate)) + 
  geom_boxplot(aes(fill = rotation)) + 
  labs(x = NULL,
       y = "Intercept at 0N",
       title = "Seems to be two groups for the intercept")

ggsave("02_fit-curves/figs_blin-intcpt-2-groups.png")


dasoil <- read_csv("01_proc-raw-outs/pro_soils-60cm.csv")


#--avg ksat 150-390 cm
#--sotiris aridity
#--heather german thing
#--the 0-60cm is fine
#--do a 60-150cm
#--do the same thing for subsoil >150
#--add drainable porosity (heather added it), units are mm/mm, sum
#-----this is just sat-dul
#--paw is dul-ll (not considered when designing drainage things)
#--make sure I did a weighted average!

#--within a site, is weather/weatherxsoil or soil driving more variation
#---think about just the intercept


leach_prms %>%  
  filter(term == "a") %>% 
  group_by(site_id, rotation) %>% 
  summarise(meanA = mean(estimate)) %>% 
  left_join(dasoil %>% 
              pivot_longer(bd:paw_mm)
  ) %>%
  ggplot(aes(value, meanA)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_grid(rotation ~ name, scales = "free") + 
  labs(title = "Ksat most strongly related to intercept value")



leach_prms %>%  
  filter(term == "a") %>% 
  group_by(site_id, rotation) %>% 
  summarise(meanA = mean(estimate)) %>% 
  left_join(dasoil %>% 
              pivot_longer(bd:paw_mm)
  ) %>%
  filter(name == "ks") %>% 
  ggplot(aes(value, meanA)) + 
  geom_point(aes(color = site_id), size = 4) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ rotation, scales = "free") + 
  labs(title = "Ksat most strongly related to intercept value\n
       but remember soil is confounded with mgmt")


# for ex gentry was chisel plowed and gold was modlboard, etc. 
# lots of things were different besides ksat
read_csv("01_proc-raw-outs/pro_site-info.csv") %>% 
  filter(site_id %in% c("gold", "gent"))


# xs parm (pivot points)--------------------------------------------------

yld_xs <-
  yldprms %>% 
  filter(term == "xs") %>% 
  select(site_id, year, rotation, term, estimate) %>% 
  rename(yield_xs = estimate)

leach_xs <- 
  leach_prms %>% 
  filter(term == "xs") %>% 
  select(site_id, year, rotation, term, estimate) %>% 
  rename(leach_xs = estimate)


yld <- 
  yld_xs %>% 
  ggplot(aes(site_id, yield_xs)) + 
  geom_point() + 
  stat_summary(color = "red", size = 2)

lch <- 
  leach_xs %>% 
  ggplot(aes(site_id, leach_xs)) + 
  geom_point() + 
  stat_summary(color = "blue", size = 2)

yld_xs %>% 
  rename(xs = yield_xs) %>%
  mutate(cat = "yield")  %>% 
  bind_rows(leach_xs %>%  rename(xs = leach_xs) %>% mutate(cat = "leach")) %>% 
  ggplot(aes(site_id, xs, color = cat)) +
  geom_jitter() +
  stat_summary(size = 2) + 
  facet_grid(.~rotation) + 
  labs(title = "comparing leaching and yield breakpoints")

ggsave("02_fit-curves/figs_xs.png")



library(patchwork)
yld /lch

yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) %>%
  arrange(yld_to_lch) %>% 
  mutate(site_id = as.factor(site_id)) %>% 
  ggplot(aes(reorder(site_id, yld_to_lch), yld_to_lch, 
             fill = rotation)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(pch = 21, position = position_jitterdodge()) +
  geom_hline(yintercept = 0) +
  geom_label(aes(label = year), position = position_jitterdodge()) +
  labs(title = "Pos means leach_xs is higher than yield_xs")


site_id_lvls <- 
  yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) %>%
  select(site_id, year, rotation, yld_to_lch) %>% 
  pivot_wider(names_from = rotation, values_from = yld_to_lch) %>% 
  mutate(rot_dif = cc -cs) %>% 
  group_by(site_id) %>% 
  summarise(rot_dif = mean(rot_dif, na.rm = T)) %>% 
  arrange(rot_dif) %>% 
  pull(site_id)

yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) %>%
  mutate(site_id = factor(site_id, levels = site_id_lvls)) %>% 
  ggplot(aes(site_id, yld_to_lch, 
             fill = rotation)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(pch = 21, position = position_jitterdodge()) +
  geom_hline(yintercept = 0) +
  labs(title = "Pos means leach_xs is higher than yield_xs")


yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) %>%
  mutate(site_id = factor(site_id, levels = rev(site_id_lvls))) %>% 
  ggplot(aes(site_id, yld_to_lch, 
             fill = rotation)) + 
  stat_summary(pch = 21) +
  geom_hline(yintercept = 0) +
  labs(title = "Pos means leach_xs is higher than yield_xs",
       x = NULL,
       y = "N application (kg/ha) buffer between\n
       yield plateua and leaching dam breach") + 
  coord_flip()


#--why doesn't gentry cs xs differ from cs
#--not many converged. 
yld_xs %>% 
  left_join(leach_xs) %>% 
  filter(site_id == "gent") %>% 
  filter(rotation == "cs")
  
yld_xs %>% 
  #filter(site_id != "suth") %>% 
  left_join(leach_xs) %>% 
  ggplot(aes(yield_xs, leach_xs)) + 
  geom_point(aes(color = site_id), size = 3) + 
  #geom_abline() +
  geom_smooth(method = "lm", color = "black", se = F) +
  facet_grid(rotation ~ site_id, scale = "free") + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Leaching pivot point vs yld pivot point")

ggsave("02_fit-curves/figs_leach-xs-vs-yld-xs.png")

yld_xs %>% 
#  filter(rotation != "cs") %>% 
  left_join(leach_xs) %>%
  # group_by(site_id, rotation) %>% 
  # summarise(yield_xs = mean(yield_xs, na.rm = T),
  #           leach_xs = mean(leach_xs, na.rm = T)) %>% 
  #           
  ggplot(aes(yield_xs, leach_xs)) + 
  geom_point(aes(color = rotation), size = 3) + 
  #geom_abline() +
  geom_smooth(method = "lm", color = "black", se = F) +
  facet_grid(rotation ~ ., scale = "free") + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Leaching pivot point vs yld pivot point")


#--sutherland yields?

rawdat %>% 
  filter(site_id == "suth") %>% 
  ggplot(aes(nrate_kgha, yield_maize_buac)) + 
  geom_point() 
yld_xs %>% 
  ggplot(aes(site_id, yield_xs)) + 
  geom_point(aes(color = site_id), size = 3) + 
  facet_grid(.~rotation) + 
  scale_color_brewer(palette = "Set1")
