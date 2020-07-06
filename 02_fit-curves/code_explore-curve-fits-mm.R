# author: gina
# created: 7/6/2020
# purpose: explore relationship between yield and leaching parms
# last updated: 

library(tidyverse)
library(plotly)
library(scales)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv")
yldprms <- read_csv("02_fit-curves/fc_blin-ylds-parms-mm.csv") %>% 
  rename(term = term2)
leach_prms <- read_csv("02_fit-curves/fc_blin-leach-parms-mm.csv") %>% 
  rename(term = term2)


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


#--something is wrong......
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
