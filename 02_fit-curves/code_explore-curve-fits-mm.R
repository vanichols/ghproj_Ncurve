# author: gina
# created: 7/6/2020
# purpose: explore relationship between yield and leaching parms
# last updated: 7/9/2020 fixed coefs
#               8/20/2020 removed sutherland

rm(list = ls())
library(tidyverse)
library(plotly)
library(scales)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv")
yldprms <- read_csv("02_fit-curves/fc_blin-yield-parms-mm.csv")
leach_prms <- read_csv("02_fit-curves/fc_blin-leach-parms-mm.csv")


# xs parm (pivot points)--------------------------------------------------

yld_xs <-
  yldprms %>% 
  pivot_longer(a:xs) %>% 
  filter(name == "xs") %>% 
  rename(yield_xs = value)

#--note leach xs is only specific to rotation
leach_xs <- 
  leach_prms %>%
  pivot_longer(a:c) %>% 
  filter(name == "xs") %>% 
  rename(leach_xs = value) %>% 
  distinct()


# combine them ------------------------------------------------------------

buff <- 
  yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) 


buff %>% 
  group_by(rotation) %>% 
  summarise(mn_buf = mean(yld_to_lch))


anova(lm(yld_to_lch ~ rotation*site, data = buff))



# heather's graphs --------------------------------------------------------

# she wants all of the lines for aonr and leaching


buff %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) %>% 
  ggplot(aes(rot2, yld_to_lch)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(color = "gray80", width = 0.1) + 
  stat_summary(geom = "point", aes(fill = rot2), size = 5, pch = 24 ) +
  scale_color_manual(values = c("Continuous Maize" = "darkblue",
                                "Rotated Maize" = "orange2")) +
  scale_fill_manual(values = c("Continuous Maize" = "darkblue",
                                "Rotated Maize" = "orange2")) +
  labs(color = NULL,
       fill = NULL,
         y = "Difference Between Leaching and Yield Pivot Points (kg N ha-1)",
         x = NULL) +
  guides(color = F,
         fill = F) +
  theme_bw() + 
  theme(legend.position = c(0.2,0.9),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.3)),
        legend.background = element_rect(color = "black"))

ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/manuscript/fig_buffer.png", width = 4)


library(ggrepel)
buff %>% 
  filter(rotation == "cs") %>% 
  ggplot(aes(rotation, yld_to_lch)) + 
  geom_jitter(aes(color = site)) + 
  geom_label_repel(aes(label = site))

buff %>% 
  filter(rotation == "cs") %>% 
  filter(yld_to_lch > 50) %>% 
  filter(yld_to_lch > 60) %>% 
  mutate_if(is.numeric, round, 0)->a

# different ways to look at this ------------------------------------------

yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) %>%
  arrange(yld_to_lch) %>% 
  mutate(site = as.factor(site)) %>% 
  ggplot(aes(reorder(site, yld_to_lch), yld_to_lch, 
             fill = rotation)) + 
  geom_boxplot(alpha = 0.5) +
  geom_point(pch = 21, position = position_jitterdodge()) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(title = "Corn Grown After Soybean Has Larger Buffer\nBetween Yield Plateua and Start of Extreme Leaching",
       y = "Buffer Between Yield Plateu and Leaching Pivot\n(kg N/ha Fertilization)")

yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) %>%
  arrange(yld_to_lch) %>% 
  mutate(site = as.factor(site)) %>% 
  group_by(rotation) %>% 
  mutate(rot_mn = mean(yld_to_lch)) %>% 
  ggplot(aes(rotation, yld_to_lch, fill = rotation)) + 
  geom_point(pch = 19, position = position_jitterdodge(), alpha = 0.5, aes(color = rotation)) +
  geom_segment(aes(xend = rotation, y = 0, yend = rot_mn), size = 1.5) +
  stat_summary(fun = "mean", geom = "point", size = 8, pch = 21, stroke = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("orange", "purple")) +
  scale_fill_manual(values = c("orange", "purple")) +
  coord_flip() +
  labs(title = "Corn Grown After Soybean Has Larger Buffer\nBetween Yield Plateua and Start of Extreme Leaching",
       y = "Buffer Between Yield Plateu and Leaching Pivot\n(kg N/ha Fertilization)") + 
  theme_bw()



yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) %>%
  arrange(yld_to_lch) %>% 
  mutate(site = as.factor(site)) %>%
  group_by(site, rotation) %>% 
  summarise(yld_to_lch = mean(yld_to_lch)) %>% 
  ggplot(aes(reorder(site, yld_to_lch), yld_to_lch, 
             fill = rotation,
             color = rotation)) + 
  geom_col(position = position_dodge2()) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(title = "Corn Grown After Soybean Has Larger Buffer\nBetween Yield Plateua and Start of Extreme Leaching",
       y = "Buffer Between Yield Plateu and Leaching Pivot\n(kg N/ha Fertilization)")


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
