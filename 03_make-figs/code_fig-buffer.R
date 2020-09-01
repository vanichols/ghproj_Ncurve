# author: gina
# created: 9/1/2020
# purpose: make figs for heather
# last updated: 

rm(list = ls())
library(tidyverse)
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

# different in buffers for cc and cs

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
ggsave("03_make-figs/fig_buffer.png", width = 4)

