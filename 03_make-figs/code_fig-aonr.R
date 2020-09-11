# author: gina
# created; 9/1/2020
# purpose: make aonr plots 
# last updated: 

rm(list = ls())

# libraries ---------------------------------------------------------------

library(tidyverse)
library(ggridges)

clr1 <- "orange2"
clr2 <- "darkblue"


# data --------------------------------------------------------------------

aonr <- 
  read_csv("02_fit-curves/fc_blin-yield-parms-mm.csv")


# fig ---------------------------------------------------------------------

aonr %>% 
  ggplot(aes(site, xs)) + 
  geom_boxplot(aes(fill = rotation))

aonr %>% 
  ggplot(aes(site, xs)) + 
  geom_boxplot(aes(fill = rotation)) + 
  facet_grid(.~site, scales = "free")

aonr %>% 
  ggplot(aes(site, xs)) + 
  geom_boxplot(aes(fill = rotation)) + 
  facet_grid(rotation~site, scales = "free")



aonr %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) %>% 
  ggplot(aes(xs, site)) + 
  geom_density_ridges(aes(fill = rot2), alpha = 0.5) + 
  labs(fill = NULL,
       y = "How are you referring to the sites? I think this should use a consistent terminology as you use in other figures",
       x = "Agronomically Optimum\nNitrogen Rate (kg ha-1)") +
  coord_flip() + 
  scale_fill_manual(values = c(clr2, clr1)) +
  theme_bw() +
  theme(legend.position = "top")

ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_aonr-9-11-20.png")

