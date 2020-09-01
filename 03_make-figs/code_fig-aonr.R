# author: gina
# created; 9/1/2020
# purpose: make aonr plots 
# last updated: 

rm(list = ls())

# libraries ---------------------------------------------------------------

library(tidyverse)



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
ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/manuscript/fig_aonr-boxplot.png")

aonr %>% 
  ggplot(aes(site, xs)) + 
  geom_boxplot(aes(fill = rotation)) + 
  facet_grid(rotation~site, scales = "free")

ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/manuscript/fig_aonr-boxplot2.png")

