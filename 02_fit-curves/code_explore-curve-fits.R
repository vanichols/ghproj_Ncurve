# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)
#               6/17/2020 look at soils data

library(tidyverse)
library(plotly)
library(scales)

daparms <- read_csv("02_fit-curves/fc_blin-leach-parms.csv")

daparms %>% 
  filter(term == "a") %>% 
  ggplot(aes(reorder(site_id, -estimate), estimate)) + 
  geom_boxplot(aes(fill = rotation)) + 
  labs(x = NULL,
       y = "Intercept at 0N",
       title = "Seems to be two groups for the intercept")

ggsave("02_fit-curves/figs_blin-intcpt-2-groups.png")


dasoil <- read_csv("01_proc-raw-outs/pro_soils-60cm.csv")


daparms %>%  
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



daparms %>%  
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
