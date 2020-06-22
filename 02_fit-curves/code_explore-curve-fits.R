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


#--note, soils data has lawlor, asking heather to remake it
dasoil <- read_csv("01_proc-raw-outs/pro_soils.csv")

#--get averages of top 60 cm in soil
dasoil60 <- 
  dasoil %>% 
  select(-texture, -paw) %>%
  pivot_longer(om_pct:p_h) %>% 
  filter(depth_cat < 7) %>% 
  group_by(site_id, name) %>% 
  summarise(mn_val = mean(value, na.rm = T))

p_soil<- 
  dasoil60 %>% 
  ggplot(aes(site_id, mn_val)) + 
  geom_point() + 
  geom_segment(aes(y = 0, yend = mn_val, x = site_id, xend = site_id)) +
  facet_wrap(~name, scales = "free") + 
  coord_flip()


p_aparm <- 
  daparms %>% 
  left_join(dasoil60) %>% 
  filter(term == "a") %>% 
  ggplot(aes(reorder(site_id, -estimate), estimate)) + 
  geom_boxplot(aes(fill = rotation))

library(patchwork)
p_soil + p_aparm
