# author: gina
# created: 6/17/2020
# purpose: process heather's data
# last updated: 

library(tidyverse)
library(stringr)
library(janitor)
library(readxl)
library(saapsim)
library(purrr)



#data ------------------------------------------------------------

sites <- read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  select(site_id) %>% 
  distinct()
sites


asoil <- read_csv("../../../Box/1_Gina_Projects/proj_Ncurve/20200522_apsimsoil.csv") %>% 
  rename(site_id = pub_reference) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim) %>% 
  mutate(site_id = stringr::str_sub(site_id, 1, 4)) %>% 
  select(site_id, depth_inc:pH) %>% 
  janitor::clean_names()

write_csv(asoil, "01_proc-raw-outs/pro_soils.csv")


#--mean values for 0-60cm
asoil %>% 
  select(-texture, -paw) %>%
  pivot_longer(om_pct:p_h) %>% 
  filter(depth_cat < 7) %>% 
  group_by(site_id, name) %>% 
  summarise(mn_val = mean(value, na.rm = T)) %>% 
  ggplot(aes(site_id, mn_val)) + 
  geom_point() + 
  geom_segment(aes(y = 0, yend = mn_val, x = site_id, xend = site_id)) +
  facet_wrap(~name, scales = "free") + 
  coord_flip()


#--lots of stuff is useless
#--need to do something diff for paw, but ok for now
