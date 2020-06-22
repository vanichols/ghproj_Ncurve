# author: gina
# created: 6/17/2020
# purpose: process heather's data
# last updated: 6/22/2020 use new data, proc site info as well

library(tidyverse)
library(stringr)
library(janitor)
library(readxl)
library(saapsim)
library(purrr)



# also process site data --------------------------------------------------


asite <- 
  read_csv("../../../Box/1_Gina_Projects/proj_Ncurve/20200618_apsimdata.csv") %>% 
  rename(site_id = site) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim) %>% 
  mutate(site_id = stringr::str_sub(site_id, 1, 4)) %>% 
  mutate_if(is.numeric, round, 0) %>% 
  janitor::clean_names() %>% 
  select(-(year:crop_p_dom), -cropsys) %>% 
  distinct()

asite

write_csv(asite, "01_proc-raw-outs/pro_site-info.csv")


#data ------------------------------------------------------------

sites <- read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  select(site_id) %>% 
  distinct()
sites


asoil <- read_csv("../../../Box/1_Gina_Projects/proj_Ncurve/20200618_apsimsoil.csv") %>% 
  rename(site_id = site) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim) %>% 
  mutate(site_id = stringr::str_sub(site_id, 1, 4)) %>% 
  select(site_id, depth_inc:pH) %>% 
  janitor::clean_names()

asoil %>% select(site_id) %>% distinct()

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

#--write the 60cm stuff
asoil60 <- 
  asoil %>% 
  select(site_id:clay_pct, bd, ll, dul, sat, ks) %>%
  pivot_longer(om_pct:ks) %>% 
  filter(depth_cat < 7) %>% 
  group_by(site_id, name) %>% 
  summarise(mn_val = mean(value, na.rm = T)) %>% 
  pivot_wider(names_from = name, values_from = mn_val) %>% 
  left_join(
    asoil %>% 
  select(site_id:depth_cat, paw) %>%
  filter(depth_cat < 7) %>% 
  group_by(site_id) %>% 
  summarise(paw_mm = sum(paw))
  ) 

asoil60 %>% write_csv("01_proc-raw-outs/pro_soils-60cm.csv")

#--lots of stuff is useless
#--need to do something diff for paw, but ok for now
