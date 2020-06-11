# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)

library(tidyverse)
library(stringr)
library(janitor)
library(readxl)
library(saapsim)
library(purrr)



# data ------------------------------------------------------------
# note: read in data directly from box folder, it will change

mydir <- "../../../Box/1_Gina_Projects/proj_Ncurve/Out Files 2020.6.10/"

#--CC
cc <- 
  saf_readapout(fold_dir = paste0(mydir, "CC Out Files/")) %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "cc")

#--SC
sc <- 
  saf_readapout(fold_dir = paste0(mydir, "SC Out Files/")) %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "sc")

#--CS
cs <- 
  saf_readapout(fold_dir = paste0(mydir, "CS Out Files/")) %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "cs")

dat <- bind_rows(cc, sc, cs) %>% 
  select(-path)

#--so you don't have to read them in every time you want to change something
write_rds(dat, "01_proc-raw-outs/pro_rawapdat.rds")


#--pull out site from file name
dat2 <- 
  dat %>%  
  separate(file, into = c("site", "rot", "nrate"), sep = "_") %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim) 

#--get only real data (not spin up, not soybean, etc.)
dat3 <- 
  dat2 %>% 
  mutate(site_id = stringr::str_sub(site, 1, 4)) %>% 
  select(-site, -rot, -nrate) %>% 
  filter(yield_maize_buac != 0) %>% #--keep only maize data
  #--Heather says spin is 1994-1998. Start in 1999
  #--based on figs, it should start in 2000, not 1999
  filter(year >= 2000) %>% 
  #--remove hoff year 2012
  filter( ! (year == 2012 & site_id == "hoff")) %>% 
  rename(nrate_kgha = n_rate_kgha)


write_csv(dat3, "01_proc-raw-outs/pro_apdat.csv")


# look at it --------------------------------------------------------------
#--use pwalk to make a fig for each site


#--leaching

leach <- 
  dat3 %>% 
    select(site_id, year, rotation, leaching_kgha, nrate_kgha) %>% 
  arrange(site_id, year, nrate_kgha)


leach %>%
  filter(site_id == 'gent') %>% 
  ggplot(., aes(nrate_kgha, leaching_kgha)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(rotation ~ year) +
  labs(title = "corn") + 
  theme(axis.text.x = element_text(angle = 90))

plots <-
  leach %>%
  split(.$site_id) %>%
  map( ~ (
    ggplot(., aes(nrate_kgha, leaching_kgha)) +
      geom_point(size = 2) +
      geom_line() +
      facet_grid(rotation ~ year) +
      labs(title = "corn") + 
      theme(axis.text.x = element_text(angle = 90))
    
  ))

paths <- stringr::str_c(names(plots), ".png")

pwalk(list(paths, plots), ggsave, path = "01_proc-raw-outs/figs/leach/", width = 10, height = 4)


#--yield

yld <- 
  dat3 %>% 
  select(site_id, year, rotation, yield_maize_buac, nrate_kgha) %>% 
  arrange(site_id, year, nrate_kgha)


yld %>%
  filter(site_id == 'gent') %>% 
  ggplot(., aes(nrate_kgha, yield_maize_buac)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(rotation ~ year) +
  labs(title = "corn") + 
  theme(axis.text.x = element_text(angle = 90))

plots <-
  yld %>%
  split(.$site_id) %>%
  map( ~ (
    ggplot(., aes(nrate_kgha, yield_maize_buac)) +
      geom_point(size = 2) +
      geom_line() +
      facet_grid(rotation ~ year) +
      labs(title = "corn") + 
      theme(axis.text.x = element_text(angle = 90))
    
  ))

paths <- stringr::str_c(names(plots), ".png")

pwalk(list(paths, plots), ggsave, path = "01_proc-raw-outs/figs/ylds/", width = 10, height = 4)




# ## OLD
# # soil data ---------------------------------------------------------------
# 
# #--need to think about the metrics we want
# 
# soi <- read_csv("data/raw/soilallsites.csv") %>% 
#   rename(site = pub_reference) %>% 
#   mutate_if(is.character, tolower) %>% 
#   mutate_if(is.character, stringr::str_trim) %>% 
#   mutate(site_id = recode(site,
#                           "randall.iragavarapu" = "rair_rand_iragav"),
#          site_id = stringr::str_sub(site_id, 1, 4))
# 
# 
# #--mean values for 0-60cm
# soi_60cm <- 
#   soi %>% 
#   select(site, site_id, depth_inc, depth_cat, om_pct:pH, -texture) %>%
#   pivot_longer(om_pct:pH) %>% 
#   filter(depth_cat < 7) %>% 
#   group_by(site, site_id, name) %>% 
#   summarise(mn_val = mean(value, na.rm = T))
# 
# soi_60cm %>% 
#   ggplot(aes(site_id, mn_val)) + 
#   geom_point() + 
#   facet_wrap(~name, scales = "free") + 
#   coord_flip()
# 
# #--lots of stuff is useless
# #--need to do something diff for paw, but ok for now
# 
# soi_tidy <- 
#   soi_60cm %>% 
#   filter(name %in% c("BD", "clay_pct", "DUL", "KS", "LL", "om_pct", "PAW", "pH")) %>% 
#   pivot_wider(names_from = name, values_from = mn_val)
# 
# 
# soi_tidy %>% write_csv("data/tidy/td_soil-60cm.csv")
