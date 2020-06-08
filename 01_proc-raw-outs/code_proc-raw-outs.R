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

#--CC
cc <- 
  saf_readapout("01_proc-raw-outs/out_files/CC/") %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "cc")

#--SC -- the crop the data is from is not indicated in the data - fixed
sc <- 
  saf_readapout("01_proc-raw-outs/out_files/SC/") %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "sc")

#--CS
cs <- 
  saf_readapout("01_proc-raw-outs/out_files/SC/") %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "cs")

dat <- bind_rows(cc, sc, cs) %>% 
  select(-path)

#--need to pull out site, Sutherland and iragblah are notated differently, ask heather to fix
dat2 <- 
  dat %>% 
  separate(file, into = c("site", "rot", "nrate", "other"), sep = "_") %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim) 

#--cs and sc are the same, wrong
dat2 %>% 
  select(site, year, rotation, soybean_annual_yield_bu_ac, maize_annual_yield_bu_ac) %>% 
  filter(site == "gentry") 


dat3 <- 
  dat2 %>% 
  mutate(site_id = stringr::str_sub(site, 1, 4)) %>% 
  select(-site, -rot, -nrate, -other) %>% 
  filter(maize_annual_yield_bu_ac != 0) #--keep only maize data
                       
                       
# look at it --------------------------------------------------------------
#--use pwalk to make a fig for each site


#--leaching

leach <- 
  dat3 %>% 
    select(site_id, year, rotation, annual_tile_nleaching, n_rate) %>% 
  arrange(site_id, year, n_rate)


leach %>%
  filter(site_id == 'gent') %>% 
  ggplot(., aes(n_rate, annual_tile_nleaching)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(rotation ~ year) +
  labs(title = "corn") + 
  theme(axis.text.x = element_text(angle = 90))

plots <-
  leach %>%
  split(.$site_id) %>%
  map( ~ (
    ggplot(., aes(n_rate, annual_tile_nleaching)) +
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
  select(site_id, year, rotation, maize_annual_yield_bu_ac, n_rate) %>% 
  arrange(site_id, year, n_rate)


yld %>%
  filter(site_id == 'gent') %>% 
  ggplot(., aes(n_rate, maize_annual_yield_bu_ac)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(rotation ~ year) +
  labs(title = "corn") + 
  theme(axis.text.x = element_text(angle = 90))

plots <-
  yld %>%
  split(.$site_id) %>%
  map( ~ (
    ggplot(., aes(n_rate, maize_annual_yield_bu_ac)) +
      geom_point(size = 2) +
      geom_line() +
      facet_grid(rotation ~ year) +
      labs(title = "corn") + 
      theme(axis.text.x = element_text(angle = 90))
    
  ))

paths <- stringr::str_c(names(plots), ".png")

pwalk(list(paths, plots), ggsave, path = "01_proc-raw-outs/figs/ylds/", width = 10, height = 4)

# soil data ---------------------------------------------------------------

#--need to think about the metrics we want

soi <- read_csv("data/raw/soilallsites.csv") %>% 
  rename(site = pub_reference) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim) %>% 
  mutate(site_id = recode(site,
                          "randall.iragavarapu" = "rair_rand_iragav"),
         site_id = stringr::str_sub(site_id, 1, 4))


#--mean values for 0-60cm
soi_60cm <- 
  soi %>% 
  select(site, site_id, depth_inc, depth_cat, om_pct:pH, -texture) %>%
  pivot_longer(om_pct:pH) %>% 
  filter(depth_cat < 7) %>% 
  group_by(site, site_id, name) %>% 
  summarise(mn_val = mean(value, na.rm = T))

soi_60cm %>% 
  ggplot(aes(site_id, mn_val)) + 
  geom_point() + 
  facet_wrap(~name, scales = "free") + 
  coord_flip()

#--lots of stuff is useless
#--need to do something diff for paw, but ok for now

soi_tidy <- 
  soi_60cm %>% 
  filter(name %in% c("BD", "clay_pct", "DUL", "KS", "LL", "om_pct", "PAW", "pH")) %>% 
  pivot_wider(names_from = name, values_from = mn_val)


soi_tidy %>% write_csv("data/tidy/td_soil-60cm.csv")
