# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)
#               6/16/2020 (weather in hoffman cc was wrong)
#               7/26/2020 (add soybean back into processed data)
#               8/20/2020 (remove sutherland from analysis)

rm(list = ls())

library(tidyverse)
library(stringr)
library(janitor)
library(readxl)
#remotes::install_github("vanichols/saapsim")
library(saapsim)
library(purrr)



#data ------------------------------------------------------------
#note: read in data directly from box folder, it will change


# mydir <- "../../../Box/1_Gina_Projects/proj_Ncurve/Out_Files_20201006/"
# 
# #--CC
# cc <-
#   saf_readapout(fold_dir = paste0(mydir, "CC_Out_Files/")) %>%
#   filter(file != "raw", file != "outs") %>%
#   mutate(rotation = "cc")
# 
# #--SC
# sc <-
#   saf_readapout(fold_dir = paste0(mydir, "SC_Out_Files/")) %>%
#   filter(file != "raw", file != "outs") %>%
#   mutate(rotation = "sc")
# 
# #--CS
# cs <-
#   saf_readapout(fold_dir = paste0(mydir, "CS_Out_Files/")) %>%
#   filter(file != "raw", file != "outs") %>%
#   mutate(rotation = "cs")
# 
# dat <- bind_rows(cc, sc, cs) %>%
#   select(-path)

#--so you don't have to read them in every time you want to change something
#write_rds(dat, "01_proc-raw-outs/pro_rawapdat.rds")


#--eliminate the things we've decided not to use
dat <- read_rds("01_proc-raw-outs/pro_rawapdat.rds") %>% 
  select(-date, -annual_rain_mm, -inseason_rain_mm, -drainage_mm, -nitrateflow_mg_l, -nyear, -doy)

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
  mutate(crop = ifelse(yield_soy_buac == 0, "corn", "soy")) %>% #--make the crop explicit
  #filter(yield_maize_buac != 0) %>% #--keep only maize data
  #--Heather says spin is 1994-1998. Start in 1999
  #--based on figs, it should start in 2000, not 1999
  filter(year >= 2000) %>% 
  #--remove hoff year 2012
  filter( ! (year == 2012 & site_id == "hoff")) %>% 
  rename(nrate_kgha = n_rate_kgha) %>% 
  select(site_id, year, nrate_kgha, rotation, crop, everything())


#--remove sutherland from analysis (8/20/2020, see 20200819_Qs in Heather Box folder)
dat4 <- 
  dat3 %>% 
  filter(site_id != "suth") 

# this is tricky. I need to separate the leaching from teh other data for this
# create a small dummy set

# dat4 %>% 
#   arrange(site_id, year, nrate_kgha, rotation, crop, nrate_kgha) %>% 
#   group_by(site_id, nrate_kgha, rotation, crop) %>% 
#   do(head(., n = 5)) %>% 
#   slice(1:10) %>% 
#   arrange(site_id, rotation, nrate_kgha, year) %>% 
#   group_by(site_id, nrate_kgha, rotation) %>% 
#   mutate(pre_lead = dplyr::lead(pre_leaching_kgha, n = 1, default = NA),
#          nyear_leach_kgha = pre_lead + in_leaching_kgha + post_leaching_kgha) %>% 
#   select(-pre_leaching_kgha, -in_leaching_kgha, -post_leaching_kgha, -pre_lead) %>% 
#   ungroup()
# 

###---leaching
dleach4 <- 
  dat4 %>% 
  select(site_id, nrate_kgha, rotation, crop, year, pre_leaching_kgha, in_leaching_kgha, post_leaching_kgha)

#--calculate leaching from sowing to sowing
dleach5 <- 
  dleach4 %>% 
  arrange(site_id, rotation, nrate_kgha, year) %>% 
  group_by(site_id, nrate_kgha, rotation) %>% 
  mutate(pre_lead = dplyr::lead(pre_leaching_kgha, n = 1, default = NA),
         nyear_leach_kgha = pre_lead + in_leaching_kgha + post_leaching_kgha) %>% 
  select(-pre_leaching_kgha, -in_leaching_kgha, -post_leaching_kgha, -pre_lead) %>% 
  ungroup()

#--find first year of simulation, if it's soybean we want to eliminate it
dleach6 <- 
  dleach5 %>%
  select(year, site_id, rotation, crop, nyear_leach_kgha, nrate_kgha) %>% 
  group_by(site_id, rotation) %>% 
  mutate(minyear = min(year),
         getridof = ifelse( (minyear == year & crop == "soy"), "x", "keep")) %>% 
  filter(getridof != "x") %>% 
  select(-minyear, -getridof)
  
#--make a year to group by and sum
dleach7 <- 
  dleach6 %>% 
  mutate(yearleach_id = ifelse(crop == "corn", year, year-1)) %>% 
  group_by(yearleach_id, site_id, rotation, nrate_kgha) %>% 
  summarise(nyear_leach_kgha_tot = sum(nyear_leach_kgha)) %>% 
  rename(year = "yearleach_id")
  

dat5 <- 
  dat4 %>%
  left_join(dleach7) %>% 
  select(site_id, rotation, crop, nrate_kgha, year, leaching_kgha, nyear_leach_kgha_tot, everything())
  

write_csv(dat5, "01_proc-raw-outs/pro_apdat.csv")



# # other -------------------------------------------------------------------
# 
# 
# 
# #--make data for Phil Dixon
# #--leaching only, cs clarified
# dat4 <- 
#   dat3 %>%
#   select(year, rotation, site_id, nrate_kgha, leaching_kgha) %>% 
#   mutate(rotation = ifelse(rotation == "sc", "cs", rotation)) %>% 
#   arrange(year, rotation, site_id, nrate_kgha)
# 
# dat4 %>% 
#   mutate_if(is.character, as.factor) %>% 
#   mutate(year = as.factor(year))
#   summary()
# 
# str(
#   dat4 %>% 
#     mutate_if(is.character, as.factor) %>% 
#     mutate(year = as.factor(year)))
# 
# dat4 %>% 
#   ggplot(aes(nrate_kgha, leaching_kgha)) + 
#   geom_point()
# 
# write_csv(dat4, "01_proc-raw-outs/pro_lunchinators.csv")
# 
# #--hoffman 2004 only had 96 mm of in-season rain?! but only for cc?
# #--addressed
# dat3 %>% 
#   filter(inseason_rain_mm < 250) %>% 
#   select(site_id, rotation, everything())
# 
# dat3 %>% 
#   filter(site_id == "hoff", year == 2004, rotation != "cc") %>% 
#   select(site_id, rotation, everything())
# 
# #--rand has 1038 of in-season rain, and 1201 of annual rain
# dat3 %>% 
#   filter(inseason_rain_mm > 1000) %>% 
#   select(site_id, everything())
# 
# #--gold? 
# dat3 %>%
#   filter(site_id == "gold") %>% 
#   filter(inseason_rain_mm < 300) %>% 
#   select(site_id, rotation, everything())
# 

# look at it --------------------------------------------------------------
#--use pwalk to make a fig for each site


#--leaching

leach <- 
  dat4 %>% 
    select(site_id, year, rotation, crop, leaching_kgha, nrate_kgha) %>% 
  arrange(site_id, year, nrate_kgha)


leach %>%
  filter(site_id == 'gent') %>% 
  ggplot(., aes(nrate_kgha, leaching_kgha, color = crop)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(rotation ~ year) +
  labs(title = "leaching") + 
  theme(axis.text.x = element_text(angle = 90))

plots <-
  leach %>%
  split(.$site_id) %>%
  map( ~ (
    ggplot(., aes(nrate_kgha, leaching_kgha, color = crop)) +
      geom_point(size = 2) +
      geom_line() +
      facet_grid(rotation ~ year) +
      labs(title = "leaching") + 
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
