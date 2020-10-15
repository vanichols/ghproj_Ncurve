# author: gina
# created: 10/15/2020
# purpose: process heather's daily data for a demonstrative figure
# last updated: 

rm(list = ls())

library(tidyverse)
library(lubridate)
library(stringr)
library(janitor)
library(readxl)
#remotes::install_github("vanichols/saapsim")
library(saapsim)
library(purrr)
library(ggpubr)
library(ggthemes)

myplantday <- saf_date_to_doy("2001-05-01")

#data ------------------------------------------------------------
#note: read in data directly from box folder, it will change

# what year is corn in the CS  - 1995 is a corn year

datraw <- saf_readapout("../../../Box/1_Gina_Projects/proj_Ncurve/Daily_Out_Files_20201009/") %>% 
  separate(file, into = c("site", "rot", "nrate")) %>% 
  select(site, rot, nrate, date, year, subsurface_drain_no3, crop_buac_maize)  %>% 
  rename(leaching_kgha = subsurface_drain_no3) 

datraw %>% 
  ggplot(aes(date, leaching_kgha)) + 
  geom_line() + 
  facet_grid(.~rot)


dat <- 
  datraw %>% 
  filter(year %in% c(1995, 1996, 1997, 1998))



# assign leaching before April 15 to previous year ------------------------

dat_crop <- 
  dat %>% 
  mutate(
    doy = yday(date),
    nyear = ifelse(doy < myplantday, year - 1, year)) %>% 
  group_by(site, rot, nrate, nyear) %>% 
  mutate(leach_cumsum_crop = cumsum(leaching_kgha)) %>% 
  ungroup()

dat_rot <- 
  dat %>% 
  mutate(
    doy = yday(date),
    nyear = ifelse(doy < myplantday, year - 1, year),
    nyear2 = case_when(
      (rot == "CS") & (nyear == 1996) ~ 1995,
      (rot == "CS") & (nyear == 1998) ~ 1997,
      TRUE ~ nyear)
    ) %>%
  group_by(site, rot, nrate, nyear2) %>% 
  mutate(leach_cumsum_rot = cumsum(leaching_kgha)) %>% 
  ungroup()


dat_crop %>%   
  select(site, rot, nrate, year, date, doy, leach_cumsum_crop) %>% 
  left_join(
    dat_rot %>% 
      select(site, rot, nrate, year, date, doy, leach_cumsum_rot)
  ) %>% 
  mutate(leach_cumsum_crop = ifelse((doy < 105 & year == 1995), NA, leach_cumsum_crop),
         leach_cumsum_rot = ifelse((doy < 105 & year == 1995), NA, leach_cumsum_rot)) %>% 
  ggplot(aes(date, leach_cumsum_crop)) + 
  geom_line(color = "blue") + 
  geom_line(aes(date, leach_cumsum_rot), color = "red") + 
  facet_grid(.~rot)

plant_days <- 
  tibble(rot = c("Continuous Maize", "Maize/Soybean Rotation", "Maize/Soybean Rotation"),
       crop = c("Maize", "Maize", "Soybean"),
       pdate1 = c("1995-05-01", "1995-05-01", NA),
       pdate2 = c("1996-05-01", NA, "1996-05-01"),
       pdate3 = c("1997-05-01", "1997-05-01", NA),
       pdate4 = c("1998-05-01", NA, "1998-05-01"))



dat_rot %>%   
  mutate(leach_cumsum_rot = ifelse((doy < myplantday & year == 1995), NA, leach_cumsum_rot),
         rot = case_when(
           rot == "CC" ~ "Continuous Maize",
         rot == "CS" ~ "Maize/Soybean Rotation")
         ) %>% 
  ggplot(aes(date, leach_cumsum_rot)) + 
  geom_line(color = "orange4", size = 2) + 
  #--planting 1995
  geom_segment(data = plant_days,
               aes(x = as_date(pdate1), xend = as_date(pdate1)),
               yend = 0, y = -10, size = 1.5,
               arrow = arrow(length = unit(0.05, "npc"))) +
  geom_label(data = plant_days,
             aes(x = as_date(pdate1), y = -15, label = paste0(crop, "Planting"))) +
  #--planting 1996
  geom_segment(data = plant_days,
               aes(x = as_date(pdate2), xend = as_date(pdate2)),
               yend = 0, y = -10, size = 1.5,
               arrow = arrow(length = unit(0.05, "npc"))) +
  geom_label(data = plant_days,
             aes(x = as_date(pdate2), y = -15, label = paste0(crop, "Planting"))) +
  #--planting 1997
  geom_segment(data = plant_days,
               aes(x = as_date(pdate3), xend = as_date(pdate3)),
               yend = 0, y = -10, size = 1.5,
               arrow = arrow(length = unit(0.05, "npc"))) +
  geom_label(data = plant_days,
             aes(x = as_date(pdate3), y = -15, label = paste0(crop, "Planting"))) +
  #--planting 1998
  geom_segment(data = plant_days,
               aes(x = as_date(pdate4), xend = as_date(pdate4)),
               yend = 0, y = -10, size = 1.5,
               arrow = arrow(length = unit(0.05, "npc"))) +
  geom_label(data = plant_days,
             aes(x = as_date(pdate4), y = -15, label = paste0(crop, "Planting"))) +
  facet_wrap(~rot, ncol = 1) + 
  coord_cartesian(ylim = c(-20, 70)) + 
  theme_clean() + 
  theme(strip.text = element_text(size = rel(1.5)),
        strip.background = element_rect(fill = "gray80"),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2))) +
  labs(y = "Cumulative Nitrate Leaching (kg/ha)",
       x = NULL,
       title = "Site MN1\n112 kg/ha Nitrogen Fertilizer")

ggsave("03_make-figs/fig_leach-cum-ex.png")
ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_leach-cum-ex-10-15-20.png", width = 12, height = 9)
