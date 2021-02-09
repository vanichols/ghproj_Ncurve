# author: gina
# created; 2/9/2021
# purpose: find yield-scaled leaching 
# last updated: 

rm(list = ls())

# libraries ---------------------------------------------------------------

library(tidyverse)
library(saapsim)

datraw <- read_csv("01_proc-raw-outs/pro_apdat.csv") 

dat <- 
  datraw %>% 
  filter(crop == "corn") %>% 
  select(site_id, rotation, crop, nrate_kgha, year, leaching_kgha, nyear_leach_kgha_tot, yield_maize_buac) %>% 
  mutate(yield_maize_kgha = saf_buac_to_kgha_corn(yield_maize_buac),
         rotation2 = ifelse(rotation == "cs", "sc", rotation)) %>% 
  mutate(Nleach_yield = nyear_leach_kgha_tot / yield_maize_kgha ,
         leach_yield =  leaching_kgha / yield_maize_kgha) 

dat %>% 
  ggplot(aes(nrate_kgha, leach_yield)) + 
  geom_jitter(aes(color = site_id), size  = 3) + 
  facet_wrap(~rotation2) + 
  labs(y = "Leaching per corn yield (kg N leached / kg grain yield")

dat %>% 
  ggplot(aes(nrate_kgha, leach_yield)) + 
  geom_jitter(aes(color = site_id), size  = 3) + 
  stat_summary(geom = "line", size = 3) +
  facet_wrap(~rotation2) + 
  labs(y = "Corn yield per unit leached (kg grain yield / kg N leached")


dat %>% 
  filter(site_id == "klad"|site_id == "gold",
         yield_Nyearleach > 20000)
