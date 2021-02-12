# author: gina
# created; 2/9/2021
# purpose: find yield-scaled leaching 
# last updated: 

rm(list = ls())
theme_set(theme_bw())

# libraries ---------------------------------------------------------------

library(tidyverse)
library(saapsim)

siteinfo <- 
  read_csv("01_proc-raw-outs/pro_site-info.csv") %>% 
  select(site_id, site_id2)

datraw <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  left_join(siteinfo) %>%
  select(-site_id) %>% 
  rename("site_id" = site_id2)

datfits <- 
  read_csv("02_fit-curves/fc_blin-leach-parms-mm.csv") %>% 
  select(rotation, xs) %>% 
  mutate(rotation = ifelse(rotation == "cs", "sc", rotation)) %>% 
  rename("rotation2" = rotation) %>% 
  distinct()


dat <- 
  datraw %>% 
  filter(crop == "corn") %>% 
  select(site_id, rotation, crop, nrate_kgha, year, leaching_kgha, nyear_leach_kgha_tot, yield_maize_buac) %>% 
  mutate(yield_maize_kgha = saf_buac_to_kgha_corn(yield_maize_buac),
         rotation2 = ifelse(rotation == "cs", "sc", rotation)) %>% 
  mutate(Nleach_yield = nyear_leach_kgha_tot / yield_maize_kgha ,
         leach_yield =  leaching_kgha / yield_maize_kgha) 

dat %>% 
  ggplot(aes(nrate_kgha, 1/leach_yield)) + 
  geom_jitter(aes(color = site_id), size  = 3) + 
  stat_summary(geom = "line", size = 3) +
  geom_vline(data = datfits, aes(xintercept = xs), linetype = "dashed", color = "red", size = 2) +
  facet_wrap(~rotation2) + 
  labs(x = "Nitrogen fertilizer rate\n(kg N /ha)",
       y = "Maize yield per unit leached\n(kg grain yield / kg N leached)",
       title = "Maize yield per unit N leached is maximized at fertilizer rates lower than leaching breakpoint")

ggsave("fig_supp_leaching-scaled-yield.png")

dat %>% 
  ggplot(aes(nrate_kgha, leach_yield)) + 
  geom_jitter(aes(color = site_id), size  = 3) + 
  stat_summary(geom = "line", size = 3) +
  geom_vline(data = datfits, aes(xintercept = xs), linetype = "dashed", color = "red", size = 2) +
  facet_wrap(~rotation2) + 
  labs(x = "Nitrogen fertilizer rate\n(kg N /ha)",
       y = "Leaching per unit maize grain yield\n(kg N / kg grain)",
       title = "Leaching per unit maize yield is minimized at fertizer rates lower than leaching breakpoint")

ggsave("fig_supp_yield-scaled-leaching.png")

