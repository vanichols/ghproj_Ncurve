# author: gina
# created: 7/2/2020
# purpose: fit curve to each eu
# last updated: 

library(tidyverse)
library(scales)
library(nlraa)
library(nlme)
library(emmeans)
library(car) #--overwrites recode in dplyr
library(minpack.lm)




# data --------------------------------------------------------------------

dat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,                                                                "sc" = "cs")) %>% 
  mutate(eu = paste0(site_id,"_", year, rotation)) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id),
         rotation = as.factor(rotation)) %>% 
  filter(!is.na(nyear_leach_kgha_tot)) %>% 
  select(eu, site_id, rotation, yearF, crop, nrate_kgha, nyear_leach_kgha_tot, yield_maize_buac)


dat %>% 
  select(eu) %>% 
  distinct() %>% 
  tally()

unique(dat$site_id)

# leach -------------------------------------------------------------------

leach <- 
  dat %>% 
  select(eu, everything()) %>% 
  rename("leaching_kgha" = nyear_leach_kgha_tot) %>% 
  pivot_longer(leaching_kgha:yield_maize_buac) %>%
  filter(name == "leaching_kgha")

#--build functions

aic_blin <- function(x){
  AIC(nls(value ~ SSblin(nrate_kgha, a, b, xs, c), data = x))
}


aic_expf <- function(x){
    AIC(nls(value ~ SSexpf(nrate_kgha, a, c), data = x))
}

aic_explin <- function(x){
  AIC(nlsLM(value ~ SSexplin(nrate_kgha, cm, rm, tb), data = x))
}

#--test on one
aic_blin(filter(leach, eu == "gent_2000cc"))


#--map function to data
leach_aic <-
  leach %>%
  group_by(eu) %>% 
  nest(data = c(value, nrate_kgha, yearF)) %>%
  mutate(
    blin = data %>% map(possibly(aic_blin, NULL)),
         expf = data %>% map(possibly(aic_expf, NULL)),
    explin = data %>% map(possibly(aic_explin, NULL)),
    is_null = blin %>% map_lgl(is.null)
    ) %>% 
  filter(is_null == 0) %>%  
  unnest(cols = c(blin, expf, explin)) 

leach_aic %>% 
  filter(is_null == "TRUE")

leach_aic %>%
  select(eu, blin, expf, explin) %>% 
  pivot_longer(blin:explin) %>% 
  ggplot(aes(eu, value)) +
  geom_point(aes(color = name))

#--what % is blin best? etc
leach_aic %>%
  select(eu, blin, expf, explin) %>% 
  pivot_longer(blin:explin) %>% 
  group_by(eu) %>% 
  mutate(min = min(value)) %>% 
  filter(value == min) %>%
  ungroup() %>% 
  summarise(blinT = sum(ifelse(name == "blin", 1, 0))/n(),
         expfT = sum(ifelse(name == "expf", 1, 0))/n(),
         explinT = sum(ifelse(name == "explin", 1, 0))/n())
