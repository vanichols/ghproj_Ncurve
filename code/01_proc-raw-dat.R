# author: gina
# created: 4/13/2020
# purpose: process heather's data


library(tidyverse)
library(janitor)

# read in data ------------------------------------------------------------

raw <- read_csv("data/raw/Allcurvedata.csv") %>% 
  clean_names()


# process -----------------------------------------------------------------

#--make cropping system treatments explicit


base <- 
  raw %>%
  select(rate:index)

cs <- 
  raw %>% 
  select_if(str_detect(names(.), "cs")) %>% 
  bind_cols(base) %>% 
  mutate(sys_trt = "cs") %>% 
  rename_at(.vars = vars(ends_with("_cs")),
            list(~sub("_cs", "", .)))
  

cc <- 
  raw %>% 
  select_if(str_detect(names(.), "cc")) %>% 
  bind_cols(base) %>% 
  mutate(sys_trt = "cc") %>% 
  rename_at(.vars = vars(ends_with("_cc")),
            list(~sub("_cc", "", .)))

dat <- 
  bind_rows(cs, cc) %>% 
  select(state, site, index, rate, sys_trt, everything()) %>% 
  rename(nrate = rate) %>% 
  mutate_if(is.character, tolower)


# look at it --------------------------------------------------------------

dat %>% 
  ggplot(aes(nrate, leach)) + 
  geom_point(aes(color = site)) + 
  facet_grid(.~sys_trt)



# write it ----------------------------------------------------------------

dat %>% 
  write_csv("data/tidy/td_crop.csv")
