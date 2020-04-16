# author: gina
# created: 4/13/2020
# purpose: process heather's data
# last updated: 4/15/2020 added new files
#               4/16/2020 used new Compiled2 w/fixed Gentry

library(tidyverse)
library(janitor)
library(readxl)

# data averaged by year ------------------------------------------------------------

raw <- read_csv("data/raw/Allcurvedata.csv") %>% 
  clean_names()

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



#--look at it

dat %>% 
  ggplot(aes(nrate, leach)) + 
  geom_point(aes(color = site)) + 
  facet_grid(.~sys_trt)



#--write it

dat %>% 
  write_csv("data/tidy/td_crop.csv")


# data by year ------------------------------------------------------------


raw2 <- read_excel("data/raw/CompiledData2.xlsx") %>% 
  rename(site = pub_reference) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, str_trim)

raw2 %>% write_csv("data/tidy/td_crop-by-year.csv")


# look at it --------------------------------------------------------------

#--use pwalk to make a fig for each site

leach <- raw2 %>% 
    select(site, year, cropsys, crop, leaching_kgha, n_rate) %>% 
  arrange(site, year, n_rate) %>% 
  filter(crop == 'corn')

plots <-
  leach %>%
  split(.$site) %>%
  map( ~ (
    ggplot(., aes(n_rate, leaching_kgha)) +
      geom_point(size = 3) +
      geom_line(linetype = "dashed") +
      facet_grid(cropsys ~ year) +
      labs(title = "corn")
  ))

paths <- stringr::str_c(names(plots), ".png")

pwalk(list(paths, plots), ggsave, path = "figs/")


# soil data ---------------------------------------------------------------

#--need to think about the metrics we want

soi <- read_csv("data/raw/soilallsites.csv")

