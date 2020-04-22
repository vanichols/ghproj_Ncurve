# author: gina
# created: 4/13/2020
# purpose: process heather's data
# last updated: 4/15/2020 added new files
#               4/16/2020 used new Compiled2 w/fixed Gentry
#               4/22/2020 data is still not settled, will continue to update

library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(readxl)
# data by year ------------------------------------------------------------


raw2 <- read_excel("data/raw/20200421_apsimdatacomplete.xlsx") %>% 
  rename(site = pub_reference) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim)

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

leach %>% 
  filter(site == "lawlor") %>% 
  filter(cropsys != "cc") %>% 
  ggplot(aes(n_rate, leaching_kgha, color = as.factor(year))) + 
  geom_point(size = 4) + 
  geom_line(size = 2) + 
  theme_bw() + 
  facet_grid(.~cropsys)

# soil data ---------------------------------------------------------------

#--need to think about the metrics we want

soi <- read_csv("data/raw/soilallsites.csv")

