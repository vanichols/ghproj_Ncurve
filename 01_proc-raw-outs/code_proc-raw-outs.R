# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 

library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(readxl)
library(saapsim)

# data ------------------------------------------------------------

#--CC
cc <- 
  saf_readapout("01_proc-raw-outs/out_files/CC/") %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "cc")

#--SC -- the crop the data is from is not indicated in the data
sc <- 
  saf_readapout("01_proc-raw-outs/out_files/SC/") %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "sc")

#--CS
cs <- 
  saf_readapout("01_proc-raw-outs/out_files/SC/") %>%
  filter(file != "raw", file != "outs") %>% 
  mutate(rotation = "cs")


raw2 <- read_excel("data/raw/20200421_apsimdatacomplete.xlsx") %>% 
  rename(site = pub_reference) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim) %>% 
  mutate(site_id = recode(site,
                       "randall.iragavarapu" = "rair"),
         site_id = stringr::str_sub(site, 1, 4))
                       
                       
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
