# author: gina
# created: 4/22/2020
# purpose: fit curves to data
# notes: using naive approach

library(tidyverse)
library(nlraa)
library(nlme)
library(broom)



# read in data ------------------------------------------------------------

dat <- read_csv("data/tidy/td_crop-by-year.csv")


#--start with just leaching_kgha

leach <- dat %>% 
  select(site, year, cropsys, crop, leaching_kgha, n_rate) %>% 
  arrange(site, year, n_rate) %>% 
  filter(crop == 'corn') %>%
  mutate(year.f = as.factor(year))

leach2 <- 
  leach %>% 
  mutate(cropsys = dplyr::recode(cropsys,
                                 "sc" = "cs"))


# fit curves --------------------------------------------------------------

#--use a plateau-linear function
?SSblin

#--huggins test
#
fit1 <- nls(leaching_kgha ~ SSblin(n_rate, a, b, xs, c), data = leach2 %>% filter(site == "huggins", cropsys == "cc"))
tidy(fit1)
fitted(fit1) %>% tidy()

#--build a function
#
nls_bilinfit <- function(x){
  
  nls(leaching_kgha ~ SSblin(n_rate, a, b, xs, c),
        data = x)
}

#--test function
#
nls_bilinfit(filter(leach2, site == "huggins"))

#--map function to data
dat_parms <-
  leach2 %>%
  nest(data = c(crop, leaching_kgha, n_rate, year.f)) %>%
  mutate(mod = data %>% map(possibly(nls_bilinfit, NULL)),
    is_null = mod %>% map_lgl(is.null)) %>% 
    filter(is_null == 0) %>% 
    mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
    unnest(cols = c(res)) %>% 
    select(site, year, cropsys, term:p.value)
  
dat_parms  


#--how many didn't converge?
#
#--only 13? not bad. No idea what to do with those
leach2 %>% 
  select(site, year) %>% 
  distinct() %>% 
  left_join(dat_parms) %>% 
  filter(is.na(term))

dat_parms %>%
  unite(site, year, col = "site_year", remove = F) %>% 
  ggplot(aes(site_year, estimate, color = site)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(cropsys ~ term, scales = "free") + 
  coord_flip()

ggsave("figs/blin-fit-all.png")
