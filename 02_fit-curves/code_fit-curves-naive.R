# author: gina
# created: 4/22/2020
# purpose: fit curves to data
# notes: using naive approach
# updated: 6/11/2020, new data

library(tidyverse)
library(nlraa)
library(nlme)
library(broom)



# read in data ------------------------------------------------------------

dat <- read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                  "sc" = "cs"))


#--start with just leaching_kgha

leach <- 
  dat %>% 
  select(site_id, year, yearF, rotation, leaching_kgha, nrate_kgha) %>% 
  arrange(site_id, year, nrate_kgha) 


# fit curves --------------------------------------------------------------

#--use a plateau-linear function
?SSblin

#--huggins test
#
fit1 <- nls(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c), 
            data = leach %>%
              filter(site_id == "hugg", rotation == "cc"))
tidy(fit1)
fitted(fit1) %>% tidy()

#--build a function
#
nls_bilinfit <- function(x){
  
  nls(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c),
        data = x)
}

#--test function
#
nls_bilinfit(filter(leach, site_id == "hugg"))

#--map function to data
dat_parms <-
  leach %>%
  nest(data = c(leaching_kgha, nrate_kgha, yearF)) %>%
  mutate(mod = data %>% map(possibly(nls_bilinfit, NULL)),
    is_null = mod %>% map_lgl(is.null)) %>% 
    filter(is_null == 0) %>% 
    mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
    unnest(cols = c(res)) %>% 
    select(site_id, year, rotation, term:p.value)
  
dat_parms  


#--how many didn't converge?
# 3, not bad
leach %>% 
  select(site_id, year) %>% 
  distinct() %>% 
  left_join(dat_parms) %>% 
  filter(is.na(term))

dat_parms %>%
  unite(site_id, year, col = "site_year", remove = F) %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(rotation ~ term, scales = "free") + 
  coord_flip() + 
  theme(axis.text.y = element_blank())

ggsave("02_fit-curves/figs_blin-fit-all.png")
