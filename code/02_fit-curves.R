# author: gina
# created: 4/13/2020
# purpose: fit curves to data
# notes: 

library(tidyverse)
library(nlraa)
library(broom)

# read in data ------------------------------------------------------------

dat <- read_csv("data/tidy/td_crop-by-year.csv")


#--start with just leaching_kgha, CC
dat_leach <- 
  dat %>% 
  select(site, year, crop, cropsys, n_rate, leaching_kgha) %>% 
  mutate_if(is.character, tolower) %>% 
  #--remove gentry for now
  filter(site != "gentry")

#--something is wrong with gentry, I think
dat_leach %>% 
  filter(site_id == "gentry") %>% 
  ggplot(aes(n_rate, leaching_kgha)) + 
  geom_point(aes(color = crop), size = 4) + 
  facet_grid(.~cropsys)

dat_leach %>% 
  filter(site_id == "gentry") %>% 
  group_by(n_rate) %>% 
  summarise(n = n())

# fit curves --------------------------------------------------------------

#--use a plateau-linear function
?SSplin

# huggins, no random effect of year:
fit1 <- nls(leaching_kgha ~ SSplin(n_rate, a, xs, b), data = dat_sub %>% filter(site_id == "huggins"))
summary(fit1)
tidy(fit1)
fitted(fit1) %>% tidy()

#--pretty sure I can map this. Go to nlraa_help, I try to fit a random effect of year

# try mapping -------------------------------------------------------------

dat_parms <- 
  dat_sub %>% 
  group_by(site, sys_trt) %>% 
  nest() %>% 
  mutate(mod = data %>% map(~nls(leach ~ SSplin(nrate, a, xs, b), data = .)),
         sum = mod %>% map(~tidy(.))) %>% 
  select(site, sum) %>% 
  unnest(cols = c(sum))


#dat_parms <- 
  dat_sub %>% 
  group_by(site_id, cropsys) %>% 
  nest() %>% 
  mutate(mod = data %>% map(~nls(leach ~ SSplin(nrate, a, xs, b), data = .)),
         fits = mod %>% map(~fitted(.)),
         tidyfits = fits %>% map(~tidy(.))) %>% 
    select(site, data, tidyfits) %>% 
    unnest(cols = c(data, tidyfits))

  
ggplot(data = s1, aes(x = x, y = y)) + 
  geom_point(color = "red") + 
  geom_line(aes(y = fitted(fit1)), color = "red") + 
  geom_point(data = s2, aes(x = x, y = y), color = "blue") + 
  geom_line(data = s2, aes(x = x, y = fitted(fit2)), color = "blue") 

summary(fit1) #--pivot point is at 128
summary(fit2) #--pivot point is at 39