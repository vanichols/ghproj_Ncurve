# author: gina
# created: 4/13/2020
# purpose: fit curves to data
# notes: 

library(tidyverse)
library(nlraa)
library(broom)

# read in data ------------------------------------------------------------

dat <- read_csv("data/tidy/td_crop.csv")



# fit curves --------------------------------------------------------------

#--use a plateau-linear function
?SSplin

# huggins:
fit1 <- nls(leach ~ SSplin(nrate, a, xs, b), data = dat %>% filter(site == "huggins"))
summary(fit1)
tidy(fit1)
fitted(fit1) %>% tidy()

dat_sub <- 
  dat %>% 
  filter(site %in% c("huggins", "gentry"))


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
  group_by(site, sys_trt) %>% 
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