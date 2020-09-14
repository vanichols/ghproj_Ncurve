# author: gina
# created; 7/2/2020
# purpose: have fitting occur in compact way
# last updated: 8/20/2020, running without sutherland

rm(list = ls())

# libraries ---------------------------------------------------------------

#remotes::install_github("femiguez/nlraa")
library(nlraa)
library(nlme)
library(dplyr) #--overwrites collapse from nlme
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(emmeans)
library(car) #--overwrites recode in dplyr
library(minpack.lm)
library(janitor)
library(broom)



# data prep ---------------------------------------------------------------

dat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                  "sc" = "cs"),
         rotation = as.factor(rotation)) %>%
  select(nrate_kgha, everything()) %>% 
  pivot_longer(annual_rain_mm:yield_maize_buac) %>% 
  select(-date, -doy)

leach <- 
  dat %>% 
  filter(name == "leaching_kgha") %>% 
  filter(crop == "corn")

leach1 <- 
  leach %>%
  mutate(eu = paste(site_id, year, rotation, sep = "_")) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

leachG <- groupedData(value ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)


# fit the model -----------------------------------------------------------

lmodG <- nlsList(value ~ SSblin(nrate_kgha, a, b, xs, c), data = leachG) 
lmod1 <- nlme(lmodG, random = pdDiag(a + b + c ~ 1)) #--add random effects
#--add fixed effect of rotation
fxf1 <- fixef(lmod1) 

lmod2 <- update(lmod1, 
               fixed = list(a + b + xs + c ~ rotation),
               start = c(fxf1[1], 0, #--a
                         fxf1[2], 0, #--b
                         fxf1[3], 0, #--xs
                         fxf1[4], 0)) #--c
fxf2 <- fixef(lmod2) #--now we have a value for cc and cs
#--add raneff for eu nested within site
lmod3a <- update(lmod2, 
                random = list(site_id = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site_id/eu)


# use model to make preds -------------------------------------------------

#-make predictions so I can make graphs

nrates <- seq(0, 300)

lmod3a
summary(lmod3a)


#--at level of eu
pred_dat <- 
  leachG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  mutate(preds = predict(lmod3a, newdata = .))

pred_dat %>% write_csv("02_fit-curves/fc_leach-preds-eu.csv")


#--at level of eu
new_dat <- 
  leachG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) 

pred_dat1 <- predict(lmod3a, newdata = new_dat, level = 0:1)

# leachG %>% 
#   select(yearF, rotation, site_id, eu) %>% 
#   expand_grid(., nrates) %>% 
#   rename(nrate_kgha = nrates) %>% 
#   left_join(pred_dat1)

#--at level of crop rotation
pred_dat2 <- 
  leachG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  mutate(preds = predict(lmod3a, newdata = ., level = 0)) %>% 
  select(rotation, nrate_kgha, preds) %>% 
  distinct() %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                          "cc" = "Continuous Maize",                        
                          "cs" = "Rotated Maize"))

pred_dat2 %>% write_csv("02_fit-curves/fc_leach-preds-rot.csv")

