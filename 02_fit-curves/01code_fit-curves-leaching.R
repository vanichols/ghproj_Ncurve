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
  filter(!is.na(nyear_leach_kgha_tot)) %>% 
  filter(crop != "soy") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                  "sc" = "cs"),
         rotation = as.factor(rotation)) %>%
  select(nrate_kgha, everything()) %>% 
  pivot_longer(leaching_kgha:yield_maize_buac) 

leach <- 
  dat %>% 
  filter(name == "nyear_leach_kgha_tot")

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



## Parameter values and contrast among groups
em_a <- 
  tidy(emmeans(lmod3a, ~ rotation, param = "a")) %>% 
  mutate(parameter = "a")
con_a <- tidy(contrast(emmeans(lmod3a, ~ rotation, param = "a"), "pairwise")) %>% 
  mutate(parameter = "a")

emmeans(lmod3a, ~ 1, param = "a")
intervals(lmod3a)


em_b <- tidy(emmeans(lmod3a, ~ rotation, param = "b")) %>% 
  mutate(parameter = "b")
con_b <- tidy(contrast(emmeans(lmod3a, ~ rotation, param = "b"), "pairwise")) %>% 
  mutate(parameter = "b")

em_xs <- tidy(emmeans(lmod3a, ~ rotation, param = "xs")) %>% 
  mutate(parameter = "xs")
con_xs <- tidy(contrast(emmeans(lmod3a, ~ rotation, param = "xs"), "pairwise")) %>% 
  mutate(parameter = 'xs')

em_c <- tidy(emmeans(lmod3a, ~ rotation, param = "c")) %>% 
  mutate(parameter = "c")
con_c <- tidy(contrast(emmeans(lmod3a, ~ rotation, param = "c"), "pairwise")) %>% 
  mutate(parameter = "c")

ems <- 
  em_a %>% 
  bind_rows(em_b) %>% 
  bind_rows(em_xs) %>% 
  bind_rows(em_c) %>% 
  select(-p.value)

ems %>% write_csv("02_fit-curves/fc_blin-leach-rotation-estimates.csv")

cons <- 
  con_a %>% 
  bind_rows(con_b) %>% 
  bind_rows(con_xs) %>% 
  bind_rows(con_c) %>% 
  mutate(p.value = round(p.value, 3))

cons %>% write_csv("02_fit-curves/fc_blin-leach-rotation-contrasts.csv")


## Contrasts
#--a doesn't vary by rotation. We saw above it is more depenedent upon the site?
#--all of these do depend on rotation: 


# use model to make preds -------------------------------------------------

# just get estimates for each value
coef_est <- 
  coef(lmod3a, effects = "random") %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  separate(rowname, into = c("site", "x", "year", "rotation")) %>% 
  pivot_longer(cols = 5:12, names_to = "term", values_to = "estimate") %>% 
  mutate(term2 = str_sub(term, 1, 2),
                  term2 = str_replace_all(term2, "[[:punct:]]", ""),
                  termrot = ifelse(grepl("cs", term), "csterm", "ccterm")) %>%
  select(-term) %>%
  pivot_wider(names_from = termrot,
              values_from = estimate) %>%
  mutate(est = ifelse(rotation == "cs", ccterm + csterm, ccterm)) %>%
  select(-ccterm, -csterm, -x) %>%
  pivot_wider(names_from = term2, values_from = est)
         

coef_est %>% write_csv("02_fit-curves/fc_blin-leach-parms-mm.csv")


pred_dat <- 
  leachG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  mutate(preds = predict(lmod3a, newdata = .))


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

