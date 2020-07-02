# author: gina
# created; 7/2/2020
# purpose: have fitting occur in compact way
# last updated: 

rm(list = ls())

# libraries ---------------------------------------------------------------

#remotes::install_github("femiguez/nlraa")
library(nlraa)
library(nlme)
library(dplyr) #--overwrites collapse from nlme
library(ggplot2)
library(readr)
library(emmeans)
library(car) #--overwrites recode in dplyr
library(minpack.lm)
library(janitor)


# overview ----------------------------------------------------------------

dat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                  "sc" = "cs"))

# leaching ----------------------------------------------------------------

leach <- dat %>% 
  select(site_id, year, yearF, rotation, leaching_kgha, nrate_kgha) %>% 
  mutate(eu = paste(site_id, year, rotation, sep = "_")) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id),
         rotation = as.factor(rotation))

leachG <- groupedData(leaching_kgha ~ nrate_kgha | eu, data = leach)

#--fit model to each group
lmodG <- nlsList(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c), data = leachG) 

#--add random effects
lmod1 <- nlme(lmodG, random = pdDiag(a + b + c ~ 1))

#--add fixed effect of rotation
fxf1 <- fixef(lmod1) 
lmod2 <- update(lmod1, 
               fixed = list(a + b + xs + c ~ rotation),
               start = c(fxf1[1], 0, #--a
                         fxf1[2], 0, #--b
                         fxf1[3], 0, #--xs
                         fxf1[4], 0)) #--c

#--add raneff for eu nested within site (I think)
lmod3a <- update(lmod2, 
                random = list(site_id = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site_id/eu)


#--look at predictions
leachG$prds <- predict(lmod3a, level = 0)

#--this looks more like a tri-linear, maybe it's an optical illusion?
ggplot() + 
  geom_point(data = leachG, aes(x = nrate_kgha, 
                                y = prds, 
                                color = rotation), size = 2) +
  geom_line(data = leachG, aes(x = nrate_kgha, 
                                y = prds, 
                                color = rotation), size = 1) +
  labs(y = "leaching_kgha")



sim_leach <- simulate_nlme(lmod3a, nsim = 100, psim = 1, level = 0)
leachG$mn.s <- apply(sim_leach, 1, mean)
leachG$mxn.s <- apply(sim_leach, 1, max)
leachG$mnn.s <- apply(sim_leach, 1, min)


ggplot() + 
  geom_ribbon(data = leachG,
              mapping = aes(x = nrate_kgha,
                            ymin = mxn.s,
                            ymax = mnn.s, fill = rotation),
              alpha = 0.5) +
  geom_line(data = leachG, aes(x = nrate_kgha, 
                               y = prds, 
                               color = rotation), size = 2) +
  labs(y = "leaching_kgha")



#--I need to figure out how to 'smooth' the predictions

newdat <- dat %>% 
  select(year, site_id, rotation) %>%
  distinct() %>% 
  expand_grid(nrate_kgha = seq(0, 250, by = 10))
