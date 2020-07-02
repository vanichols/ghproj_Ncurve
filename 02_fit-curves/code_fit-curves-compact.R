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

# The 'experiment' is measuring nitrate leaching at several nitrogen fertilization rates in 
#  corn grown in 2 rotations (corn-corn, corn-soybean)
# The experiment was done at 9 sites.
# At each site it was done for 19 years.
# Years are essentially the 'replicates'


# I am interested in the nitrate leaching vs nitrogen fertilization rate curve
# I want to know how important the 'site' is and 'rotation' in determining the params of the curve

dat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                  "sc" = "cs"))

# leaching ----------------------------------------------------------------

leach <- dat %>% 
  select(site_id, year, yearF, rotation, leaching_kgha, nrate_kgha) 
  
#--a glimpse of the data
ggplot(data = leach, aes(x = nrate_kgha, y = leaching_kgha)) + 
  facet_wrap(~ rotation) + 
  geom_jitter()

## Here an important decision is what nonlinear model to use
## Possible options are:
## 1. SSexpf
## 2. SSblin
## 3. SSexplin

fm1 <- nls(leaching_kgha ~ SSexpf(nrate_kgha, a, c), data = leach)
fm2 <- nls(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c), data = leach)
fm3 <- nlsLM(leaching_kgha ~ SSexplin(nrate_kgha, cm, rm, tb), data = leach)

anova(fm1, fm2, fm3)
anova(fm2, fm3) #--is using anova on nls objects ok? they aren't nested models...


AIC(fm1, fm2, fm3) #--binlinear has lowest aic

## while the explin actually 'fits' better, the residuals show it is a bad model for this data.
## blin is the winner, as it also makes more biological sense 

# bilinear fit ------------------------------------------------------------

leach1 <- 
  leach %>%
  mutate(eu = paste(site_id, year, rotation, sep = "_")) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

leachG <- groupedData(leaching_kgha ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)

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

fxf2 <- fixef(lmod2) #--now we have a value for cc and cs

#--add raneff for eu nested within site (?)
lmod3a <- update(lmod2, 
                random = list(site_id = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site_id/eu)

## Fewer outliers, this is a better model
plot(lmod3a, id = 0.001)

intervals(lmod3a) 

#--interclass correlation
VarCorr(lmod3a)

#--get % of variances
tibble(aparam = c("a", "b", "c"),
       site_var = as.numeric(VarCorr(lmod3a)[2:4]),
       eu_var = as.numeric(VarCorr(lmod3a)[6:8]),
       res_var = as.numeric(VarCorr(lmod3a)[9])) %>% 
  mutate(tot = site_var + eu_var + res_var,#--where should I add the residual?
         pct_site = site_var/tot,
         pct_eu = eu_var/tot)


#--plot the residual against the rainfall

raincentered <- 
  rawdat %>% 
  select(site_id, annual_rain_mm, year) %>% 
  distinct() %>% 
  group_by(site_id) %>% 
  mutate(cannual_rain_mm = scale(annual_rain_mm, center = T))

drainagecentered <- 
  rawdat %>% 
  select(site_id, drainage_mm, year) %>% 
  distinct() %>% 
  group_by(site_id) %>% 
  mutate(cdrainage_mm = scale(drainage_mm, center = T))


#--maybe variance is getting larger?
# but after removing the effect of the site, the rainfall doesn't matter much
dat %>% 
  left_join(raincentered) %>% 
  mutate(resid = resid(lmod3a)) %>% #--use standardized resid instead 
  ggplot(aes(cannual_rain_mm, resid)) + 
  geom_point(aes(color = site_id)) +
  geom_smooth(method = "lm", se = F, aes(color = site_id))


#--I need to figure out how to 'smooth' the predictions

newdat <- dat %>% 
  select(year, site_id, rotation) %>%
  distinct() %>% 
  expand_grid(nrate_kgha = seq(0, 250, by = 10))

leachG$prds <- predict(lmod3a, level = 0, newdata = newdat)



sim_leach <- simulate_nlme(fmm3a, nsim = 1000, psim = 1, level = 0)
leachG$mn.s <- apply(sim_leach, 1, mean)
leachG$mxn.s <- apply(sim_leach, 1, max)
leachG$mnn.s <- apply(sim_leach, 1, min)


ggplot() + 
  #  geom_point(data = leachG, aes(x = nrate_kgha, y = leaching_kgha)) + 
  # geom_ribbon(data = leachG, 
  #             mapping = aes(x = nrate_kgha, 
  #                           ymin = mxn.s, 
  #                           ymax = mnn.s, fill = rotation), 
  #             alpha = 0.5) + 
  geom_point(data = leachG, aes(x = nrate_kgha, 
                               y = prds, 
                               color = rotation), size = 2) +
  labs(y = "leaching_kgha")




# model exploration -------------------------------------------------------


## Parameter values and contrast among groups
emmeans(fmm3a, ~ rotation, param = "a")
emmeans(fmm3a, ~ rotation, param = "b")
emmeans(fmm3a, ~ rotation, param = "xs")
emmeans(fmm3a, ~ rotation, param = "c")
## Contrasts
contrast(emmeans(fmm3a, ~ rotation, param = "a"), "pairwise")
#--a doesn't vary by rotation. We saw above it is more depenedent upon the site?
#--all of these do depend on rotation: 
contrast(emmeans(fmm3a, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(fmm3a, ~ rotation, param = "xs"), "pairwise")
contrast(emmeans(fmm3a, ~ rotation, param = "c"), "pairwise")



# no3 ----------------------------------------------------------------


no3 <- 
  dat %>% 
  select(site_id, year, yearF, rotation, nitrateflow_mg_l, nrate_kgha) 

#--a glimpse of the data
ggplot(data = no3, aes(x = nrate_kgha, y = nitrateflow_mg_l)) + 
  facet_wrap(~ rotation) + 
  geom_jitter()

## Here an important decision is what nonlinear model to use
## Possible options are:
## 1. SSexpf
## 2. SSblin
## 3. SSexplin

fmn1 <- nls(nitrateflow_mg_l ~ SSexpf(nrate_kgha, a, c), data = no3)
fmn2 <- nls(nitrateflow_mg_l ~ SSblin(nrate_kgha, a, b, xs, c), data = no3)
fmn3 <- nlsLM(nitrateflow_mg_l ~ SSexplin(nrate_kgha, cm, rm, tb), data = no3)

anova(fmn1, fmn2, fmn3)
anova(fmn2, fmn3) #--is using anova on nls objects ok? they aren't nested models...


AIC(fmn1, fmn2, fmn3) #--binlinear has lowest aic


# bilinear fit ------------------------------------------------------------

no31 <- 
  no3 %>%
  mutate(eu = paste(site_id, year, rotation, sep = "_")) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

no3G <- groupedData(nitrateflow_mg_l ~ nrate_kgha | eu, data = no31)
no3G$rotation <- as.factor(no3G$rotation)

#--fit model to each group
fmnG <- nlsList(nitrateflow_mg_l ~ SSblin(nrate_kgha, a, b, xs, c), data = no3G) #--non-converg doesn't 
#matter
plot(intervals(fmnG))
#--looks similar to leaching

#--a, b, and c could have random components added
fmnm1 <- nlme(fmnG, random = pdDiag(a + b + c ~ 1))
plot(fmnm1)
plot(fmnm1, id = 0.000001) #--fernando likes things to be under 2
plot(fmnm1, id = 0.01) #--eek. 


#--add fixed effect of rotation
fxf1 <- fixef(fmnm1) 
fmnm2 <- update(fmnm1, 
               fixed = list(a + b + xs + c ~ rotation),
               start = c(fxf1[1], 0, #--a
                         fxf1[2], 0, #--b
                         fxf1[3], 0, #--xs
                         fxf1[4], 0)) #--c

fxf2 <- fixef(fmnm2) #--now we have a value for cc and cs


#--could try having the variance increase with higher values
#--doesn't converge
#fmnm3 <- update(fmnm2, weights = varPower())
#plot(fmnm3, id = 0.01)

#--compare them, this code is not working
# par(mfrow=c(2,1))
# plot(fmnm3, id = 0.01, main="varPower")
# plot(fmnm2, id = 0.01, main="no variance modelling")

#--instead of modelling the variance, try adding two random effect levels
# I don't quite get this code notation, someone wrote it for me 

fmnm3a <- update(fmnm2, 
                random = list(site_id = pdDiag(a + b + c ~ 1),
                              eu = pdDiag(a + b + c ~ 1)),
                groups = ~ site_id/eu)

## Fewer outliers, this is a better model
plot(fmnm3a, id = 0.001)


#--do I even need b and c to vary?
fmnm3b <- update(fmnm2, 
                 random = list(site_id = pdDiag(a ~ 1),
                               eu = pdDiag(a ~ 1)),
                 groups = ~ site_id/eu)
plot(fmnm3b, id = 0.001)
#--yes. This looks terrible. 


# explore model -----------------------------------------------------------

#--what is this telling me?
intervals(fmnm3a) 

#--site-effects
ranef(fmnm3a)$site_id %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  clean_names() %>% 
  pivot_longer(a_intercept:c_intercept) %>% 
  ggplot(aes(reorder(rowname, value), value)) + 
  geom_point() + 
  facet_grid(.~name) + 
  coord_flip()

#--eu-effects
ranef(fmnm3a)[[2]] %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  clean_names() %>%
  separate(col = rowname, into = c("site", "site_year-rot"), sep = "/") %>% 
  separate(`site_year-rot`, sep = "_", into = c("site", "year", "rot")) %>% 
  pivot_longer(c(a_intercept, b_intercept, c_intercept)) %>%
  rename(site_id = site) %>%
  mutate(year = as.numeric(year)) %>% 
  left_join(dat) %>% 
  filter(name == "a_intercept") %>% 
  ggplot(aes(reorder(year, value), value)) + 
  geom_point(aes(size = drainage_mm)) + 
  facet_grid(rot~site_id) 



#--interclass correlation
VarCorr(fmnm3a)

tibble(aparam = c("a", "b", "c"),
       site_var = as.numeric(VarCorr(fmnm3a)[2:4]),
       eu_var = as.numeric(VarCorr(fmnm3a)[6:8]),
       res_var = as.numeric(VarCorr(fmnm3a)[9])) %>% 
  mutate(tot = site_var + eu_var + res_var,#--where should I add the residual?
         pct_site = site_var/tot,
         pct_eu = eu_var/tot,
         pct_er = res_var/tot) %>%
  pivot_longer(pct_site:pct_er) %>% 
  ggplot(aes(x = "", y =value, fill = name)) + 
  geom_bar(stat="identity", width=1) +
           coord_polar("y", start=0) + 
  facet_grid(.~aparam)


pl_no3 <- 
  tibble(aparam = c("a", "b", "c"),
       site_var = as.numeric(VarCorr(fmnm3a)[2:4]),
       eu_var = as.numeric(VarCorr(fmnm3a)[6:8]),
       res_var = as.numeric(VarCorr(fmnm3a)[9])) %>% 
  mutate(tot = site_var + eu_var + res_var,#--where should I add the residual?
         pct_site = site_var/tot,
         pct_eu = eu_var/tot,
         pct_er = res_var/tot) %>%
  pivot_longer(pct_site:pct_er) %>% 
  ggplot(aes(x = "", y =value, fill = name)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  facet_grid(.~aparam) + 
  labs(title = "NO3 Flow\n % Variance for Each Parameter")



## Parameter values and contrast among groups
emmeans(fmnm3a, ~ rotation, param = "a")
emmeans(fmnm3a, ~ rotation, param = "b")
emmeans(fmnm3a, ~ rotation, param = "xs")
emmeans(fmnm3a, ~ rotation, param = "c")
## Contrasts
contrast(emmeans(fmnm3a, ~ rotation, param = "a"), "pairwise")
#--a doesn't vary by rotation. We saw above it is more depenedent upon the site?
#--all of these do depend on rotation: 
contrast(emmeans(fmnm3a, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(fmnm3a, ~ rotation, param = "xs"), "pairwise")
contrast(emmeans(fmnm3a, ~ rotation, param = "c"), "pairwise")

