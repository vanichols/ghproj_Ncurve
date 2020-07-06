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
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
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
                                  "sc" = "cs")) %>%
  select(nrate_kgha, everything()) %>% 
  pivot_longer(annual_rain_mm:yield_maize_buac) %>% 
  select(-date, -doy)

# leaching ----------------------------------------------------------------

leach <- 
  dat %>% 
  filter(name == "leaching_kgha")

#--a glimpse of the data
ggplot(data = leach, aes(x = nrate_kgha, y = value)) + 
  facet_wrap(~ rotation) + 
  geom_jitter()

## Here an important decision is what nonlinear model to use
## Possible options are:
## 1. SSexpf
## 2. SSblin
## 3. SSexplin

fm1 <- nls(value ~ SSexpf(nrate_kgha, a, c), data = leach)
fm2 <- nls(value ~ SSblin(nrate_kgha, a, b, xs, c), data = leach)
fm3 <- nlsLM(value ~ SSexplin(nrate_kgha, cm, rm, tb), data = leach)

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

leachG <- groupedData(value ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)

#--fit model to each group
lmodG <- nlsList(value ~ SSblin(nrate_kgha, a, b, xs, c), data = leachG) 

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



# extract coefficients ----------------------------------------------------

coefs <- 
  broom::tidy(lmod3a, effects = "random", conf.int=TRUE) %>% 
  as_tibble() %>% 
  separate(level, into = c("x", "eu"), sep = "/") %>% 
  separate(eu, into = c("site_id", "year", "rotation")) %>% 
  mutate(term2 = str_sub(term, 1, 2),
         term2 = str_replace_all(term2, "[[:punct:]]", "")) %>% 
  select(site_id, year, rotation, term2, estimate)

#--shit we didn't have xs vary by anything, only cropping system. 
coefs %>% 
  filter(term2 == "xs")

write_csv(coefs, "02_fit-curves/fc_blin-leach-parms-mm.csv")

# rainfall? ---------------------------------------------------------------

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

#--How to get xs values for each eu?
coefs <- coef(lmod3a, level = 1)

str(coefs)



## coefficients (group-level estimates)
(rcoef1 <- tidy(lmm1, effects = "ran_coefs"))
if (require(tidyr) && require(dplyr)) {
  ## reconstitute standard coefficient-by-level table
  spread(rcoef1,key=term,value=estimate)
  ## split ran_pars into type + term; sort fixed/sd/cor
  (tt %>% separate(term,c("type","term"),sep="__",fill="left")
    %>% arrange(!is.na(type),desc(type)))
}
head(augment(lmm1, sleepstudy))
glance(lmm1)


coefs <- broom::tidy(coefs) 

sim_leach <- simulate_nlme(fmm3a, nsim = 1000, psim = 1, level = 0)
leachG$mn.s <- apply(sim_leach, 1, mean)
leachG$mxn.s <- apply(sim_leach, 1, max)
leachG$mnn.s <- apply(sim_leach, 1, min)


ggplot() + 
  #  geom_point(data = leachG, aes(x = nrate_kgha, y = value)) + 
  # geom_ribbon(data = leachG, 
  #             mapping = aes(x = nrate_kgha, 
  #                           ymin = mxn.s, 
  #                           ymax = mnn.s, fill = rotation), 
  #             alpha = 0.5) + 
  geom_point(data = leachG, aes(x = nrate_kgha, 
                               y = prds, 
                               color = rotation), size = 2) +
  labs(y = "value")




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


# yields ----------------------------------------------------------------

ylds <- 
  dat %>% 
  filter(name == "yield_maize_buac")

#--a glimpse of the data
ggplot(data = ylds, aes(x = nrate_kgha, y = value)) + 
  facet_wrap(~ rotation) + 
  geom_jitter()

## Here an important decision is what nonlinear model to use
## Possible options are:
## 1. SSlinp
## 2. ?

fm1 <- nls(value ~ SSlinp(nrate_kgha, a, b, xs), data = ylds)

# linear plat fit ------------------------------------------------------------

ylds1 <- 
  ylds %>%
  mutate(eu = paste(site_id, year, rotation, sep = "_")) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

yldsG <- groupedData(value ~ nrate_kgha | eu, data = ylds1)
yldsG$rotation <- as.factor(yldsG$rotation)

#--fit model to each group
lmodG <- nlsList(value ~ SSlinp(nrate_kgha, a, b, xs), data = yldsG) 
plot(intervals(lmodG))

#--a, b, and xs could have random components added
lmod1 <- nlme(lmodG, random = pdDiag(a + b + xs ~ 1))
plot(lmod1)
plot(lmod1, id = 0.000001) #--fernando likes things to be under 2
plot(lmod1, id = 0.01) #--eek. 

intervals(lmod1) #--random effect intervals are reasonable

fxf1 <- fixef(lmod1) #--we need these if we want to add an effect of rotation

#--note this an update, it keeps the random effects, we are just adding a fixed effect
lmod2 <- update(lmod1, 
               fixed = list(a + b + xs ~ rotation),
               start = c(fxf1[1], 0, #--a
                         fxf1[2], 0, #--b
                         fxf1[3], 0)) #--xs

fxf2 <- fixef(lmod2) #--now we have a value for cc and cs

intervals(lmod2) #--still well constrained estimates
plot(lmod2, id = 0.01) #--outliers mean maybe we could model the residual variance better. Don't know how to do that.
lmod2

#--take a look at a few outliers
ylds1 %>% 
  mutate(pred = predict(lmod2)) %>% 
  filter(eu %in% c("gent_2005_cs", "hoff_2001_cc")) %>% 
  ggplot(aes(nrate_kgha, value)) + 
  geom_line(aes(nrate_kgha, pred), color = "red") + 
  geom_point() + 
  facet_wrap(~eu)

#--this is too much to look at, not sure how to make it digestable
#plot(augPred(lmod2, level = 0:1))

## Contrasts, everything varies by rotation
contrast(emmeans(lmod2, ~ rotation, param = "a"), "pairwise")
contrast(emmeans(lmod2, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(lmod2, ~ rotation, param = "xs"), "pairwise")

#--could try having the variance increase with higher values
# doesn't even run
lmod3 <- update(lmod2, weights = varPower())
plot(lmod3, id = 0.01)

#--FEM does something I don't quite get. 
# instead of modelling the variance, try adding two random effect levels?
lmod3a <- update(lmod2, random = list(site_id = pdDiag(a + b + xs ~ 1),
                                    eu = pdDiag(a + b + xs ~ 1)),
                groups = ~ site_id/eu)

#--outliers don't change. how do i decide?
plot(lmod3a, id = 0.001)
plot(lmod2, id = 0.001)

intervals(lmod3a) 
#--I feel like these are telling me something about between site variation and w/in site var.
#--It seems that site is absorbing more variance than the eu for a?

#--interclass correlation, seems like site is helping?
VarCorr(lmod3a)

#--get % of variances
tibble(aparam = c("a", "b", "c"),
       site_var = as.numeric(VarCorr(lmod3a)[2:4]),
       eu_var = as.numeric(VarCorr(lmod3a)[6:8]),
       res_var = as.numeric(VarCorr(lmod3a)[9])) %>% 
  mutate(tot = site_var + eu_var, #--should I add the residual?
         pct_site = site_var/tot,
         pct_eu = eu_var/tot)


# extract coefficients ----------------------------------------------------

coefs <- 
  broom::tidy(lmod3a, effects = "random", conf.int=TRUE) %>% 
  as_tibble() %>% 
  separate(level, into = c("x", "eu"), sep = "/") %>% 
  separate(eu, into = c("site_id", "year", "rotation")) %>% 
  mutate(term2 = str_sub(term, 1, 2),
         term2 = str_replace_all(term2, "[[:punct:]]", "")) %>% 
  select(site_id, year, rotation, term2, estimate)


write_csv(coefs, "02_fit-curves/fc_blin-ylds-parms-mm.csv")
