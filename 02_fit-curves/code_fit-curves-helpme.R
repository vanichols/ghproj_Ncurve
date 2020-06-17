# author: gina
# created; 4/10/2020
# purpose: work through FEM example w/my data
# last updated: 4/16/2020
#               4/17/2020 follow FEM new personalized example
#               4/20/2020 new following w/updated data
#               6/11/2020 new data again
#               6/16/2020 cleaned up, trying to distill my questions


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


# data prep ------------------------------------------------------------

leach <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  select(site_id, year, rotation, leaching_kgha, nrate_kgha) %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                 "sc" = "cs"))

ggplot(data = leach, aes(x = nrate_kgha, y = leaching_kgha, color = yearF)) + 
  facet_wrap(~ rotation) + 
  geom_point()

## Here an important decision is what nonlinear model to use
## Possible options are:
## 1. SSexpf
## 2. SSblin
## 3. SSexplin

fm1 <- nls(leaching_kgha ~ SSexpf(nrate_kgha, a, c), data = leach)
fm2 <- nls(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c), data = leach)
fm3 <- nlsLM(leaching_kgha ~ SSexplin(nrate_kgha, cm, rm, tb), data = leach)

anova(fm1, fm2, fm3)
anova(fm2, fm3)

## while the explin actually 'fits' better, the residuals show it is a bad model for this data.
## blin is the winner. 


# bilinear fit ------------------------------------------------------------

leach1 <- 
  leach %>%
  mutate(eu = paste0(site_id,"_", year, rotation)) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

leachG <- groupedData(leaching_kgha ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)

#--fit model to each group
fmG <- nlsList(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c), data = leachG) #--non-converg doesn't matter
plot(intervals(fmG))

#--a, b, and c could have random components added
fmm1 <- nlme(fmG, random = pdDiag(a + b + c ~ 1))
plot(fmm1)
plot(fmm1, id = 0.000001) #--fernando likes things to be under 2
plot(fmm1, id = 0.01) #--eek. 

head(coef(fmm1)) #--notice xs is the same for everything
intervals(fmm1) #--random effect intervals are reasonable

fxf1 <- fixef(fmm1) #--we need these if we want to add an effect of rotation

#--note this an update, it keeps the random effects, we are just adding a fixed effect
fmm2 <- update(fmm1, 
               fixed = list(a + b + xs + c ~ rotation),
               start = c(fxf[1], 0, #--a
                         fxf[2], 0, #--b
                         fxf[3], 0, #--xs
                         fxf[4], 0)) #--c

fxf2 <- fixef(fmm2) #--now we have a value for cc and cs

anova(fmm2) #--everything is not 0. not that informative
intervals(fmm2) #--still well constrained estimates
plot(fmm2, id = 0.01) #--outliers mean maybe we could model the residual variance better. Don't know how to do that.

#--take a look at a few outliers
leach1 %>% 
  mutate(pred = predict(fmm2)) %>% 
  filter(eu %in% c("hugg_2010cc", "hoff_2014cc", "rand_2010cc")) %>% 
  ggplot(aes(nrate_kgha, leaching_kgha)) + 
  geom_line(aes(nrate_kgha, pred), color = "red") + 
  geom_point() + 
  facet_wrap(~eu)

#--this is too much to look at, not sure how to make it digestable
plot(augPred(fmm2, level = 0:1))


#--could try having the variance increase with higher values
fmm3 <- update(fmm2, weights = varPower())
plot(fmm3, id = 0.01)

#--compare them ?not working grr
par(mfrow=c(2,1))
plot(fmm3, id = 0.01, main="varPower")
plot(fmm2, id = 0.01, main="no variance modelling")

#--FEM does something I don't quite get. 
# instead of modelling the variance, try adding two random effect levels?
fmm3a <- update(fmm2, random = list(site_id = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site_id/eu)

## Fewer outliers, this is a better model
plot(fmm3a, id = 0.001)

intervals(fmm3a) 
#--I feel like these are telling me something about between site variation and w/in site var.
#--It seems that site is absorbing more variance than the eu for a?
ranef(fmm3a)

# --interclass correlation
VarCorr(fmm3a)

tibble(aparam = c("a", "b", "c"),
       site_var = as.numeric(VarCorr(fmm3a)[2:4]),
       eu_var = as.numeric(VarCorr(fmm3a)[6:8])) %>% 
  mutate(tot = site_var + eu_var,
         pct_site = site_var/tot)

#--can I interpret this as there is more variation explained by site for the parameter a?

## Parameter values and contrast among groups
emmeans(fmm3a, ~ rotation, param = "a")
emmeans(fmm3a, ~ rotation, param = "b")
emmeans(fmm3a, ~ rotation, param = "xs")
emmeans(fmm3a, ~ rotation, param = "c")
## Contrasts
contrast(emmeans(fmm3a, ~ rotation, param = "a"), "pairwise")
#--a doesn't vary by rotation. We saw above it is more depenedent upon the site?
#--all of these do: 
contrast(emmeans(fmm3a, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(fmm3a, ~ rotation, param = "xs"), "pairwise")
contrast(emmeans(fmm3a, ~ rotation, param = "c"), "pairwise")

