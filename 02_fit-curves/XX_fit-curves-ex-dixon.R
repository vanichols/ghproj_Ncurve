# author: gina
# created; 7/2/2020
# purpose: explore non-linear model fits preparing for stats consult mtg
# last updated: 7/2/2020

rm(list = ls())

# libraries ---------------------------------------------------------------

#remotes::install_github("femiguez/nlraa") #--could be used for sim
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
# At each site it was done for 20 years.
# Years are essentially the 'replicates'


# I am interested in the nitrate leaching vs nitrogen fertilization rate curve
# I want to know how important the 'site' is and 'rotation' in determining the params of the curve

#--make sure year is a factor
leach <- 
  read_csv("01_proc-raw-outs/pro_lunchinators.csv") %>% 
  mutate(yearF = as.factor(year))

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
anova(fm2, fm3) #--is using anova on nls objects ok? they aren't nested models

# use AIC instead?

AIC(fm1, fm2, fm3) #--binlinear has lowest aic

## blin is the winner, it also makes more biological sense 


# bilinear fit ------------------------------------------------------------

leach1 <- 
  leach %>%
  mutate(eu = paste(site_id, year, rotation, sep = "_")) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

leachG <- groupedData(leaching_kgha ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)

#--fit model to each group
lmodG <- nlsList(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c), data = leachG) 
#--add random effects to a b and c
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

#--add raneff for eu nested within site (I think that is what this is doing)
lmod3a <- update(lmod2, 
                random = list(site_id = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site_id/eu)

## Fewer outliers, this is a better model
plot(lmod3a, id = 0.001)

intervals(lmod3a) 

#--interclass correlation
VarCorr(lmod3a)

#--get % of variances?
tibble(aparam = c("a", "b", "c"),
       site_var = as.numeric(VarCorr(lmod3a)[2:4]),
       eu_var = as.numeric(VarCorr(lmod3a)[6:8]),
       res_var = as.numeric(VarCorr(lmod3a)[9])) %>% 
  mutate(tot = site_var + eu_var,#--should I add the residual?
         pct_site = site_var/tot,
         pct_eu = eu_var/tot)


# look at fit -------------------------------------------------------------

#--I need to figure out how to 'smooth' the predictions
leachG$prds <- predict(lmod3a, level = 0)


#--this comes from FEM nlraa, not sure how to smooth these either
sim_leach <- simulate_nlme(lmod3a, nsim = 100, psim = 1, level = 0)
leachG$mn.s <- apply(sim_leach, 1, mean)
leachG$mxn.s <- apply(sim_leach, 1, max)
leachG$mnn.s <- apply(sim_leach, 1, min)

#--there seems to be a problem, the cc looks tri-linear
ggplot() + 
  geom_ribbon(data = leachG,
              mapping = aes(x = nrate_kgha,
                            ymin = mxn.s,
                            ymax = mnn.s, fill = rotation),
              alpha = 0.5) +
  geom_point(data = leachG, aes(x = nrate_kgha, 
                               y = prds, 
                               color = rotation), size = 2) +
  labs(y = "leaching_kgha")


# model exploration -------------------------------------------------------

# which parameters are affected by crop rotation?
contrast(emmeans(lmod3a, ~ rotation, param = "a"), "pairwise")
#--a doesn't vary by rotation. We saw above it is more depenedent upon the site?
#--all of these do depend on rotation: 
contrast(emmeans(lmod3a, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(lmod3a, ~ rotation, param = "xs"), "pairwise")
contrast(emmeans(lmod3a, ~ rotation, param = "c"), "pairwise")


