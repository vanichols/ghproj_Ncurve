# author: gina
# created; 4/10/2020
# purpose: work through FEM example w/my data
# last updated: 4/16/2020
#               4/17/2020 follow FEM new personalized example
#               4/20/2020 new following w/updated data
#               6/11/2020 new data again


# actual paper example ----------------------------------------------------
#https://cran.r-project.org/web/packages/nlraa/vignettes/nlraa-AgronJ-paper.html


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

## FEM: First plot
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
fm3 <- nlsLM(leaching_kgha ~ SSexplin(nrate_kgha, cm, rm, tb), data = leach) # what is nlsLM?
## FEM: nlsLM is a version of nls which is more robust than nls. it is in the minpack.lm package

## After looking at the data a bit more it is clear that cs is almost
## linear and cc shows in many cases a 'break-point'. For this reason the 
## blinear model seems a reasonable one

anova(fm1, fm2, fm3)
anova(fm2, fm3)

# 6/11 now with the new data, it seems the explinear is the best
#--what does an explin look like?
# cm is related to max growth during linear phase
# rm is max growth during exponential phase
# tb is time when switch happens
x <- seq(1,100, by = 5)
y <- explin(x, cm = 20, rm = 0.14, tb = 10) + rnorm(length(x), 0, 5)
y <- abs(y)
dat <- data.frame(x = x, y = y)
fit <- nls(y ~ SSexplin(x, cm, rm, tb), data = dat)
## plot
ggplot(data = dat, aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit)))


ggplot(data = leach, aes(x = nrate_kgha, y = leaching_kgha)) +
  geom_point() + 
  geom_line(aes(y = fitted(fm3)), color = "red") +
  geom_line(aes(y = fitted(fm2)), color = "blue") +
  ggtitle("red = Expolinear Fit. \n blue = blin")




# try FEM logic on expolin ------------------------------------------------

leach1 <- 
  leach %>%
  mutate(eu = paste0(site_id,"_",year, rotation)) %>%
  mutate(site_id = as.factor(site_id))

leachG <- groupedData(leaching_kgha ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)

fmL <- nlsList(leaching_kgha ~ SSexplin(nrate_kgha, cm, rm, tb), data = leachG) 
#--don't worry about convergence errors, but it is worse than the blin

# cm is growth during 2nd linear phase
plot(intervals(fmL))

# Without a fixed effect defined, it's fitting a grand model, but
#  allowing cm and tb to vary for each eu
fmm <- nlme(fmL, random = pdDiag(cm + tb ~ 1))
fmm

plot(fmm) #--these look bad
# pattern of under-pred, followed by over-pred
# I don't think the expolinear is good.

plot(fmm, id = 0.000001) #--fernando likes things to be under 2
coef(fmm) 

intervals(fmm)
# the random effect intervals seem to be well constrained

fxf <- fixef(fmm)

## Incorporating the effect of rotation is a bit tricky, 
## We need the parameter estimates from the previous model
## as starting values and we put zeros for the starting value
## of the 'treatment effect'
fmm2 <- update(fmm, fixed = list(cm + rm + tb ~ rotation),
               start = c(fxf[1], 0, #--cm
                         fxf[2], 0, #--rm
                         fxf[3], 0)) #--tb

fxf2 <- fixef(fmm2)
anova(fmm2) #--not sure what this actually tells me
intervals(fmm2)
# These shows that the parameters are still well constrained

plot(fmm2, id = 0.01) #--this shows our model is bad, I think

plot(augPred(fmm2, level = 0:1))

## Testing simulate_nlme
## This is for the MEAN function
if(packageVersion("nlraa") < '0.61') stop("need latest version of nlraa")

fmm2.sim1 <- simulate_nlme(fmm2, nsim = 100, psim = 1, level = 0)

fmm3 <- update(fmm2, weights = varPower())

#--this shows the opposite - higher variance at low values
plot(fmm3)


leachG$mn.s <- apply(fmm2.sim1, 1, mean)
leachG$mxn.s <- apply(fmm2.sim1, 1, max)
leachG$mnn.s <- apply(fmm2.sim1, 1, min)


ggplot() + 
  geom_point(data = leachG, aes(x = nrate_kgha, y = leaching_kgha)) + 
  geom_ribbon(data = leachG, 
              mapping = aes(x = nrate_kgha, 
                            ymin = mxn.s, 
                            ymax = mnn.s, fill = rotation), 
              alpha = 0.5) + 
  ggtitle("Simualtion at level 0")

## What if I try to simualte at level 1?
fmm2.sim11 <- simulate_nlme(fmm2, nsim = 100, psim = 1, level = 1)
## Need to process this matrix correctly still

leachG$mn.s11 <- apply(fmm2.sim11, 1, mean)
leachG$mxn.s11 <- apply(fmm2.sim11, 1, max)
leachG$mnn.s11 <- apply(fmm2.sim11, 1, min)



fmm3 <- update(fmm2, random = list(site_id = pdDiag(cm + rm + tb ~ 1),
                                   eu = pdDiag(cm + rm + tb ~ 1)),
               groups = ~ site_id/eu)

ggplot() + 
  geom_point(data = leachG, aes(x = nrate_kgha, y = leaching_kgha)) + 
  geom_ribbon(data = leachG, 
              mapping = aes(x = nrate_kgha, 
                            ymin = mxn.s11, 
                            ymax = mnn.s11, fill = rotation), 
              alpha = 0.5) + 
  ggtitle("Simualtion at level 0")


anova(fmm3)

intervals(fmm3)

## Fewer outliers, this is a better model
plot(fmm3, id = 0.001)

## Parameter values and contrast among groups
emmeans(fmm3, ~ rotation, param = "cm")
emmeans(fmm3, ~ rotation, param = "rm")
emmeans(fmm3, ~ rotation, param = "tb")
## Contrasts
contrast(emmeans(fmm3, ~ rotation, param = "cm"), "pairwise")
contrast(emmeans(fmm3, ~ rotation, param = "rm"), "pairwise")
contrast(emmeans(fmm3, ~ rotation, param = "tb"), "pairwise")


## This is not a perfect model by any means,
## but it is something to get started

leachG$prds <- predict(fmm3, level = 0)

ggplot(data = leachG, aes(x = nrate_kgha, y = leaching_kgha)) + 
  geom_jitter() + 
  facet_wrap( ~ rotation) + 
  geom_line(aes(y = prds), size = 2, color = "red")



# ok me -------------------------------------------------------------------

fit1 <- nls(leaching_kgha ~ SSplin(nrate_kgha, a, xs, b), data = leach)

ggplot(data = leach, aes(x = nrate_kgha, y = leaching_kgha)) +
  geom_point() + 
  geom_line(aes(y = fitted(fit1))) + 
  ggtitle("Plateua Linear")

# note: originally rotation wasn't included in the eu definition. 
leach1 <- leach %>%
  mutate(eu = paste0(site_id,"_",year, "_", rotation))

leachG <- groupedData(leaching_kgha ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)

gnL <- nlsList(leaching_kgha ~ SSplin(nrate_kgha, a, xs, b), data = leachG) 

plot(gnL)
plot(intervals(gnL))
## Very strong heterogeneous variance
# more variability in b than in a or xs
# a is the initial plateua, xs is the pivot point, b is the later slope

# so lets b be a random factor (?) need to read nlme
gnm <- nlme(gnL, random = pdDiag(a + b ~ 1))

gnm
## Not a lot of random variability 

## Need to account for the increasing variance (?)
# It doesn't actually seem so bad. Problems, but...
plot(gnm)


# what are those points at 0?


# try to reproduce the resid plot, but use plotly -------------------------

# what is this plot pulling from?
# example from plot class nlme: 
# plot(fm1, resid(., type = "p") ~ fitted(.) | Sex, abline = 0)
# 
# resid(gnm, type = "p")
# 
# # # ?? what is type?
# # an optional character string specifying the type of residuals to be used. If "response", as by default, the “raw” residuals (observed - fitted) are used; else, if "pearson", the standardized residuals (raw residuals divided by the corresponding standard errors) are used; else, if "normalized", the normalized residuals (standardized residuals pre-multiplied by the inverse square-root factor of the estimated error correlation matrix) are used. Partial matching of arguments is used, so only the first character needs to be provided.
# 
# 
# leachG$resids <- resid(gnm, type = "p")
# leachG$fits <- fitted(gnm)
# 
# gnp1 <- 
#   leachG %>% 
#   ggplot(aes(fits, resids, color = site, shape = as.factor(year))) +
#   geom_point() + 
#   guides(color = F)
# 
# library(plotly)
# ggplotly(gnp1)


plot(augPred(gnm, level = 0:1))

#--should I normalize things so the value at 0 is always '0'?

fxf <- fixef(gnm)

gnm2 <- update(gnm, fixed = list(a + xs + b ~ rotation),
               random = a + b ~ 1,
               start = c(fxf[1], 0, fxf[2], 0, fxf[3], 0))

## There is a strong effect of cropping system on 
## all aspects of leaching
anova(gnm2)

plot(augPred(gnm2, level = 0:1))

## This shows some outliers
plot(gnm2, id = 0.01)

# this variance doesn't actually look that bad. 

## Trying to account for the increasing variance
# This has a good explanation of methods to do this:
# https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd452.htm
# I think transformation isn't a good option in our case bc
# we are very interested in the raw relationship
gnm3 <- update(gnm2, weights = varPower())

#--this shows the opposite - higher variance at low values
plot(gnm3)

anova(gnm3)



