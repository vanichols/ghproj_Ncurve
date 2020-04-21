# author: gina
# created; 4/10/2020
# purpose: work through FEM example w/my data
# last updated: 4/16/2020
#               4/17/2020 follow FEM new personalized example


# actual paper example ----------------------------------------------------
#https://cran.r-project.org/web/packages/nlraa/vignettes/nlraa-AgronJ-paper.html
library(nlraa)
library(nlme)
library(dplyr)
library(ggplot2)
library(readr)
library(emmeans)
library(car)
library(minpack.lm)

# my data prep ------------------------------------------------------------
#--my data
#= Yep, Gentry is certainly weird
leach <- read_csv("data/tidy/td_crop-by-year.csv") %>% 
  select(site, year, cropsys, crop, leaching_kgha, n_rate) %>% 
  arrange(site, year, n_rate) %>% 
  filter(crop == 'corn') %>%
  filter(site != "gentry") %>%
  mutate(year.f = as.factor(year))

## FEM: First plot
ggplot(data = leach, aes(x = n_rate, y = leaching_kgha, color = year)) + 
  facet_wrap(~ cropsys) + 
  geom_point()

## Here an important decision is what nonlinear model to use
## Possible options are:
## 1. SSexpf
## 2. SSblin
## 3. SSexplin

fm1 <- nls(leaching_kgha ~ SSexpf(n_rate, a, c), data = leach)
fm2 <- nls(leaching_kgha ~ SSblin(n_rate, a, b, xs, c), data = leach)
fm3 <- nlsLM(leaching_kgha ~ SSexplin(n_rate, cm, rm, tb), data = leach)

## After looking at the data a bit more it is clear that sc is almost
## linear and cc shows in many cases a 'break-point'. For this reason the 
## blinear model seems a reasonable one

## According to this the blin is the better model
## But not that different from SSexplin
anova(fm1, fm2, fm3)

## This seems like a reasonable model
ggplot(data = leach, aes(x = n_rate, y = leaching_kgha)) +
  geom_point() + 
  geom_line(aes(y = fitted(fm2))) + 
  ggtitle("Bilinear fit. \n Do we need something more complex?")

ggplot(data = leach, aes(x = n_rate, y = leaching_kgha)) +
  geom_point() + 
  geom_line(aes(y = fitted(fm3))) + 
  ggtitle("Expolinear fit. \n Do we need something more complex?")


# note: fernando didn't have cropsys in his eu def. keep it 'wrong' so I understand how he is interpreting things
# FEM: 2020-04-21. I added cropsys here, but one thing I noteice later (it was not obvious at first) is that
# this is an unbalanced data set, meaning that cc is present in every year but sc is present every other year
leach1 <- leach %>%
  mutate(eu = paste0(cropsys,"_",site,"_",year)) %>%
  mutate(site = as.factor(site))

leachG <- groupedData(leaching_kgha ~ n_rate | eu, data = leach1)
leachG$cropsys <- as.factor(leachG$cropsys)

fmL <- nlsList(leaching_kgha ~ SSblin(n_rate, a, b, xs, c), data = leachG) 

plot(fmL)
plot(intervals(fmL))
## these plots are always VERY informative. plot(fmL) shows the residuals
## from individually fitting a nonlinear model to each 'experimental unit'
## the intervals plot shows that 'c' is varying systematically for different 
## experimental units. 'xs' can be hard to esimate because some units have 
## very large confidence intervals. 'a' and 'b' are likely to vary according to
## factors in the 'design' of this experiment

## The residual variance is mostly ok, but there are some 
## significant outliers, not that many though

## I had to eliminate xs here for this to converge
fmm <- nlme(fmL, random = pdDiag(a + b + c ~ 1))

fmm
## this model looks fine
intervals(fmm)
## the random effect intervals seem to be well constrained

## Some outliers, mostly from gold
plot(fmm, id = 0.01)

fxf <- fixef(fmm)

## Incorporating the effect of cropsys is a bit tricky, 
## We need the parameter estimates from the previous model
## as starting values and we put zeros for the starting value
## of the 'treatment effect'
fmm2 <- update(fmm, fixed = list(a + b + xs + c ~ cropsys),
               start = c(fxf[1], 0, fxf[2], 0, fxf[3], 0, fxf[4], 0))

## There is an effect of cropsys
## on b and c, the slopes
anova(fmm2)

intervals(fmm2)
## These shows that the parameters are still well constrained

## This shows some outliers
## might need to dig deeper to see
## what is going on
plot(fmm2, id = 0.01)

## Residuals between -2 and 2 are reasonable and for a data set
## of this size a few ones outside are reasonable. If there is 
## more information availbale or a plausible hypothesis we
## could model the residual variance better

png("figs/augPred-fmm2.png", width = 1e3, height = 1e3)
plot(augPred(fmm2, level = 0:1))
dev.off()

## The previous plot shows a very good agreement between modeled and observed
## This is evidence that we should not be too worried about the outliers
## To make it clear, normally, I am very concerned about outliers and it is important
## to dig deeper, but in this case I'm using the plot to give me reassurance that this is not
## a big deal.

## Testing simulate_nlme
## This is for the MEAN function
if(packageVersion("nlraa") < '0.61') stop("need latest version of nlraa")

fmm2.sim1 <- simulate_nlme(fmm2, nsim = 100, psim = 1, level = 0)


# This has a good explanation of methods to do this:
# https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd452.htm
# I think transformation isn't a good option in our case bc
# we are very interested in the raw relationship
fmm3 <- update(fmm2, weights = varPower())

#--this shows the opposite - higher variance at low values
plot(fmm3)


leachG$mn.s <- apply(fmm2.sim1, 1, mean)
leachG$mxn.s <- apply(fmm2.sim1, 1, max)
leachG$mnn.s <- apply(fmm2.sim1, 1, min)


ggplot() + 
  geom_point(data = leachG, aes(x = n_rate, y = leaching_kgha)) + 
  geom_ribbon(data = leachG, 
              mapping = aes(x = n_rate, 
                            ymin = mxn.s, 
                            ymax = mnn.s, fill = cropsys), 
              alpha = 0.5) + 
  ggtitle("Simualtion at level 0")

## What if I try to simualte at level 1?
fmm2.sim11 <- simulate_nlme(fmm2, nsim = 100, psim = 1, level = 1)
## Need to process this matrix correctly still

## This is a better model, but...
fmm3 <- update(fmm2, random = list(site = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site/eu)

anova(fmm3)

intervals(fmm3)

## Fewer outliers, this is a better model
plot(fmm3, id = 0.001)

## Parameter values and contrast among groups
emmeans(fmm3, ~ cropsys, param = "a")
emmeans(fmm3, ~ cropsys, param = "b")
emmeans(fmm3, ~ cropsys, param = "xs")
emmeans(fmm3, ~ cropsys, param = "c")
## Contrasts
contrast(emmeans(fmm3, ~ cropsys, param = "a"), "pairwise")
contrast(emmeans(fmm3, ~ cropsys, param = "b"), "pairwise")
contrast(emmeans(fmm3, ~ cropsys, param = "xs"), "pairwise")
contrast(emmeans(fmm3, ~ cropsys, param = "c"), "pairwise")
=======

## This is not a perfect model by any means,
## but it is something to get started

leachG$prds <- predict(fmm3, level = 0)

ggplot(data = leachG, aes(x = n_rate, y = leaching_kgha)) + 
  geom_point() + 
  facet_wrap( ~ cropsys) + 
  geom_line(aes(y = prds))



# ok me -------------------------------------------------------------------

fit1 <- nls(leaching_kgha ~ SSplin(n_rate, a, xs, b), data = leach)

ggplot(data = leach, aes(x = n_rate, y = leaching_kgha)) +
  geom_point() + 
  geom_line(aes(y = fitted(fit1))) + 
  ggtitle("Plateua Linear")

# note: originally cropsys wasn't included in the eu definition. 
leach1 <- leach %>%
  mutate(eu = paste0(site,"_",year, "_", cropsys))

leachG <- groupedData(leaching_kgha ~ n_rate | eu, data = leach1)
leachG$cropsys <- as.factor(leachG$cropsys)

gnL <- nlsList(leaching_kgha ~ SSplin(n_rate, a, xs, b), data = leachG) 

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

gnm2 <- update(gnm, fixed = list(a + xs + b ~ cropsys),
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



