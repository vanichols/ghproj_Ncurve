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
## One option, reasonable for the data is the exponential

fit0 <- nls(leaching_kgha ~ SSexpf(n_rate, a, c), data = leach)

## This seems like a reasonable model
ggplot(data = leach, aes(x = n_rate, y = leaching_kgha)) +
  geom_point() + 
  geom_line(aes(y = fitted(fit0))) + 
  ggtitle("Exponential fit. \n Do we need something more complex?")


# note: fernando didn't have cropsys in his eu def. keep it 'wrong' so I understand how he is interpreting things
leach1 <- leach %>%
  mutate(eu = paste0(site,"_",year))

leachG <- groupedData(leaching_kgha ~ n_rate | eu, data = leach1)
leachG$cropsys <- as.factor(leachG$cropsys)

fmL <- nlsList(leaching_kgha ~ SSexpf(n_rate, a, c), data = leachG) 

plot(fmL)
plot(intervals(fmL))
## Very strong heterogeneous variance and more variability in a than in c
# a is the value at x = 0, c is exponential rate

# so he lets a be a random factor (?)
fmm <- nlme(fmL, random = pdDiag(a ~ 1))

fmm
## Not a lot of random variability 

## Need to account for the increasing variance
plot(fmm)

fxf <- fixef(fmm)

fmm2 <- update(fmm, fixed = list(a + c ~ cropsys),
               random = a ~ 1,
               start = c(fxf[1], 0, fxf[2], 0))

## There is a strong effect of cropping system on 
## leaching
anova(fmm2)

## This shows some outliers
plot(fmm2, id = 0.01)

## Trying to account for the increasing variance
# Is he basing that off of THIS plot? Or the fmm plot?

# This has a good explanation of methods to do this:
# https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd452.htm
# I think transformation isn't a good option in our case bc
# we are very interested in the raw relationship
fmm3 <- update(fmm2, weights = varPower())

#--this shows the opposite - higher variance at low values
plot(fmm3)

anova(fmm3)


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


