# author: gina
# created; 4/10/2020
# purpose: work through FEM example w/my data
# last updated: 4/16/2020
#               4/17/2020 follow FEM new personalized example
#               4/20/2020 new following w/updated data
#               6/11/2020 new data again


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

yield <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  select(site_id, year, rotation, yield_maize_buac, nrate_kgha) %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                 "sc" = "cs"))

# look at it
ggplot(data = yield, aes(x = nrate_kgha, y = yield_maize_buac, color = yearF)) + 
  facet_wrap(~ rotation) + 
  geom_point() + 
  geom_line(aes(group = interaction(year, site_id)))

## Possible options are:
## 1. SSblin
## 2. Quadratic Plateau
## 3. ?

fmbl <- nls(yield_maize_buac ~ SSblin(nrate_kgha, a, b, xs, c), data = yield)
fmqp <- nls(yield_maize_buac ~ SSquadp(nrate_kgha, a, b, c, xs), data = yield)

anova(fmbl, fmqp)
#anova(fm2, fm3)

ggplot(data = yield, aes(x = nrate_kgha, y = yield_maize_buac)) +
  geom_point() + 
  geom_line(aes(y = fitted(fmbl)), color = "red") +
  geom_line(aes(y = fitted(fmqp)), color = "blue") +
  ggtitle("red = Bilinear \n blue = quadplat")

# bilinear fit ------------------------------------------------------------

yield1 <- 
  yield %>%
  mutate(eu = paste0(site_id,"_",year, rotation)) %>%
  mutate(site_id = as.factor(site_id))

yieldG <- groupedData(yield_maize_buac ~ nrate_kgha | eu, data = yield1)
yieldG$rotation <- as.factor(yieldG$rotation)

fmL <- nlsList(yield_maize_buac ~ SSblin(nrate_kgha, a, b, xs, c), data = yieldG) 
plot(intervals(fmL))

# allow somethings to vary, xs will be by rotation
fmL2 <- nlme(fmL, random = pdDiag(a + b ~ 1))
plot(fmL2)
plot(fmL2, id = 0.000001) #--fernando likes things to be under 2

plot(fmL, id = 0.01) #--huggins and hoffman, bah
coef(fmL)
## these plots are always VERY informative. plot(fmL) shows the residuals
## from individually fitting a nonlinear model to each 'experimental unit'
## the intervals plot shows that 'c' is varying systematically for different 
## experimental units. 'xs' can be hard to esimate because some units have 
## very large confidence intervals. 'a' and 'b' are likely to vary according to
## factors in the 'design' of this experiment

## The residual variance is mostly ok, but there are some 
## significant outliers, not that many though

fmm <- nlme(fmL, random = pdDiag(a + b + c ~ 1))
# GN - So w/o a fixed effect defined, it's fitting a grand model, allowing a b and c to vary for each eu? 
# FEM - yes, the statement above is correct

fmm
## this model looks fine
intervals(fmm)
## the random effect intervals seem to be well constrained

## Some outliers?
plot(fmm, id = 0.01)

fxf <- fixef(fmm)

## Incorporating the effect of rotation is a bit tricky, 
## We need the parameter estimates from the previous model
## as starting values and we put zeros for the starting value
## of the 'treatment effect'
fmm2 <- update(fmm, fixed = list(a + b + xs + c ~ rotation),
               start = c(fxf[1], 0, #--a
                         fxf[2], 0, #--b
                         fxf[3], 0, #--xs
                         fxf[4], 0)) #--c

fxf2 <- fixef(fmm2)

# What if I wanted to include site (there are 10)?

## I'm guessing this does not work well, there are better ways of improving
## the model...
fmm2up <- update(fmm2, fixed = list(a + b + xs + c ~ rotation + site_id),
                 start = c(fxf2[1:2], rep(1, 9), #--a
                           fxf2[3:4], rep(0, 9), #--b
                           fxf2[5:6], rep(120, 9), #--xs
                           fxf2[7:8], rep(0, 9))) #--c



# Gina trying to reverse engineer formula notation ------------------------

## GN What would I do if I wanted to fit a model where xs doesn't vary by rotation. I'll try below.

fmm2a <- update(fmm, fixed = list(a + b + c ~ rotation, xs ~ 1),
                start = c(fxf[1], 0, fxf[3], 0, fxf[4], 0, 
                          fxf[2]))

#--bah, no. 
#--nlme example
exfm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
              data = Loblolly,
              fixed = Asym + R0 + lrc ~ 1,
              random = Asym ~ 1,
              start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
summary(exfm1)
exfm2 <- update(exfm1, random = pdDiag(Asym + lrc ~ 1))
summary(fm2)
exfm3 <- update(exfm1, random = pdSymm(Asym + lrc ~ 1)) 
summary(exfm3)
anova(exfm2, exfm3)
#--why does the one w/pdSymm have more df? It has more things to estimate, doesn't it? pdDiag assigns covariances as 0. 

# resume w/fernando -------------------------------------------------------

## There is an effect of rotation
## on b and c, the slopes -- everything?
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

fmm3 <- update(fmm2, weights = varPower())

#--this shows the opposite - higher variance at low values
plot(fmm3)


yieldG$mn.s <- apply(fmm2.sim1, 1, mean)
yieldG$mxn.s <- apply(fmm2.sim1, 1, max)
yieldG$mnn.s <- apply(fmm2.sim1, 1, min)


ggplot() + 
  geom_point(data = yieldG, aes(x = nrate_kgha, y = yield_maize_buac)) + 
  geom_ribbon(data = yieldG, 
              mapping = aes(x = nrate_kgha, 
                            ymin = mxn.s, 
                            ymax = mnn.s, fill = rotation), 
              alpha = 0.5) + 
  ggtitle("Simualtion at level 0")

## What if I try to simualte at level 1?
fmm2.sim11 <- simulate_nlme(fmm2, nsim = 100, psim = 1, level = 1)
## Need to process this matrix correctly still

yieldG$mn.s11 <- apply(fmm2.sim11, 1, mean)
yieldG$mxn.s11 <- apply(fmm2.sim11, 1, max)
yieldG$mnn.s11 <- apply(fmm2.sim11, 1, min)



fmm3 <- update(fmm2, random = list(site_id = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site_id/eu)

#--would it make sense to have year nested in site_id? And let things vary by year?
fmm3a <- update(fmm2, random = list(site_id = pdDiag(a + b + c ~ 1),
                                    year.f = pdDiag(a + b + c ~ 1)),
                groups = ~ site_id/year.f)


ggplot() + 
  geom_point(data = yieldG, aes(x = nrate_kgha, y = yield_maize_buac)) + 
  geom_ribbon(data = yieldG, 
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
emmeans(fmm3, ~ rotation, param = "a")
emmeans(fmm3, ~ rotation, param = "b")
emmeans(fmm3, ~ rotation, param = "xs")
emmeans(fmm3, param = "xs", specs)
emmeans(fmm3, ~ rotation, param = "c")
## Contrasts
contrast(emmeans(fmm3, ~ rotation, param = "a"), "pairwise")
contrast(emmeans(fmm3, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(fmm3, ~ rotation, param = "xs"), "pairwise")
contrast(emmeans(fmm3, ~ rotation, param = "c"), "pairwise")


## This is not a perfect model by any means,
## but it is something to get started

yieldG$prds <- predict(fmm3, level = 0)

ggplot(data = yieldG, aes(x = nrate_kgha, y = yield_maize_buac)) + 
  geom_jitter() + 
  facet_wrap( ~ rotation) + 
  geom_line(aes(y = prds), size = 2, color = "red")



# ok me -------------------------------------------------------------------

fit1 <- nls(yield_maize_buac ~ SSplin(nrate_kgha, a, xs, b), data = yield)

ggplot(data = yield, aes(x = nrate_kgha, y = yield_maize_buac)) +
  geom_point() + 
  geom_line(aes(y = fitted(fit1))) + 
  ggtitle("Plateua Linear")

# note: originally rotation wasn't included in the eu definition. 
yield1 <- yield %>%
  mutate(eu = paste0(site_id,"_",year, "_", rotation))

yieldG <- groupedData(yield_maize_buac ~ nrate_kgha | eu, data = yield1)
yieldG$rotation <- as.factor(yieldG$rotation)

gnL <- nlsList(yield_maize_buac ~ SSplin(nrate_kgha, a, xs, b), data = yieldG) 

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
# yieldG$resids <- resid(gnm, type = "p")
# yieldG$fits <- fitted(gnm)
# 
# gnp1 <- 
#   yieldG %>% 
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
## all aspects of yielding
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



