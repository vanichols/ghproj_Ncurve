# author: gina
# created; 4/10/2020
# purpose: work through FEM example w/my data
# last updated: 4/16/2020


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

leach1 <- leach %>%
  mutate(eu = paste0(cropsys,"_",site,"_",year)) %>%
  mutate(site = as.factor(site))

leachG <- groupedData(leaching_kgha ~ n_rate | eu, data = leach1)
leachG$cropsys <- as.factor(leachG$cropsys)

fmL <- nlsList(leaching_kgha ~ SSblin(n_rate, a, b, xs, c), data = leachG) 

plot(fmL)
plot(intervals(fmL))
## The residual variance is mostly ok, but there are some 
## significant outliers, not that many though

## I had to eliminate xs here for this to converge
fmm <- nlme(fmL, random = pdDiag(a + b + c ~ 1))

fmm

## Some outliers, mostly from gold
plot(fmm, id = 0.01)

fxf <- fixef(fmm)

fmm2 <- update(fmm, fixed = list(a + b + xs + c ~ cropsys),
               start = c(fxf[1], 0, fxf[2], 0, fxf[3], 0, fxf[4], 0))

## There is an effect of cropsys
## on b and c, the slopes
anova(fmm2)

## This shows some outliers
## might need to dig deeper to see
## what is going on
plot(fmm2, id = 0.01)

## Testing simulate_nlme
## This is for the MEAN function
if(packageVersion("nlraa") < '0.59') stop("need latest version of nlraa")

fmm2.sim1 <- simulate_nlme(fmm2, nsim = 100, psim = 1, level = 0)

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
