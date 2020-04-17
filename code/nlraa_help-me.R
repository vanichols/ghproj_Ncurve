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

leach1 <- leach %>%
  mutate(eu = paste0(site,"_",year))

leachG <- groupedData(leaching_kgha ~ n_rate | eu, data = leach1)
leachG$cropsys <- as.factor(leachG$cropsys)

fmL <- nlsList(leaching_kgha ~ SSexpf(n_rate, a, c), data = leachG) 

plot(fmL)
plot(intervals(fmL))
## Very strong heterogeneous variance and more variability in a than in c

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
fmm3 <- update(fmm2, weights = varPower())

plot(fmm3)

anova(fmm3)

## This is not a perfect model by any means,
## but it is something to get started
leachG$prds <- predict(fmm3, level = 0)

ggplot(data = leachG, aes(x = n_rate, y = leaching_kgha)) + 
  geom_point() + 
  facet_wrap( ~ cropsys) + 
  geom_line(aes(y = prds))
