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

# fernando's data prep ----------------------------------------------------

#--note there are 3 blocks
sm %>% 
  ggplot(aes(DOY, Yield)) + 
  geom_point(aes(shape = Crop, color = Crop)) + 
  facet_grid(.~Input)

#--create an 'eu' that uniquely defines each 'curve'
sm$eu <- with(sm, factor(Block):factor(Input):factor(Crop))

#--get rid of the 0 yield at the day of planting
sm2 <- subset(sm, DOY != 141)

# The next step is to create the groupedData which is a convenient structre to be used throughout the fitting process in nlme.

#--it seems like this is saying 'there is a curve of yield vs doy for each eu'
smG <- groupedData(Yield ~ DOY | eu, data = sm2)


# my data prep ------------------------------------------------------------

#--my data
leach <- read_csv("data/tidy/td_crop-by-year.csv") %>% 
  select(site, year, cropsys, crop, leaching_kgha, n_rate) %>% 
  arrange(site, year, n_rate) %>% 
  filter(crop == 'corn') 

#--there is still something wrong with gentry  
#--remove it for now, create the eu
leach2 <- leach %>% 
  filter(site != "gentry") %>% 
  mutate(eu = paste(cropsys, site, year, sep = "_"))

#--hmm, let's just do the CC cropsys for now
leach3 <- 
  leach2 %>% 
  filter(cropsys == "cc") %>% 
  mutate(eu = paste(site, year, sep = "_"))

leach3

leachG <- groupedData(leaching_kgha ~ n_rate |eu, data = leach3)


# just fitting a curve to each eu using nls -------------------------------

# Use a beta growth function via SSbgf
#fit.lis <- nlsList(Yield ~ SSbgf(DOY, w.max, t.e, t.m), data = smG)
#ummmm, he says use SSbrgrp intead. He says it just behaves better.
fit.lis <- nlsList(Yield ~ SSbgrp(DOY, w.max, t.e, t.m), data = smG)
plot(fit.lis)
plot(augPred(fit.lis, level = 0:1))

#--mine is also getting an error right now, don't worry about it. 
fit.lisL <- nlsList(leaching_kgha ~ SSplin(n_rate, a, xs, b), data = leachG)
plot(fit.lisL)
plot(augPred(fit.lisL, level = 0:1))

#--which ones didn't converge?
leach3 %>% 
  filter(eu %in% c("kladivko_2000", "kladivko_1999", "sutherland_2012")) %>% 
  ggplot(aes(n_rate, leaching_kgha)) +
  geom_point() + 
  geom_line() + 
  facet_grid(.~eu)


# use nlme, which accepts fixed and random things -------------------------

#--relax the convergence criteria (?)
#--so right now it is just fitting one mack daddy model, right?
fit.me <- nlme(fit.lis, control = list(maxIter = 100, msMaxiter = 300, pnlsMaxIter = 20))
plot(fit.me)
plot(augPred(fit.me, level = 0:1))

#--a curve that describes 'everything'
fit.meL <- nlme(fit.lisL, control = list(maxIter = 100, msMaxiter = 300, pnlsMaxIter = 20))
plot(fit.meL)
plot(augPred(fit.meL, level = 0:1))
#--converged fine



#--I guess there might be something better, bgf2, which doesn't have a self-start
#--we specify w.b as 0 (the initial biomass) and t.b as 141 bc we know when we planted
fit.lis2 <- nlsList(Yield ~ bgf2(DOY, w.max, w.b = 0, t.e, t.m, t.b = 141),
                    data = smG,
                    start = c(w.max = 30, t.e=280, t.m=240))
plot(fit.lis2)

#--ok now fit the mackdaddy model to that new nls object
fit.me2 <- nlme(fit.lis2)
plot(augPred(fit.me2, level = 0:1))

#--update the variance-covariance matrix to be simplified (?)
#--so we say w.max and t.e and t.m variation aren't related. I guess. SImpler, converges,so let's role with that. (?)
#--I don't get this, I need to read about nlme
fit2.me2 <- update(fit.me2, random = pdDiag(w.max + t.e + t.m ~ 1))
anova(fit.me2, fit2.me2)


#--this didn't fit for me. I don't know why. 
fit2.me2 <- update(fit.meL, random = pdDiag(a + xs + b ~ 1))
# no idea what this error is

################ STOPPED, need help. Nothing works after this. 


fe <- fixef(fit2.me2) ## Some starting values with visual help
feL <- fixef(fit.meL)

# So we say these things vary by crop ( a fixed effect)
# we have to give it 6 starting values bc we have a w.max, t.e, and t.m parameter for each crop (3 crops)
fit3.me2 <- update(fit2.me2, fixed = list(w.max + t.e + t.m ~ Crop),
                  start = c(fe[1], -10, 20, fe[2], -40, 0, fe[3], -40, 0))

#--ok here I am struggling.......
fit3.meL <- update(fit.meL, fixed = list(a + xs + b ~ site))

## We next include the Input
fe2 <- fixef(fit3.me2)
fit4.me2 <- update(fit3.me2, fixed = list(w.max + t.e + t.m
                               ~ Crop + Input),
                  start = c(fe2[1:3], 0, fe2[4:6], 0, fe2[7:9], 0))
## and the interaction
fe3 <- fixef(fit4.me2)
fit5.me2 <- update(fit4.me2,
                   fixed = list(w.max + t.e + t.m
                     ~ Crop + Input + Crop:Input),
                  start = c(fe3[1:4], 0, 0,
                            fe3[5:8], 0, 0,
                            fe3[9:12], 0, 0))

plot(fit5.me2)
#-unequal variance....
#--so let's let the variance depend on the crop
fit6.me2 <- update(fit5.me2,
                   weights = varPower(form = ~ fitted(.) | Crop))

fit7.me2 <- update(fit6.me2, weights = varPower(form = ~ fitted(.)))

anova(fit6.me2, fit7.me2)

#--model 6 is the winner
fit6.me2

# the random effcts are very small compared to the residual. Maybe we don't need them. 

## Random effects are almost zero, use gnls (specifically written for models without random effects)
fit8.me2 <- gnls(Yield ~ bgf2(DOY, w.max, t.e, t.m, w.b=0, t.b=141),
                 data = smG,
                 params = list(w.max + t.e + t.m ~ Crop + Input
                                                   + Crop:Input),
                 weights = varPower(form = ~ fitted(.) | Crop),
                 start = fixef(fit7.me2))
anova(fit6.me2, fit8.me2)

#--the model without the random effect is better
anova(fit8.me2)

# this shows that Crop, Input, and thier interaction are sig for all terms except the t.m param

# how do resids look?
## Random effects are almost zero
plot(fit8.me2)

#--wow resids look great.
# let's look at the predictions
smG$prds <- fitted(fit8.me2)

doys <- 168:303
ndat <- expand.grid(DOY=doys, Crop= unique(smG$Crop), Input=c(1,2))
ndat$preds <- predict(fit8.me2, newdata = ndat)

ndat2 <- ndat
ndat2[ndat2$Crop == "M" & ndat2$DOY > 270,"preds"] <- NA
ndat2 <- na.omit(ndat2)

ndat2 %>% 
  ggplot(aes(DOY, preds)) + 
  geom_line(aes(color = Crop)) + 
  facet_grid(.~Input)
