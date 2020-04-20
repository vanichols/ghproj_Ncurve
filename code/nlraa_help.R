# actual paper example ----------------------------------------------------
#https://cran.r-project.org/web/packages/nlraa/vignettes/nlraa-AgronJ-paper.html
library(nlraa)
library(nlme)
library(tidyverse)

#--note there are 3 blocks
sm %>% 
  ggplot(aes(DOY, Yield)) + 
  geom_point(aes(shape = Crop, color = Crop)) + 
  facet_grid(.~Input)

#--create an 'eu' that uniquely defines each 'curve'
sm$eu <- with(sm, factor(Block):factor(Input):factor(Crop))
#--get rid of the 0 yield at the day of planting
sm2 <- subset(sm, DOY != 141)
#The next step is to create the groupedData which is a convenient structre to be used throughout the fitting process in nlme.

#--it seems like this is saying 'there is a curve of yield vs doy for each eu
smG <- groupedData(Yield ~ DOY | eu, data = sm2)


# Use a beta growth function via SSbgf
fit.lis <- nlsList(Yield ~ SSbgf(DOY, w.max, t.e, t.m), data = smG)

#ummmm, he says use SSbrgrp intead. He says it just behaves better.
fit.lis <- nlsList(Yield ~ SSbgrp(DOY, w.max, t.e, t.m), data = smG)
plot(fit.lis)


#--only 20 of the 24 possible fits got convergence

#--relax the convergence criteria (?)
fit.me <- nlme(fit.lis, control = list(maxIter = 100, msMaxiter = 300, pnlsMaxIter = 20))
 #### QUESTION. How does it know what I want as random?

#--look at resids and preds
plot(fit.me)
plot(augPred(fit.me, level = 0:1))


#--I guess there might be something better, bgf2, which doesn't have a self-start
#--we specify w.b as 0 (the initial biomass) and t.b as 141 bc we know when we planted
fit.lis2 <- nlsList(Yield ~ bgf2(DOY, w.max, w.b = 0, t.e, t.m, t.b = 141),
                    data = smG,
                    start = c(w.max = 30, t.e=280, t.m=240))
plot(fit.lis2)

#--ok now fit the non-linear mixed model
# how does it know what is random? It just assumes everything? How does it know about the eus? From the grouped command?
fit.me2 <- nlme(fit.lis2)
fit.me2

#--let's try this whole thing using sm2, not the grouped one
# can't use nlsList if it isn't grouped
fit.lis2noG <- nls(Yield ~ bgf2(DOY, w.max, w.b = 0, t.e, t.m, t.b = 141),
                    data = sm2,
                    start = c(w.max = 30, t.e=280, t.m=240))

fit.me2 <- nlme(fit.lis2noG)

#--wait we want to updat the variance-covariance matrix to be simplified
# I guess the default is pdSymm. 
# which is a "general positive-definite matrix"
# not sure what that means. 
# a pdDiag has 0s for the covariances, but allows for diff var for each random effect.
# I'm not sure about the notaiton. Why the ~1?

fit2.me2 <- update(fit.me2, random = pdDiag(w.max + t.e + t.m ~ 1))
fit2.me2 #--notice there are no correlations for the random effects now

anova(fit.me2, fit2.me2)

#--so we said w.max and t.e and t.m aren't related. I guess. SImpler, converges,so let's role with that. (?)


fe <- fixef(fit2.me2) ## Some starting values with visual help
# So we say these things vary by crop ( a fixed effect)
# we have to give it 6 starting values bc we have a w.max, t.e, and t.m parameter for each crop (3 crops)

fit3.me2 <- update(fit2.me2, fixed = list(w.max + t.e + t.m ~ Crop),
                  start = c(fe[1], -10, 20, #--these are the w.max for each crop 
                            fe[2], -40, 0, #--the t.e for each crop
                            fe[3], -40, 0)) #--the t.m for each crop

## We next include the Input
fe2 <- fixef(fit3.me2)
fit4.me2 <- update(fit3.me2, fixed = list(w.max + t.e + t.m
                               ~ Crop + Input),
                  start = c(fe2[1:3], 0, # the w.maxes
                            fe2[4:6], 0, # the t.e.s
                            fe2[7:9], 0)) # the t.ms

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

#--or not.
fit7.me2 <- update(fit6.me2, weights = varPower(form = ~ fitted(.)))

#--this makes no sense. It's still testing model 1 vs 2, but model 1 and 2 changed.
anova(fit7.me2, fit6.me2)
anova(fit6.me2, fit7.me2)

#--model 6 is the winner (?) based on what?
fit6.me2

# the random effcts are very small compared to the residual. Maybe we don't need them. 

## Random effects are almost zero, use gnls (specifically written for models without random effects)
fit8.me2 <- gnls(Yield ~ bgf2(DOY, w.max, t.e, t.m, w.b=0, t.b=141),
                 data = smG,
                 params = list(w.max + t.e + t.m ~ Crop + Input
                                                   + Crop:Input),
                 weights = varPower(form = ~ fitted(.) | Crop),
                 start = fixef(fit7.me2))


anova(fit8.me2, fit6.me2)

anova(fit6.me2, fit8.me2)
#--the model without the random effect is better (lower AIC/BIC etc)
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
