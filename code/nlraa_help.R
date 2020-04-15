library(nlme)
data(barley)
barley$yearf <- as.factor(barley$year)
barleyG <- groupedData(yield ~ NF | yearf, data = barley)
#First step is to create the grouped data object. Then fit the asymptotic regression to each year and the mixed model.

## Fit the nonlinear model for each year
fit.nlis <- nlsList(yield ~ SSasymp(NF, Asym, R0, lrc), data = barleyG)
## Use this to fit a nonlinear mixed model
fit.nlme <- nlme(fit.nlis)
## Investigate residuals
plot(fit.nlme)

## Look at predictions
plot(augPred(fit.nlme, level = 0:1))


dat$sitef <- as.factor(dat$site)
datG <- groupedData(leach ~ nrate | sitef, data = dat)
#First step is to create the grouped data object. Then fit the asymptotic regression to each year and the mixed model.



## Fit the nonlinear model for each year
#fit.nlis <- nlsList(yield ~ SSasymp(NF, Asym, R0, lrc), data = barleyG)

fit1 <- nlsList(leach ~ SSplin(nrate, a, xs, b), data = datG)
summary(fit1) %>% tidy()

## Use this to fit a nonlinear mixed model
fit.nlme <- nlme(fit.nlis)
## Investigate residuals
plot(fit.nlme)

## Look at predictions
plot(augPred(fit.nlme, level = 0:1))


# actual paper example ----------------------------------------------------
#https://cran.r-project.org/web/packages/nlraa/vignettes/nlraa-AgronJ-paper.html

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
