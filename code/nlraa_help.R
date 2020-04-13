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
