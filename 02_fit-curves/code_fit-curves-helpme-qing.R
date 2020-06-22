# author: gina
# created; 6/22/2020
# purpose: get help from qing
# last updated: 


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



# overview ----------------------------------------------------------------

# The 'experiment' is measuring nitrate leaching at several nitrogen fertilization rates in 
#  corn grown in 2 rotations (corn-corn, corn-soybean)
# The experiment was done at 9 sites.
# At each site it was done for 19 years.
# Years are essentially the 'replicates'


# I am interested in the nitrate leaching vs nitrogen fertilization rate curve
# I want to know how important the 'site' is and 'rotation' in determining the params of the curve


# data prep ------------------------------------------------------------

dat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                 "sc" = "cs"))

leach <- dat %>% 
  select(site_id, year, yearF, rotation, leaching_kgha, nrate_kgha) 
  
#--a glimpse of the data
ggplot(data = leach, aes(x = nrate_kgha, y = leaching_kgha)) + 
  facet_wrap(~ rotation) + 
  geom_jitter()

## Here an important decision is what nonlinear model to use
## Possible options are:
## 1. SSexpf
## 2. SSblin
## 3. SSexplin

fm1 <- nls(leaching_kgha ~ SSexpf(nrate_kgha, a, c), data = leach)
fm2 <- nls(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c), data = leach)
fm3 <- nlsLM(leaching_kgha ~ SSexplin(nrate_kgha, cm, rm, tb), data = leach)

anova(fm1, fm2, fm3)
anova(fm2, fm3)

## while the explin actually 'fits' better, the residuals show it is a bad model for this data.
## blin is the winner, as it also makes more biological sense 


# bilinear fit ------------------------------------------------------------

leach1 <- 
  leach %>%
  mutate(eu = paste0(site_id,"_", year, rotation)) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

leachG <- groupedData(leaching_kgha ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)

#--fit model to each group
fmG <- nlsList(leaching_kgha ~ SSblin(nrate_kgha, a, b, xs, c), data = leachG) #--non-converg doesn't matter
plot(intervals(fmG))

#--a, b, and c could have random components added
fmm1 <- nlme(fmG, random = pdDiag(a + b + c ~ 1))
plot(fmm1)
plot(fmm1, id = 0.000001) #--fernando likes things to be under 2
plot(fmm1, id = 0.01) #--eek. 


#--add fixed effect of rotation
fxf1 <- fixef(fmm1) 
fmm2 <- update(fmm1, 
               fixed = list(a + b + xs + c ~ rotation),
               start = c(fxf1[1], 0, #--a
                         fxf1[2], 0, #--b
                         fxf1[3], 0, #--xs
                         fxf1[4], 0)) #--c

fxf2 <- fixef(fmm2) #--now we have a value for cc and cs


#--could try having the variance increase with higher values
fmm3 <- update(fmm2, weights = varPower())
plot(fmm3, id = 0.01)

#--compare them, this code is not working
par(mfrow=c(2,1))
plot(fmm3, id = 0.01, main="varPower")
plot(fmm2, id = 0.01, main="no variance modelling")

#--instead of modelling the variance, try adding two random effect levels
# I don't quite get this code notation, someone wrote it for me 
fmm3a <- update(fmm2, random = list(site_id = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site_id/eu)

## Fewer outliers, this is a better model
plot(fmm3a, id = 0.001)


# explore model -----------------------------------------------------------

#--what is this telling me?
intervals(fmm3a) 

#--interclass correlation
VarCorr(fmm3a)

#--I'm not sure the proper way to calcuate this
tibble(aparam = c("a", "b", "c"),
       site_var = as.numeric(VarCorr(fmm3a)[2:4]),
       eu_var = as.numeric(VarCorr(fmm3a)[6:8]),
       res_var = as.numeric(VarCorr(fmm3a)[9])) %>% 
  mutate(tot = site_var + eu_var + res_var,#--where should I add the residual?
         pct_site = site_var/tot,
         pct_eu = eu_var/tot)

#--can I interpret this as site is a very important consideration for the parameter a?


# what about covariates in non-linear models? -----------------------------

#--leaching increases with increasing rain amount. 
#--I'd like to correct for that. 

dat %>% 
  ggplot(aes(annual_rain_mm, leaching_kgha, color = site_id)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~nrate_kgha, scales = "free") + 
  labs(title = "Leaching inc. with more annual rain at every Nrate")

# how do I include a covariate in a non-linear model? I think only b and c parms would be affected. 

# The other thing is that is site is important, I'd want to explore what 
# ABOUT a site is important. 

# I think the ksat of the soil is important in determining a parm (intercept). ksat is a continuous variable, is there a way to test this hypothesis? I took the fitted parms from each eu, looked at the relationship between the intercept and ksat, and it has a positive correlation. This seems inelegant and probably wrong though. 


# model exploration -------------------------------------------------------


## Parameter values and contrast among groups
emmeans(fmm3a, ~ rotation, param = "a")
emmeans(fmm3a, ~ rotation, param = "b")
emmeans(fmm3a, ~ rotation, param = "xs")
emmeans(fmm3a, ~ rotation, param = "c")
## Contrasts
contrast(emmeans(fmm3a, ~ rotation, param = "a"), "pairwise")
#--a doesn't vary by rotation. We saw above it is more depenedent upon the site?
#--all of these do depend on rotation: 
contrast(emmeans(fmm3a, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(fmm3a, ~ rotation, param = "xs"), "pairwise")
contrast(emmeans(fmm3a, ~ rotation, param = "c"), "pairwise")

# Question:

leach
