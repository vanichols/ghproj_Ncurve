# author: gina
# created; 7/2/2020
# purpose: have fitting occur in compact way
# last updated: 8/20/2020, running without sutherland

rm(list = ls())

# libraries ---------------------------------------------------------------

#remotes::install_github("femiguez/nlraa")
library(nlraa)
library(nlme)
library(dplyr) #--overwrites collapse from nlme
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(emmeans)
library(car) #--overwrites recode in dplyr
library(minpack.lm)
library(janitor)
library(broom)



# data prep ---------------------------------------------------------------

dat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                  "sc" = "cs"),
         rotation = as.factor(rotation)) %>%
  select(nrate_kgha, everything()) %>% 
  pivot_longer(annual_rain_mm:yield_maize_buac) %>% 
  select(-date, -doy)

leach <- 
  dat %>% 
  filter(name == "leaching_kgha") %>% 
  filter(crop == "corn")

leach1 <- 
  leach %>%
  mutate(eu = paste(site_id, year, rotation, sep = "_")) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

leachG <- groupedData(value ~ nrate_kgha | eu, data = leach1)
leachG$rotation <- as.factor(leachG$rotation)


# fit the model -----------------------------------------------------------

lmodG <- nlsList(value ~ SSblin(nrate_kgha, a, b, xs, c), data = leachG) 
lmod1 <- nlme(lmodG, random = pdDiag(a + b + c ~ 1)) #--add random effects
#--add fixed effect of rotation
fxf1 <- fixef(lmod1) 
lmod2 <- update(lmod1, 
               fixed = list(a + b + xs + c ~ rotation),
               start = c(fxf1[1], 0, #--a
                         fxf1[2], 0, #--b
                         fxf1[3], 0, #--xs
                         fxf1[4], 0)) #--c
fxf2 <- fixef(lmod2) #--now we have a value for cc and cs
#--add raneff for eu nested within site
lmod3a <- update(lmod2, 
                random = list(site_id = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c ~ 1)),
               groups = ~ site_id/eu)


# use model to make preds -------------------------------------------------

#-make predictions so I can make graphs

nrates <- seq(0, 300)

lmod3a

#--at level of eu
pred_dat <- 
  leachG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  mutate(preds = predict(lmod3a, newdata = .))

#--at level of eu
new_dat <- 
  leachG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) 

pred_dat1 <- predict(lmod3a, newdata = new_dat, level = 0:1)

leachG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  left_join(pred_dat1)

#--at level of crop rotation
pred_dat2 <- 
  leachG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  mutate(preds = predict(lmod3a, newdata = ., level = 0)) %>% 
  select(rotation, nrate_kgha, preds) %>% 
  distinct() %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                          "cc" = "Continuous Maize",                        
                          "cs" = "Rotated Maize"))

pred_dat %>% 
  filter(preds > 0) %>% 
  ggplot(aes(nrate_kgha, preds, group = eu)) + 
  geom_line(color = "gray80") + 
  geom_line(data = pred_dat2, 
            aes(nrate_kgha, preds, color = rot2, group = rot2),
            size = 2) + 
  scale_color_manual(values = c("Continuous Maize" = "darkblue",
                                "Rotated Maize" = "orange2")) +
  labs(color = NULL,
       y = "Nitrogen Leaching (kg ha-1)",
       x = "Fertilizer Applied (kg N ha-1)") +
  theme_bw() + 
  theme(legend.position = c(0.2,0.9),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.3)),
        legend.background = element_rect(color = "black"))

ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/manuscript/fig_leaching.png", width = 6)



#--scratch pad

newP <- data.frame(nrate_kgha = seq(0, 300))

predict(lmod3a, newP)



#--example from predict.nlme
fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),  data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1, ## <---grouping--->  Asym ~ 1 | Seed
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
fm1

age. <- seq(from = 2, to = 30, by = 2)
newLL.301 <- data.frame(age = age., Seed = 301)
newLL.329 <- data.frame(age = age., Seed = 329)
(p301 <- predict(fm1, newLL.301, level = 0:1))
(p329 <- predict(fm1, newLL.329, level = 0:1))
## Prediction are the same at level 0 :
all.equal(p301[,"predict.fixed"],
          p329[,"predict.fixed"])
