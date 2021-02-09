# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)
#               6/17/2020 look at soils data
#               8/20/2020 (removed sutherland, checking)
#               9/14/2020 (make it look nice)
#               10/9/2020 (this won't work any more, no soybean...)

library(tidyverse)
library(scales)
library(ggridges)
library(ggpubr)



# plot things -------------------------------------------------------------

clr1 <- "orange2"
clr2 <- "darkblue"
clr3 <- "#00BA38"

show_col(clr3)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  mutate(rotation2 = ifelse(rotation == "sc", "cs", rotation),
         croprot = paste0(crop, rotation2)) %>% 
  select(croprot, everything())

#--original

rawdat %>% 
  arrange(nrate_kgha) %>% 
  mutate(
    nrateF = paste(nrate_kgha, "kg N/ha"),
    nrateF = fct_inorder(nrateF),
    nrateF2 = factor(nrateF, levels = rev(levels(nrateF))),
    croprotnice = dplyr::recode(croprot,
                                corncc = "Continuous Maize",
                                corncs = "Rotated Maize",
                                soycs = "Rotated Soybean")) %>%
  group_by(nrateF2, croprotnice) %>% 
  mutate(mnleach = mean(leaching_kgha)) %>% 
  ggplot(aes(x = leaching_kgha, y = nrateF2, fill = croprotnice)) +
  geom_density_ridges(alpha = 0.5) +
  geom_point(aes(x = mnleach, y = nrateF2), pch = 22, size = 3) +
  scale_fill_manual(values = c(clr1, clr2, clr3)) + 
  labs(y = "Nitrogen Fertilizer Rate\n",
       x = expression(Nitrogen~Leaching~(kg~N~ha^-1)),
       fill = NULL) + 
  theme_bw() + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.text.y = element_text(vjust = -1),
        axis.title = element_text(size = rel(1.3)),
        legend.justification = c(1, 1),
        legend.position = c(0.95,0.99),
        legend.background = element_blank(),
        legend.text = element_text(size = rel(1.2)))

ggsave("fig3.png")

#--move labels
nlabs <- 
  rawdat %>% 
  arrange(nrate_kgha) %>% 
  mutate(
    nrateF = paste(nrate_kgha, "kg N/ha"),
    nrateF = fct_inorder(nrateF),
    nrateF2 = factor(nrateF, levels = rev(levels(nrateF))),
    croprotnice = dplyr::recode(croprot,
                                corncc = "Continuous Maize",
                                corncs = "Rotated Maize",
                                soycs = "Rotated Soybean")) %>% 
  filter(croprotnice == "Rotated Maize") %>% 
  select(nrate_kgha, croprotnice, nrateF, nrateF2) %>% 
  distinct()

rawdat %>% 
  arrange(nrate_kgha) %>% 
  mutate(
    nrateF = paste(nrate_kgha, "kg N/ha"),
    nrateF = fct_inorder(nrateF),
    nrateF2 = factor(nrateF, levels = rev(levels(nrateF))),
    croprotnice = dplyr::recode(croprot,
                                     corncc = "Continuous Maize",
                                     corncs = "Rotated Maize",
                                     soycs = "Rotated Soybean")) %>%
  group_by(nrateF2, croprotnice) %>% 
  mutate(mnleach = mean(leaching_kgha)) %>% 
  ggplot(aes(x = leaching_kgha, y = nrateF2, fill = croprotnice)) +
  geom_density_ridges(alpha = 0.5) +
  geom_point(aes(x = mnleach, y = nrateF2), pch = 22, size = 3) +
  geom_label(data = nlabs, aes(x = 175, y = nrateF2, label = paste(nrate_kgha, "kg N/ha Fertilizer"),
                               vjust = -1, hjust = 0),
             fill = "white") +
  # geom_label(data = nlabs, aes(x = -50, y = nrateF2, label = paste(nrate_kgha, "kg N/ha Fertilizer"),
  #                              vjust = -2, hjust = 0),
  #            fill = "white") +
  scale_fill_manual(values = c(clr1, clr2, clr3)) + 
  labs(y = "Data Density",
       x = expression(Nitrogen~Leaching~(kg~N~ha^-1)),
       fill = NULL) + 
  theme_bw() + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.text.y = element_blank(),
        axis.title = element_text(size = rel(1.3)),
        legend.justification = c(1, 1),
        legend.position = c(0.5,0.99),
        legend.background = element_blank(),
        legend.text = element_text(size = rel(1.2)))

ggsave("fig3_stacked.png")


# looks dumb --------------------------------------------------------------


fig_dat <- 
  rawdat %>% 
  mutate(nrateF = as.factor(nrate_kgha),
         nrateF2 = factor(nrateF, levels = rev(levels(nrateF))),
         croprotnice = dplyr::recode(croprot,
                                     corncc = "Continuous Maize",
                                     corncs = "Rotated Maize",
                                     soycs = "Rotated Soybean")) %>%
  group_by(nrateF2, croprotnice) %>% 
  mutate(mnleach = mean(leaching_kgha)) %>% 
  select(site_id, croprot, croprotnice, nrate_kgha, nrateF, nrateF2, leaching_kgha, mnleach)


fig_dat %>%
  arrange(nrate_kgha) %>% 
  mutate(striplab = paste(nrate_kgha, "kg N/ha"),
         striplab = fct_inorder(striplab),
         striplab2 = fct_rev(striplab)) %>% 
  ggplot(aes(leaching_kgha, fill = croprotnice)) +
  geom_density_line(alpha = 0.5) +
  geom_point(aes(x = mnleach, y = 0), pch = 22, size = 3) +
  scale_fill_manual(values = c(clr1, clr2, clr3)) + 
  labs(y = "Data Density",
       x = expression(Nitrogen~Leaching~(kg~N~ha^-1)),
       fill = NULL) +
  theme_bw() +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_blank(),
        axis.title = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(1.3)),
        #legend.justification = c(1, 1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.text = element_text(size = rel(1.2))) +
  facet_wrap(~striplab2, ncol = 1, strip.position = "top") + 
  coord_cartesian(xlim = c(0, 250))

ggsave("fig3_dumb.png")


fig_dat %>%
  filter(nrate_kgha != 300) %>% 
  arrange(nrate_kgha) %>% 
  mutate(striplab = paste(nrate_kgha, "kg N/ha"),
         striplab = fct_inorder(striplab),
         striplab2 = fct_rev(striplab)) %>% 
  ggplot(aes(leaching_kgha, fill = croprotnice)) +
  geom_density_line(alpha = 0.5) +
  geom_point(aes(x = mnleach, y = 0), pch = 22, size = 3) +
  scale_fill_manual(values = c(clr1, clr2, clr3)) + 
  labs(y = "Data Density",
       x = expression(Nitrogen~Leaching~(kg~N~ha^-1)),
       fill = NULL) +
  theme_bw() +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_blank(),
        axis.title = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(1.3)),
        #legend.justification = c(0.2, 0.8),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.text = element_text(size = rel(1.2))) +
  facet_wrap(~striplab2, ncol = 2, strip.position = "top") + 
  coord_cartesian(xlim = c(0, 250))

ggsave("fig3_panels.png")


quantile(rawdat$leaching_kgha, 0.1
         )

#--rafa's idea
rawdat %>%
  select(croprot, site_id, nrate_kgha, year, leaching_kgha) %>% 
  group_by(nrate_kgha, croprot) %>% 
  summarise(mdn = median(leaching_kgha),
            lo = quantile(leaching_kgha, .75),
            hi = quantile(leaching_kgha, .25)) %>% 
  mutate(
    croprotnice = dplyr::recode(croprot,
                                corncc = "Continuous Maize",
                                corncs = "Rotated Maize",
                                soycs = "Rotated Soybean")) %>%
  ggplot() + 
  geom_line(aes(nrate_kgha, mdn, color = croprotnice), size = 2) + 
  geom_ribbon(aes(nrate_kgha, ymin = lo, ymax = hi, fill = croprotnice), alpha = 0.2) + 
  scale_fill_manual(values = c(clr1, clr2, clr3)) + 
  scale_color_manual(values = c(clr1, clr2, clr3))  
  
