# author: gina
# created: 6/1/2020
# purpose: process heather's data
# last updated: 6/8/2020 (yields weren't crop specific, asked heather to change)
#               6/17/2020 look at soils data
#               8/20/2020 (removed sutherland, checking)
#               9/14/2020 (make it look nice)

library(tidyverse)
library(plotly)
library(scales)
library(ggridges)




# plot things -------------------------------------------------------------

clr1 <- "orange2"
clr2 <- "darkblue"
clr3 <- "#00BA38"

show_col(clr3)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  mutate(rotation2 = ifelse(rotation == "sc", "cs", rotation),
         croprot = paste0(crop, rotation2)) %>% 
  select(croprot, everything())

rawdat %>% 
  mutate(nrateF = as.factor(nrate_kgha),
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
  scale_fill_manual(values = c(clr2, clr1, clr3)) + 
  labs(y = expression(Nitrogen~Feritilizer~Rate~(kg~N~ha^-1)),
       x = expression(Nitrogen~Leaching~(kg~N~ha^-1)),
       fill = NULL) + 
  theme_bw() + 
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.justification = c(1, 1),
        legend.position = c(0.95,0.99),
        legend.background = element_blank(),
        legend.text = element_text(size = rel(1.2)))

ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/manuscript/fig_n-leaching-9-14-2020.png")

