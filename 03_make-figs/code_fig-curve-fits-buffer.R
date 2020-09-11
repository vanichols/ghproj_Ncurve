# author: gina
# created: 7/6/2020
# purpose: explore relationship between yield and leaching parms
# last updated: 7/9/2020 fixed coefs
#               8/20/2020 removed sutherland

rm(list = ls())
library(tidyverse)
library(patchwork)
library(scales)
library(ggridges)



clr1 <- "orange2"
clr2 <- "darkblue"



# buffer fig --------------------------------------------------------------


rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv")
yldprms <- read_csv("02_fit-curves/fc_blin-yield-parms-mm.csv")
leach_prms <- read_csv("02_fit-curves/fc_blin-leach-parms-mm.csv")


yld_xs <-
  yldprms %>% 
  pivot_longer(a:xs) %>% 
  filter(name == "xs") %>% 
  rename(yield_xs = value)

#--note leach xs is only specific to rotation
leach_xs <- 
  leach_prms %>%
  pivot_longer(a:c) %>% 
  filter(name == "xs") %>% 
  rename(leach_xs = value) %>% 
  distinct()

buff <- 
  yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) 


#--density ridges
buff %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) %>% 
  ggplot(aes(yld_to_lch, rot2)) + 
  geom_density_ridges(aes(fill = rot2), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(clr1, clr2)) +
  labs(fill = NULL,
       x = "Difference Between Leaching and Yield Pivot Points (kg N ha-1)",
       y = NULL) +
  guides(color = F,
         fill = F) +
  coord_flip() +
  theme_bw() 


#--violin

buff_fig <- 
  buff %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) %>% 
  ggplot(aes(rot2, yld_to_lch)) + 
  geom_violin(aes(fill = rot2)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(clr2, clr1)) +
  labs(fill = NULL,
       y = "Environmental Buffer (kg N ha-1)",
       x = NULL) +
  guides(color = F,
         fill = F) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.position = "top",
        legend.text = element_text(size = rel(1.3)))



# leaching curves ---------------------------------------------------------

pred_dat <- read_csv("02_fit-curves/fc_leach-preds-eu.csv")
pred_dat2 <- read_csv("02_fit-curves/fc_leach-preds-rot.csv")


leach_fig <- 
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
       x = "Nitrogen Fertilizer Applied (kg N ha-1)") +
  theme_bw() + 
  guides(color = F) +
  theme(legend.position = c(0.2,0.9),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.3)),
        legend.background = element_rect(color = "black"))



# yield curves ------------------------------------------------------------

pred_dat3 <- read_csv("02_fit-curves/fc_yield-preds-eu.csv")
pred_dat4 <- read_csv("02_fit-curves/fc_yield-preds-rot.csv")


yield_fig <- 
  pred_dat3 %>% 
  ggplot(aes(nrate_kgha, preds, group = eu)) + 
  geom_line(color = "gray80") + 
  geom_line(data = pred_dat4, 
            aes(nrate_kgha, preds, color = rot2, group = rot2),
            size = 2) + 
  scale_color_manual(values = c("Continuous Maize" = "darkblue",
                                "Rotated Maize" = "orange2")) +
  labs(color = NULL,
       y = "Maize Grain Yield (Mg ha-1)",
       x = "Nitrogen Fertilizer Applied (kg N ha-1)") +
  guides(color = F) +
  theme_bw() + 
  theme(legend.position = "top") + 
  #theme(legend.position = c(0.2, 0.9)) + 
  theme(
    axis.text = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.3)),
    legend.background = element_rect(color = "black")
  )




# put together ------------------------------------------------------------

# legned on top and centered?
#https://github.com/thomasp85/patchwork/issues/122

#(yield_fig / leach_fig) | buff_fig + plot_layout(guides = "collect") & theme(legend.position = 'top')


#--not working, i give up
(yield_fig / leach_fig) | buff_fig 

ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_yld-lch-buff-9-11-20.png")
