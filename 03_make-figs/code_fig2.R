# author: gina
# created: 7/6/2020
# purpose: explore relationship between yield and leaching parms
# last updated: 7/9/2020 fixed coefs
#               8/20/2020 removed sutherland
#               2/4/2021 made edits

rm(list = ls())

#devtools::install_github("wilkelab/ungeviz")
library(ungeviz)
library(tidyverse)
library(patchwork)
library(scales)
library(ggridges)
#install.packages("remotes")
#remotes::install_github("daattali/colourpicker")


clr1 <- "#2a89c3"#"darkblue"
clr2 <- "#fdb462" #"orange2"



nleachlab <- expression(Nitrogen~Leached~(kg~N~ha^-1))
yldlab <- expression(Maize~Yield~(Mg~ha^-1))
nfertlab <- expression(Nitrogen~Fertilizer~Applied~(kg~N~ha^-1))
bufflab <- expression(Environmental~Buffer~(kg~N~ha^-1))

# buffer fig --------------------------------------------------------------


rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv")
yldprms <- read_csv("02_fit-curves/fc_blin-yield-parms-mm.csv")
leach_prms <- read_csv("02_fit-curves/fc_blin-leach-parms-mm.csv")


yld_xs <-
  yldprms %>% 
  pivot_longer(a:xs) %>% 
  filter(name == "xs") %>% 
  rename(yield_xs = value) %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) 

#--note leach xs is only specific to rotation
leach_xs <- 
  leach_prms %>%
  pivot_longer(a:c) %>% 
  filter(name == "xs") %>% 
  rename(leach_xs = value) %>% 
  distinct() %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) 

buff <- 
  yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) %>% 
  select(-leach_xs, -yield_xs)

buff_mean <- 
  buff %>% 
  group_by(rotation) %>% 
  summarise(yld_to_lch = mean(yld_to_lch, na.rm = T)) %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize"))


#--violin

buff_fig <- 
  buff %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) %>% 
  ggplot(aes(rot2, yld_to_lch)) + 
  geom_violin(aes(fill = rot2)) + 
  geom_hpline(width = 0.05, size = 0.5, color = "gray20") +
  geom_hpline(data = buff_mean, width = 0.2, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Continuous Maize" = clr2, 
                               "Rotated Maize" = clr1)) +
  labs(fill = NULL,
       y = bufflab,
       x = NULL,
       title = "(a)") +
  guides(color = F,
         fill = F) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.position = "top",
        legend.text = element_text(size = rel(1.3)),
        plot.title = element_text(size = rel(1.5), face = "bold"))

buff_fig

buff %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) %>%
  select(-rotation, -rot) %>% 
  pivot_wider(names_from = rot2, values_from = yld_to_lch) %>% 
  ggplot(aes(x=x)) +
  geom_point(data = buff_mean, x = -0.0025, aes(y = yld_to_lch, fill = rot2), pch = 22, size = 5) +
  geom_density(aes(y = `Continuous Maize`,x = -..density..), fill = clr2) +
  geom_density(aes(y = `Rotated Maize`,x = ..density..), fill = clr1) + 
  geom_hpline(x = -0.0005, aes(y = `Continuous Maize`), width = 0.001, color = "#D2691E", size = 0.5) +
  geom_hpline(x = 0.0005, aes(y = `Rotated Maize`), width = 0.001, color = "#165B85", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hpline(data = buff_mean %>% filter(rot == "cc"), 
              x = -0.0025, aes(y = yld_to_lch), width = 0.005, size = 2, 
              color = "#D2691E") +
  geom_hpline(data = buff_mean %>% filter(rot == "cs"), 
              x = 0.0025, aes(y = yld_to_lch), width = 0.005, size = 2,
              color = "#165B85") +
  scale_fill_manual(values = c("Continuous Maize" = clr2, 
                               "Rotated Maize" = clr1)) +
  labs(fill = NULL,
       y = bufflab,
       x = "Data Density",
       title = "(a)") +
  # guides(color = F,
  #        fill = F) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_blank(),
        axis.title = element_text(size = rel(1.3)),
        legend.position = c(0.1, 0.9),
        legend.direction = "vertical",
        legend.justification = c(0.2, 0.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = rel(1.3)),
        plot.title = element_text(size = rel(1.5), face = "bold"))

#--a new buff fig
buff_fig <- 
  buff %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize")) %>%
  select(-rotation, -rot) %>% 
  pivot_wider(names_from = rot2, values_from = yld_to_lch) %>% 
  ggplot(aes(x=x)) +
  geom_point(data = buff_mean, x = -0.0025, aes(y = yld_to_lch, fill = rot2), pch = 22, size = 5) +
  geom_density(aes(y = `Continuous Maize`,x = -..density..), fill = clr2) +
  geom_density(aes(y = `Rotated Maize`,x = ..density..), fill = clr1) + 
  geom_hpline(x = -0.0005, aes(y = `Continuous Maize`), width = 0.001, color = "#D2691E", size = 0.5) +
  geom_hpline(x = 0.0005, aes(y = `Rotated Maize`), width = 0.001, color = "#165B85", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hpline(data = buff_mean %>% filter(rot == "cc"), 
              x = -0.0025, aes(y = yld_to_lch), width = 0.005, size = 2, 
              color = "#D2691E") +
  geom_hpline(data = buff_mean %>% filter(rot == "cs"), 
              x = 0.0025, aes(y = yld_to_lch), width = 0.005, size = 2,
              color = "#165B85") +
  scale_fill_manual(values = c("Continuous Maize" = clr2, 
                               "Rotated Maize" = clr1)) +
  labs(fill = NULL,
       y = bufflab,
       x = "Data Density",
       title = "(a)") +
  # guides(color = F,
  #        fill = F) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.text.y = element_blank(),
        axis.title = element_text(size = rel(1.3)),
        legend.position = c(0.15, 0.95),
        legend.direction = "vertical",
        legend.justification = c(0.2, 0.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = rel(1.3)),
        plot.title = element_text(size = rel(1.5), face = "bold")) + 
  coord_flip()




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
  scale_color_manual(values = c("Continuous Maize" = clr2,
                                "Rotated Maize" = clr1)) +
  labs(color = NULL,
       y = nleachlab,
       x = nfertlab) +
  theme_bw() + 
  guides(color = F) +
  theme(legend.position = c(0.2,0.9),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.3)),
        legend.background = element_rect(color = "black"))


leach_fig2 <- 
  leach_xs  %>% 
  ggplot(aes(name, leach_xs)) + 
  geom_boxplot(aes(fill = rotation, color = rotation)) + 
  scale_fill_manual(values = c(clr2,
                               clr1)) +
  scale_color_manual(values = c(clr2,
                               clr1)) +
  scale_y_continuous(limits = c(0, 300)) +
  guides(fill = F,
         color = F) +
  labs(x = NULL, y = NULL) +
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.3)),
    legend.background = element_rect(color = "black")
  )


leach_rug <- 
  pred_dat %>% 
  filter(preds > 0) %>% 
  ggplot(aes(nrate_kgha, preds, group = eu)) + 
  geom_line(color = "gray80") + 
  geom_line(data = pred_dat2, 
            aes(nrate_kgha, preds, color = rot2, group = rot2),
            size = 2) + 
  geom_rug(data = leach_xs, 
           aes(x = leach_xs, y = 0, color = rot2, group = rot2), 
           position = "jitter",
           alpha = 0.5,
           sides = "b") +
  scale_color_manual(values = c("Continuous Maize" = clr2,
                                "Rotated Maize" = clr1)) +
  labs(color = NULL,
       y = nleachlab,
       x = nfertlab,
       title = "(c)") +
  theme_bw() + 
  guides(color = F) +
  theme(legend.position = c(0.2,0.9),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.3)),
        legend.background = element_rect(color = "black"),
        plot.title = element_text(size = rel(1.5), face = "bold"))



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
  scale_color_manual(values = c("Continuous Maize" = clr2,
                                "Rotated Maize" = clr1)) +
  labs(color = NULL,
       y = yldlab,
       x = nfertlab) +
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


#--yield box plot

yield_fig2 <- 
  yld_xs  %>% 
  ggplot(aes(name, yield_xs)) + 
  geom_boxplot(aes(fill = rotation)) + 
  scale_fill_manual(values = c(clr2,
                                clr1)) +
  scale_y_continuous(limits = c(0, 300)) +
  guides(fill = F) +
  labs(x = NULL, y = NULL) +
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.3)),
    legend.background = element_rect(color = "black")
  )


yield_rug <- 
  pred_dat3 %>% 
  ggplot(aes(nrate_kgha, preds, group = eu)) +
  geom_line(color = "gray80") + 
  geom_line(data = pred_dat4, 
            aes(nrate_kgha, preds, color = rot2, group = rot2),
            size = 2) + 
  geom_rug(data = yld_xs, 
           aes(x = yield_xs, y = 0, color = rot2, group = rot2),
           alpha = 0.5, position = "jitter", 
           sides = "b") +
  scale_color_manual(values = c("Continuous Maize" = clr2,
                                "Rotated Maize" = clr1)) +
  labs(color = NULL,
       y = yldlab,
       x = nfertlab,
       title = "(b)") +
  guides(color = F) +
  theme_bw() + 
  theme(legend.position = "top") + 
  #theme(legend.position = c(0.2, 0.9)) + 
  theme(
    axis.text = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.3)),
    legend.background = element_rect(color = "black"),
    plot.title = element_text(size = rel(1.5), face = "bold"))



# put together ------------------------------------------------------------

# legned on top and centered?
#https://github.com/thomasp85/patchwork/issues/122

#(yield_fig / leach_fig) | buff_fig + plot_layout(guides = "collect") & theme(legend.position = 'top')


#--not working, i give up. actually it is working.
# (yield_fig / leach_fig) | buff_fig 
# ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_yld-lch-buff-10-09-20.png")


#--trying with boxplots
# (yield_fig2 + yield_fig + leach_fig2 + leach_fig + plot_layout(ncol = 1, heights = c(1, 4, 1, 4))) |buff_fig
# 
# ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_yld-lch-buff-9-14-20.png")
# 
# 
# 
# #--trying with only aonr boxplots?
# (yield_fig + yield_fig2 + leach_fig + plot_layout(ncol = 1, heights = c(4, 1, 4))) |buff_fig
# 
# ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_yld-lch-buff-9-14-20-v2.png")



#--yield rug?
# (yield_rug + leach_rug + plot_layout(ncol = 1, heights = c(4, 4))) |buff_fig
# ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_yld-lch-buff-10-09-20-v3.png")


buff_fig | (yield_rug + leach_rug + plot_layout(ncol = 1, heights = c(4, 4)))
#ggsave("../../../Box/1_Gina_Projects/proj_Ncurve/fig_yld-lch-buff-10-16-20.png")
ggsave("fig2.png")
