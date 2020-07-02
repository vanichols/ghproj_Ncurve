# author: gina
# created: 4/22/2020
# purpose: fit curves to data
# notes: using naive approach
# updated: 6/11/2020, new data
#          7/2/2020, add no3 as resp var

library(tidyverse)
library(nlraa)
library(nlme)
library(broom)
library(plotly)



# read in data ------------------------------------------------------------

rawdat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv")

dat <- 
  rawdat %>% 
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                  "sc" = "cs")) %>%
  select(-yield_soy_buac, -doy, -date) %>%
  select(site_id, year, yearF, rotation, nrate_kgha, everything()) %>% 
  pivot_longer(annual_rain_mm:yield_maize_buac)


# leaching ----------------------------------------------------------------

leach <- 
  dat %>% 
  filter(name == "leaching_kgha") %>% 
  select(-name) %>% 
  arrange(site_id, year, nrate_kgha) 


#--use a bi-linear function
?SSblin

#--huggins test
#
fit1 <- nls(value ~ SSblin(nrate_kgha, a, b, xs, c), 
            data = leach %>%
              filter(site_id == "hugg", rotation == "cc"))
tidy(fit1)
fitted(fit1) %>% tidy()

#--build a function
#
nls_bilinfit <- function(x){
  
  nls(value ~ SSblin(nrate_kgha, a, b, xs, c),
        data = x)
}

#--test function
#
nls_bilinfit(filter(leach, site_id == "hugg"))

#--map function to data
leach_parms <-
  leach %>%
  nest(data = c(value, nrate_kgha, yearF)) %>%
  mutate(mod = data %>% map(possibly(nls_bilinfit, NULL)),
    is_null = mod %>% map_lgl(is.null)) %>% 
    filter(is_null == 0) %>% 
    mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
    unnest(cols = c(res)) %>% 
    select(site_id, year, rotation, term:p.value)
  
#--how many didn't converge?
# 3, not bad
leach %>% 
  select(site_id, year) %>% 
  distinct() %>% 
  left_join(leach_parms) %>% 
  filter(is.na(term))

write_csv(dat_parms, "02_fit-curves/fc_blin-leach-parms.csv")

leach_parms %>%
  unite(site_id, year, col = "site_year", remove = F) %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(rotation ~ term, scales = "free") + 
  coord_flip() + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Bilinear fit to Leaching vs Nrate")

ggsave("02_fit-curves/figs_blin-leach-parms.png")



# no3 ---------------------------------------------------------------------

no3 <- 
  dat %>% 
  filter(name == "nitrateflow_mg_l") %>% 
  select(-name) %>% 
  arrange(site_id, year, nrate_kgha) 

#--map function to data
no3_parms <-
  no3 %>%
  nest(data = c(value, nrate_kgha, yearF)) %>%
  mutate(mod = data %>% map(possibly(nls_bilinfit, NULL)),
         is_null = mod %>% map_lgl(is.null)) %>% 
  filter(is_null == 0) %>% 
  mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
  unnest(cols = c(res)) %>% 
  select(site_id, year, rotation, term:p.value)

#--how many didn't converge?
# 2, not bad
no3 %>% 
  select(site_id, year) %>% 
  distinct() %>% 
  left_join(dat_parms) %>% 
  filter(is.na(term))

write_csv(dat_parms, "02_fit-curves/fc_blin-no3-parms.csv")

no3_parms %>%
  unite(site_id, year, col = "site_year", remove = F) %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(rotation ~ term, scales = "free") + 
  coord_flip() + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Bilinear fit to no3ing vs Nrate")

ggsave("02_fit-curves/figs_blin-no3-parms.png")



# yield -------------------------------------------------------------------

yield <- 
  dat %>% 
  filter(name == "yield_maize_buac") %>% 
  select(-name) %>% 
  arrange(site_id, year, nrate_kgha) 



#--map function to data
yld_parms <-
  yield %>%
  nest(data = c(value, nrate_kgha, yearF)) %>%
  mutate(mod = data %>% map(possibly(nls_bilinfit, NULL)),
         is_null = mod %>% map_lgl(is.null)) %>% 
  filter(is_null == 0) %>% 
  mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
  unnest(cols = c(res)) %>% 
  select(site_id, year, rotation, term:p.value)

#--how many didn't converge?
# 11, not terrible
yield %>% 
  select(site_id, year) %>% 
  distinct() %>% 
  left_join(yld_parms) %>% 
  filter(is.na(term))

write_csv(yld_parms, "02_fit-curves/fc_blin-yield-parms.csv")

yld_parms %>%
  unite(site_id, year, col = "site_year", remove = F) %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(rotation ~ term, scales = "free") + 
  coord_flip() + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Bilinear fit to Yields vs Nrate")

ggsave("02_fit-curves/figs_blin-yield-parms.png")




# explore -----------------------------------------------------------------


#--does the xs from the leaching match the xs from the yield?
dat_parms %>% 
  select(site_id, year, rotation, term, estimate, std.error) %>% 
  filter(term == "xs") %>% 
  rename(leach = estimate,
         leach_se = std.error) %>% 
  left_join(
    dat_parms2 %>% 
  select(site_id, year, rotation, term, estimate, std.error) %>% 
  filter(term == "xs") %>% 
  rename(ylds = estimate,
         ylds_se = std.error) 
  ) %>% 
  ggplot(aes(leach, ylds)) + 
  geom_point() + 
  facet_grid(.~rotation) + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "leaching pivot point",
       y = "yield pivot point")


dat_parms %>% 
  mutate(respvar = "leaching") %>% 
  bind_rows(dat_parms2 %>% 
              mutate(respvar = "yields")
  ) %>% 
  unite(site_id, year, col = "site_year", remove = F) %>% 
  filter(term == "xs") %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(.~ rotation + respvar, scales = "free") + 
  theme(axis.text.x = element_blank()) +
  labs(title = "Pivot Point")
  


# leave cs and sc separate ------------------------------------------------

#--change data
dat2 <- 
  rawdat %>%  
  mutate(yearF = as.factor(year)
         #rotation = dplyr::recode(rotation,
          #                        "sc" = "cs")
         ) %>%
  select(-yield_soy_buac, -doy, -date) %>%
  select(site_id, year, yearF, rotation, nrate_kgha, everything()) %>% 
  pivot_longer(annual_rain_mm:yield_maize_buac)

#--leaching
leach <- 
  dat2 %>% 
  filter(name == "leaching_kgha") %>% 
  select(-name) %>% 
  arrange(site_id, year, nrate_kgha) 

parms_leach <-
  leach %>%
  nest(data = c(value, nrate_kgha, yearF)) %>%
  mutate(mod = data %>% map(possibly(nls_bilinfit, NULL)),
         is_null = mod %>% map_lgl(is.null)) %>% 
  filter(is_null == 0) %>% 
  mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
  unnest(cols = c(res)) %>% 
  select(site_id, year, rotation, term:p.value)

parms_leach  

# how many didn't converge?
leach %>% 
  select(site_id, year) %>% 
  distinct() %>% 
  left_join(dat_parms) %>% 
  filter(is.na(term))


parms_leach %>%
  unite(site_id, year, col = "site_year", remove = F) %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(rotation ~ term, scales = "free") + 
  coord_flip() + 
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank()) + 
  labs(title = "Bilinear fit to Leaching vs Nrate") 


#--yield

yield <- 
  dat2 %>% 
  filter(name == "yield_maize_buac") %>% 
  select(-name) %>% 
  arrange(site_id, year, nrate_kgha) 


parms_yield <-
  yield %>%
  nest(data = c(value, nrate_kgha, yearF)) %>%
  mutate(mod = data %>% map(possibly(nls_bilinfit, NULL)),
         is_null = mod %>% map_lgl(is.null)) %>% 
  filter(is_null == 0) %>% 
  mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
  unnest(cols = c(res)) %>% 
  select(site_id, year, rotation, term:p.value)

yield %>% 
  select(site_id, year) %>% 
  distinct() %>% 
  left_join(dat_parms2) %>% 
  filter(is.na(term))

parms_yield %>%
  unite(site_id, year, col = "site_year", remove = F) %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(rotation ~ term, scales = "free") + 
  coord_flip() + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Bilinear fit to Yields vs Nrate")

#--does the xs from the leaching match the xs from the yield?
parms_leach %>% 
  select(site_id, year, rotation, term, estimate, std.error) %>% 
  filter(term == "xs") %>% 
  rename(leach = estimate,
         leach_se = std.error) %>% 
  left_join(
    parms_yield %>% 
      select(site_id, year, rotation, term, estimate, std.error) %>% 
      filter(term == "xs") %>% 
      rename(ylds = estimate,
             ylds_se = std.error) 
  ) %>% 
  ggplot(aes(leach, ylds)) + 
  geom_point() + 
  facet_grid(.~rotation) + 
  geom_smooth(method = "lm", se = F)


parms_leach %>% 
  mutate(respvar = "leaching") %>% 
  bind_rows(parms_yield %>% 
              mutate(respvar = "yields")
  ) %>% 
  unite(site_id, year, col = "site_year", remove = F) %>% 
  filter(term == "xs") %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_grid(.~ rotation + respvar, scales = "free") + 
  theme(axis.text.x = element_blank()) +
  labs(title = "Pivot Point")


#--are the values related to precip? or flow?
parms_leach %>% 
  mutate(respvar = "leaching") %>% 
  bind_rows(parms_yield %>% 
              mutate(respvar = "yields")
  ) %>% 
  unite(site_id, year, col = "site_year", remove = F) %>% 
  filter(term == "xs") %>% 
  left_join(dat2 %>% 
              filter(name %in% c("drainage_mm", "inseason_rain_mm")) %>% 
              pivot_wider(names_from = name, values_from = value) %>% 
              select(-nrate_kgha) %>% 
              distinct()) %>% 
  ggplot(aes(site_year, estimate, color = site_id)) + 
  geom_point(aes(size = inseason_rain_mm)) + 
  facet_grid(.~ rotation + respvar, scales = "free") + 
  theme(axis.text.x = element_blank()) +
  labs(title = "Pivot Point")


