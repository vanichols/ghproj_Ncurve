# author: gina
# created: 4/22/2020
# purpose: see which sites are similar/different
# notes: 

library(tidyverse)
library("factoextra")

# read in data ------------------------------------------------------------

soil_dat <- read_csv("data/tidy/td_soil-60cm.csv") %>% 
  select(-site)

# play with pca -----------------------------------------------------------

df <- soil_dat %>% as.data.frame()

df2 <- df[-1]
row.names(df2) <- df$site_id

##--corelations?
library(corrplot)

df2_cor <- df2 %>%
  select_if(is.numeric) 
corres <- cor(df2_cor, use="complete.obs")
corrplot::corrplot.mixed(corres)

#--keep only the values that are most directly measured, not calculated
df3 <- df2 

res2 <- prcomp(df3, scale = TRUE)
fviz_eig(res2)

fviz_pca_ind(res2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



fviz_pca_var(res2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res2,
                repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/


soil_dat %>% 
  filter(site_id %in% c("rand", "hugg"))
