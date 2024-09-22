
## This file performs the principal component analysis for H1 

library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(tidyr)
library(runner)
library(factoextra)
library(sf)
# library(tricolore)
library(ggthemes)
library(ggpubr)
library(reshape2)
library(readxl)

setwd("") ## update working directory here

set.seed(1235) ## IMPORTANT FOR K-MEANS

district_names <- read.csv(paste0(getwd(),"/Insurance/Defaults_admin2.csv"))

droughtyears_raw <- read.csv(paste0(getwd(),"/Farmer Survey/BadYears_admin2.csv")) 

yield_raw <- read_xlsx(paste0(getwd(),"/Yield/district_maize_combined_cleaned.xlsx"))

map.temp.path <- paste0(getwd(),"/Mapping/zambia_shapefiles/")

admin2.sf <- sf::st_read(paste0(map.temp.path,"zmb_admbnda_adm2_dmmu_20201124.shp"),
                         quiet = TRUE) %>% 
  st_simplify(., preserveTopology = FALSE, dTolerance = .03)

admin1.sf <- sf::st_read(paste0(map.temp.path,"zmb_admbnda_adm1_dmmu_20201124.shp"),
                         quiet = TRUE) %>% 
  st_simplify(., preserveTopology = FALSE, dTolerance = .03)

border.sf <- sf::st_read(paste0(map.temp.path,"zmb_admbnda_adm0_dmmu_20201124.shp"),
                         quiet = TRUE)

admin2.sf$ADM2_EN <- gsub("Itezhi-tezhi","Itezhi_tezhi",admin2.sf$ADM2_EN,fixed=T)
admin2.sf$ADM2_EN <- gsub("Lunte District","Lunte",admin2.sf$ADM2_EN,fixed=T)
admin2.sf$ADM2_EN <- gsub("Senga Hill","Senga_Hil",admin2.sf$ADM2_EN,fixed=T)
admin2.sf$ADM2_EN <- gsub("Shang'ombo","Shangombo",admin2.sf$ADM2_EN,fixed=T)
admin2.sf$ADM2_EN <- gsub("Kapiri Mposhi","Kapiri_Mposhi",admin2.sf$ADM2_EN,fixed=T)
admin2.sf$ADM2_EN <- gsub("Shiwamg'andu","Shiwang_andu",admin2.sf$ADM2_EN,fixed=T)

admin2_crosswalk <- read_xlsx(paste0(getwd(),"/Mapping/district_2000_2022_crosswalk.xlsx"))[,-1]

admin2.sf <- left_join(admin2.sf,admin2_crosswalk,by=c("ADM2_EN"="district_2022"))

payouts <- read.csv(paste0(getwd(),"/Insurance/drought_only_payouts.csv"))[,-1] %>%
  dplyr::select(district,year,payout) %>%
  pivot_wider(names_from = 'year',values_from = 'payout',id_cols = 'district')

## Payout data PCA

payouts_pca <- prcomp(payouts[,-c(1)],scale=FALSE,center=FALSE) ## unsure whether to scale here
summary(payouts_pca)

res.ind.payouts <- get_pca_ind(payouts_pca)

loadings <- payouts_pca$rotation

loadings_long <- as.data.frame(loadings) %>% mutate(year = rownames(loadings)) %>% dplyr::select(PC1:PC3,year) %>% pivot_longer(c(PC1:PC3),names_to="component",values_to="loading")

payout_loadings <- loadings_long

payouts_years <- ggplot(loadings_long %>% filter(year >= 2000 & year <= 2019) %>% group_by(year,component) %>% summarise(loading = mean((loading))),aes(x=component,fill=component,y=loading)) +
  facet_wrap(~year,nrow=1) +
  # scale_fill_manual(values=c("green","red","blue")) +
  xlab("payouts") +
  ylim(c(-0.8,0.8)) +
  geom_col(position = "stack") +
  theme(legend.position = 'none', axis.text.x = element_blank())

payouts_years_all <- ggplot(loadings_long %>% filter(year <= 2019) %>% group_by(year,component) %>% summarise(loading = mean((loading))),aes(x=component,fill=component,y=loading)) +
  facet_wrap(~year,nrow=1) +
  # scale_fill_manual(values=c("green","red","blue")) +
  xlab("payouts") +
  ylim(c(-0.8,0.8)) +
  geom_col(position = "stack") +
  theme(legend.position = 'none', axis.text.x = element_blank())

ind.coords.payouts <- as.data.frame(res.ind.payouts$coord[,c(1:3)])
ind.coords.payouts$district <- payouts$district

admin2.sf.payouts <- left_join(admin2.sf,ind.coords.payouts,by=c("ADM2_EN" = "district")) %>%
  pivot_longer(c(`Dim.1`:`Dim.3`),names_to = "component",values_to = "loading")

pca_map_payouts <- ggplot() + 
  geom_sf(data = admin2.sf.payouts, aes(fill = loading, geometry=geometry),
          alpha = .75, color = "grey75") +
  scale_fill_gradient2(midpoint=0) +
  geom_sf(data = admin1.sf,aes(geometry = geometry),
          color = "black", size = 1, fill = NA) +
  # ggrepel::geom_label_repel(data = admin1.sf,
  #                           aes(label = ADM1_EN, geometry = geometry),
  #                           size=2,
  #                           nudge_x = 2, nudge_y = -1,
  #                           stat = "sf_coordinates",
  #                           min.segment.length = 0,
  #                           label.size = NA
  # ) +
  facet_wrap(~component) +
  theme_map() +
  theme(legend.position = 'bottom') +
  labs(fill="PC Coordinate") +
  ggtitle("Drought index PCs (76% of var)")

## Farmer PCA


droughtyears <- droughtyears_raw %>%
  gather(key="rank",value="year",-c(gid:bad_aggregation))

droughtyears$rank <- as.numeric(gsub("bad","",droughtyears$rank))

droughtyears$rank <- 11 - droughtyears$rank

droughtyears <- droughtyears %>%
  dplyr::select(-c(gid,bad_aggregation))

droughtyears <- droughtyears %>%
  dcast(province + district ~ year, value.var = "rank",max)

droughtyears <- droughtyears %>%
  dplyr::select(-"NA")

is.na(droughtyears)<-sapply(droughtyears, is.infinite)
droughtyears[is.na(droughtyears)]<-0

drought_pca <- prcomp(droughtyears[,-c(1:2)],scale=FALSE,center=FALSE) ## unsure whether to scale here
summary(drought_pca)

res.ind.drought <- get_pca_ind(drought_pca)

loadings <- drought_pca$rotation

loadings_long <- as.data.frame(loadings) %>% mutate(year = rownames(loadings)) %>% dplyr::select(PC1:PC3,year) %>% 
  pivot_longer(c(PC1:PC3),names_to="component",values_to="loading") %>%
  mutate(loading = ifelse(component == "PC3",-loading,loading)) %>% ## making component sign consistent
  mutate(component = factor(component,levels=c("PC1","PC3","PC2"))) ## making component order consistent

farmer_loadings <- loadings_long

farmer_years <- ggplot(loadings_long %>% filter(year >= 2000) %>% group_by(year,component) %>% summarise(loading = mean((loading))),aes(x=component,fill=component,y=loading)) +
  facet_wrap(~year,nrow=1) +
  # scale_fill_manual(values=c("green","red","blue")) +
  xlab("farmer") +
  ylim(c(-0.8,0.8)) +
  geom_col(position = "stack") +
  theme(legend.position = 'none', axis.text.x = element_blank())

farmer_years_all <- ggplot(loadings_long %>% filter(year > 1983) %>% group_by(year,component) %>% summarise(loading = mean((loading))),aes(x=component,fill=component,y=loading)) +
  facet_wrap(~year,nrow=1) +
  # scale_fill_manual(values=c("green","red","blue")) +
  xlab("farmer") +
  ylim(c(-0.8,0.8)) +
  geom_col(position = "stack") +
  theme(legend.position = 'none', axis.text.x = element_blank())


ind.coords.drought <- as.data.frame(res.ind.drought$coord[,c(1:3)])
ind.coords.drought$district <- droughtyears$district

admin2.sf.drought <- left_join(admin2.sf,ind.coords.drought,by=c("ADM2_EN" = "district")) %>%
  pivot_longer(c(`Dim.1`:`Dim.3`),names_to = "component",values_to = "loading") %>%
  mutate(loading = ifelse(component == "Dim.3",-loading,loading)) %>% ## making component sign consistent
  mutate(component = factor(component,levels=c("Dim.1","Dim.3","Dim.2"))) ## making component order consistent

pca_map_drought <- ggplot() + 
  geom_sf(data = admin2.sf.drought, aes(fill = loading, geometry=geometry),
          alpha = .75, color = "grey75") +
  scale_fill_gradient2(midpoint=0) +
  geom_sf(data = admin1.sf,aes(geometry = geometry),
          color = "black", size = 1, fill = NA) +
  # ggrepel::geom_label_repel(data = admin1.sf,
  #                           aes(label = ADM1_EN, geometry = geometry),
  #                           size=2,
  #                           nudge_x = 2, nudge_y = -1,
  #                           stat = "sf_coordinates",
  #                           min.segment.length = 0,
  #                           label.size = NA
  # ) +
  facet_wrap(~component) +
  theme_map() +
  theme(legend.position = 'bottom') +
  labs(fill="PC Coordinate") +
  ggtitle("Farmer recall PCs (50% of var)")

## Yield PCA

yield <- yield_raw %>% dplyr::select(District,Province,Year,`Yield Rate (MT/Ha)`) %>%
  rename_at(c(1:4),~c("district","province","year","yield")) %>%
  mutate(yield = ifelse(year == 2000,yield*100,yield)) %>%
  group_by(district,year) %>%
  summarise(yield = mean(yield,na.rm = T)) %>% 
  ungroup() %>%
  filter(year >= 2000) %>%
  pivot_wider(names_from = 'year', values_from = 'yield')
  
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
yield[,-1] <- replace(yield[,-1], TRUE, lapply(yield[,-1], NA2mean)) ## interpolate missing values w/ year mean

yield[,-1] <- t(apply(yield[,-1],1,scale))

yield_pca <- prcomp(yield[,-c(1)],scale=FALSE,center=FALSE) ## should we detrend here? i think not?
summary(yield_pca)

res.ind.yield <- get_pca_ind(yield_pca)

loadings <- yield_pca$rotation

loadings_long <- as.data.frame(loadings) %>% mutate(year = rownames(loadings)) %>% dplyr::select(PC1:PC3,year) %>% pivot_longer(c(PC1:PC3),names_to="component",values_to="loading")

yield_years <- ggplot(loadings_long %>% filter(year <= 2019) %>% group_by(year,component) %>% summarise(loading = mean((loading))),aes(x=component,fill=component,y=loading)) +
  facet_wrap(~year,nrow=1) +
  # scale_fill_manual(values=c("green","red","blue")) +
  geom_col(position = "stack") +
  xlab("yield") +
  ylim(c(-0.8,0.8)) +
  theme(legend.position = 'none')


ind.coords.yield <- as.data.frame(res.ind.yield$coord[,c(1:3)])
ind.coords.yield$district <- yield$district

admin2.sf.yield <- left_join(admin2.sf,ind.coords.yield,by=c("district_2000" = "district")) %>%
  pivot_longer(c(`Dim.1`:`Dim.3`),names_to = "component",values_to = "loading")

pca_map_yield <- ggplot() + 
  geom_sf(data = admin2.sf.yield, aes(fill = loading, geometry=geometry),
          alpha = .75, color = "grey75") +
  scale_fill_gradient2(midpoint=0) +
  geom_sf(data = admin1.sf,aes(geometry = geometry),
          color = "black", size = 1, fill = NA) +
  # ggrepel::geom_label_repel(data = admin1.sf,
  #                           aes(label = ADM1_EN, geometry = geometry),
  #                           size=2,
  #                           nudge_x = 2, nudge_y = -1,
  #                           stat = "sf_coordinates",
  #                           min.segment.length = 0,
  #                           label.size = NA
  # ) +
  facet_wrap(~component) +
  theme_map() +
  theme(legend.position = 'none') +
  ggtitle("Crop Forecast Survey")

## plot of consensus bad years by province

badyears_province <- droughtyears_raw %>% 
  mutate(province = factor(province, levels = c("Western","North_Western","Southern","Copperbelt","Central","Lusaka","Luapula","Northern","Muchinga","Eastern"))) %>% # to keep things west-east 
  pivot_longer(bad1:bad10,names_to="rank",values_to="year") %>%
  mutate(year = factor(year,levels=c(1983:2019))) %>%
  mutate(rank = as.numeric(gsub("bad","",rank))) %>%
  mutate(rank = 11-rank) %>%
  group_by(province,year) %>%
  summarise(countXrank = sum(rank,na.rm=T)) %>%
  mutate(countXrank_scaled = countXrank / sum(countXrank)) %>%
  ungroup() %>%
  filter(!is.na(year))
  
## plot of consensus bad years by PC grouping

pc_discrete <- kmeans(ind.coords.drought[,-4],3)
pc_discrete <- data.frame("district"=ind.coords.drought[,4],"group"=pc_discrete$cluster)
    
admin2.sf.grouped <- left_join(admin2.sf,pc_discrete,by=c("ADM2_EN"="district"))

ggplot() + 
  geom_sf(data = admin2.sf.grouped, aes(fill = factor(group), geometry=geometry),
          alpha = .75, color = "grey75") +
  geom_sf(data = admin1.sf,aes(geometry = geometry),
          color = "black", size = 1, fill = NA) +
  scale_fill_brewer(palette="Set2") +
  # ggrepel::geom_label_repel(data = admin1.sf,
  #                           aes(label = ADM1_EN, geometry = geometry),
  #                           size=2,
  #                           nudge_x = 2, nudge_y = -1,
  #                           stat = "sf_coordinates",
  #                           min.segment.length = 0,
  #                           label.size = NA
  # ) +
  theme_map() +
  theme(legend.position = 'bottom')

badyears_group <-  droughtyears_raw %>% 
  left_join(pc_discrete,by="district") %>%
  pivot_longer(bad1:bad10,names_to="rank",values_to="year") %>%
  mutate(year = factor(year,levels=c(1983:2019))) %>%
  mutate(rank = as.numeric(gsub("bad","",rank))) %>%
  mutate(rank = 11-rank) %>%
  group_by(group,year) %>%
  summarise(countXrank = sum(rank,na.rm=T)) %>%
  mutate(countXrank_scaled = countXrank / sum(countXrank)) %>%
  ungroup() %>%
  filter(!is.na(year))

payouts_group <- left_join(payouts,pc_discrete,by="district") %>%
  pivot_longer(`1984`:`2021`,names_to = 'year',values_to = 'payout') %>%
  mutate(year = as.numeric(year)) %>%
  filter(year <= 2019) %>%
  mutate(year = factor(year,levels=c(1983:2019))) %>%
  group_by(group,year) %>%
  summarise(payout = sum(payout,na.rm=T)) %>%
  mutate(payout_scaled = payout / sum(payout)) %>%
  ungroup()

badyears_group <- left_join(badyears_group,payouts_group,by=c("group" = "group",
                                                              "year" = "year"))

## Visualize

ggplot(badyears_province) +
  geom_col(aes(x=countXrank_scaled,y=year,fill=countXrank_scaled)) +
  scale_fill_viridis_c() +
  facet_wrap(~province,nrow=2) +
  theme(legend.position = 'none')

ggplot(badyears_group %>% mutate(group = factor(group,labels = c("Central","South","North"))) ) +
  geom_col(aes(x=countXrank_scaled,y=year,fill=countXrank_scaled)) +
  geom_errorbarh(aes(xmax=payout_scaled,xmin=0,y=year),color='red',alpha=0.5) +
  theme_bw() +
  scale_fill_viridis_c() +
  facet_wrap(~group,nrow=1) +
  ggtitle("Farmer consensus severity (bar), drought severity (red)") +
  xlab("") +
  theme(legend.position = 'none')  

ggarrange(pca_map_drought,pca_map_payouts,nrow= 2, common.legend = TRUE, legend = "top")
ggarrange(payouts_years_all,farmer_years_all,nrow = 2) 

# write.csv(pc_discrete,paste0(getwd(),"/Farmer Survey/badyear_groups.csv"))

## addition 4/20 - more quantitative correlation analysis

# over space

farmer_rotation_long <- ind.coords.drought %>% pivot_longer(-district,names_to="component",values_to = "farmer_rotation") %>%
  mutate(farmer_rotation = ifelse(component == "Dim.3",-farmer_rotation,farmer_rotation)) %>%
  mutate(component = recode(component,'Dim.2' = "Dim.3", "Dim.3" = "Dim.2"))
payout_rotation_long <- ind.coords.payouts %>% pivot_longer(-district,names_to="component",values_to = "payout_rotation")

rotations_joined <- left_join(farmer_rotation_long,payout_rotation_long,by=c("district" = "district", "component" = "component"))

cor.test(rotations_joined$farmer_rotation,rotations_joined$payout_rotation,method="spearman")

rotations_joined %>% group_by(component) %>% 
  summarise(r = cor(x=farmer_rotation,y=payout_rotation,use="complete.obs",method="spearman"))

# cross-cor over space between pairs of components

rotations_wide <- rotations_joined %>% pivot_wider(values_from = c(farmer_rotation, payout_rotation), names_from = component)
rotations_cor_matrix <- cor(rotations_wide[,-1],use="complete.obs",method="spearman")
rotations_cor_matrix <- rotations_cor_matrix[c(1:3),c(4:6)]

library("corrplot")
corrplot(rotations_cor_matrix,method="number")

# over time

loadings_joined <- left_join(payout_loadings %>% rename("payout_loading" = "loading"),
                             farmer_loadings %>% rename("farmer_loading" = "loading") %>%
                               mutate(component = recode(component,'PC3' = "PC2", "PC2" = "PC3")),
                             by=c("year" = "year", "component" = "component"))

cor.test(loadings_joined$farmer_loading,loadings_joined$payout_loading,method="spearman")

loadings_joined %>% group_by(component) %>% 
  summarise(r = cor(x=farmer_loading,y=payout_loading,use="complete.obs",method="spearman"))

ggplot(loadings_joined,aes(x=payout_loading,y=farmer_loading,color=component,group=component,
                           label=year)) +
  geom_text(check_overlap = TRUE) +
  scale_color_brewer(palette="Set2") +
  theme_bw() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  xlab("Drought PC loading") +
  ylab("Farmer PC loading")


# cross-cor over time between pairs of components

loadings_wide <- loadings_joined %>% pivot_wider(values_from = c(farmer_loading, payout_loading), names_from = component)
loadings_cor_matrix <- cor(loadings_wide[,-1],use="complete.obs",method="spearman")
loadings_cor_matrix <- loadings_cor_matrix[c(1:3),c(4:6)]
corrplot(loadings_cor_matrix,method="number")


# comparison with yield

time_series_joined <- read.csv("time_series_joined.csv") %>%
  left_join(badyears_group %>% mutate(year = as.numeric(as.character(year))),by=c("group" = "group", "year" = "year")) %>%
  group_by(district_2000) %>%
  filter(yield < 10) %>%
  mutate(yield_std = scale(yield,center=TRUE,scale=TRUE)[,1]) %>%
  ungroup()

ggplot(time_series_joined %>% filter(year < 2020),aes(x=factor(year),y=yield_std,color=countXrank_scaled)) +
  geom_boxplot() +
  scale_color_viridis_c(na.value = "#440154") +
  facet_wrap(~group) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))

## verision of k-means map that is consistent with paper 

farmer_groups <- read.csv(paste0(getwd(),"/Farmer Survey/badyear_groups.csv"))

admin2.sf.grouped <- left_join(admin2.sf,farmer_groups,by=c("ADM2_EN"="district"))

ggplot() + 
  geom_sf(data = admin2.sf.grouped, aes(fill = factor(group), geometry=geometry),
          alpha = .75, color = "grey75") +
  geom_sf(data = admin1.sf,aes(geometry = geometry),
          color = "black", size = 1, fill = NA) +
  scale_fill_viridis_d() +
  # ggrepel::geom_label_repel(data = admin1.sf,
  #                           aes(label = ADM1_EN, geometry = geometry),
  #                           size=2,
  #                           nudge_x = 2, nudge_y = -1,
  #                           stat = "sf_coordinates",
  #                           min.segment.length = 0,
  #                           label.size = NA
  # ) +
  theme_map() 
  # theme(legend.position = 'false')
