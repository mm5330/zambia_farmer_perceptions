
### This file performs the time series analysis for H2 and H3
### NB: inputs and small v large analysis should start in 2006
### yield analysis should start in 2000

library(dplyr)
library(ggplot2)
library(fitdistrplus)
library(tidyr)
library(runner)
library(factoextra)
library(sf)
library(ggthemes)
library(ggpubr)
library(reshape2)
library(readxl)
library(lubridate)

setwd("") ## update working directory here 

district_names <- read.csv(paste0(getwd(),"/Insurance/Defaults_admin2.csv"))

yield_raw <- read_xlsx(paste0(getwd(),"/Yield/district_maize_combined_cleaned.xlsx"))
yield_disagg <- read_xlsx(paste0(getwd(),"/Yield/district_maize_disaggregate_cleaned.xlsx"))
yield_province <- read_xlsx(paste0(getwd(),"/Yield/maize_yield_raw.xlsx")) %>%
  pivot_longer(-YEAR,names_to = 'province',values_to = 'yield') %>%
  filter(province != "Muchinga") %>%
  mutate(province = gsub("N/Western","North_Western",province))

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

payouts <- read.csv(paste0(getwd(),"/Insurance/drought_only_payouts.csv"))[,-1] 

payouts_national <- payouts %>%
  group_by(year) %>%
  summarise(payout = sum(payout,na.rm=T)) %>%
  mutate(policy_indicator = ifelse(year == 2003,2003,
                                   ifelse(year == 2011,2011,NA))) 

farmer_groups <- read.csv(paste0(getwd(),"/Farmer Survey/badyear_groups.csv"))

wrsi <- read.csv(paste0(getwd(),"/Biophysical/TAMSAT_SM_Adm2_1983_2022.csv")) %>%
  dplyr::select(mean,year,ADM2_NAME) %>%
  rename("wrsi" = "mean") %>%
  mutate(year = year + 1) # to put things in terms of agricultural years

### create data aggregates

## overall

## NOTE - what is going on in 2000? look into this

yield_national <- yield_disagg %>% 
  dplyr::select(-c(District,Category,Province,`Agri-season`,`Year(num)`)) %>%
  mutate_all(as.numeric) %>%
  group_by(Year) %>%
  summarise_all(~sum(.x,na.rm=T)) %>%
  mutate(yield_national = `Expected Production (MT)` / `Area Planted (Ha)`,
         fertilizer_national = (`Fertiliser Basal (MT)` + `Fertiliser Top (MT)`) / `Area Planted (Ha)`  ) %>%
  filter(Year > 2000) %>% ## for now
  mutate(policy_indicator = ifelse(Year == 2003,2003,
                                   ifelse(Year == 2011,2011,NA))) 

## small v large

yield_national_bysize <- yield_disagg %>% 
  filter(Year > 2006) %>%
  filter(!is.na(Category)) %>%
  dplyr::select(-c(District,Province,`Agri-season`,`Year(num)`)) %>%
  group_by(Year,Category) %>%
  mutate_all(as.numeric) %>%
  summarise_all(~sum(.x,na.rm=T)) %>%
  mutate(yield_national = `Expected Production (MT)` / `Area Planted (Ha)`,
         fertilizer_national = (`Fertiliser Basal (MT)` + `Fertiliser Top (MT)`) / `Area Planted (Ha)`  ) %>%
  mutate(Year = as.numeric(Year))

yield_province_bysize <- yield_disagg %>% 
  filter(Year > 2006) %>%
  filter(!is.na(Category)) %>%
  dplyr::select(-c(District,`Agri-season`,`Year(num)`)) %>%
  group_by(Year,Category,Province) %>%
  mutate_all(as.numeric) %>%
  summarise_all(~sum(.x,na.rm=T)) %>%
  mutate(yield_national = `Expected Production (MT)` / `Area Planted (Ha)`,
         fertilizer_national = (`Fertiliser Basal (MT)` + `Fertiliser Top (MT)`) / `Area Planted (Ha)`  ) %>%
  mutate(Year = as.numeric(Year))

### plot

## yield trend

pres_plot_1 <- ggplot(yield_national, aes(x = Year, y = yield_national)) +
  geom_point()+ geom_line() +
  geom_smooth( method = lm, color ='red', data = yield_national[ as.numeric(yield_national$Year)< 2011,])+
  geom_smooth( method = lm, color ='red', data = yield_national[ as.numeric(yield_national$Year) >=2011,])+
  geom_vline(aes(xintercept=policy_indicator,color=factor(policy_indicator)),linetype='dashed') +
  scale_color_manual(labels=c("FISP Introduced","FISP Scaled Up"), 
                     values = c("darkgreen","blue"),name="Policy Timing",na.translate=FALSE) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = 'top') +
  ylab("Yield (mt/ha)") +
  labs(title = "National Maize Yield over Time") 


ggplot(yield_province, aes(x = YEAR, y = yield)) +
  geom_point()+ geom_line() +
  facet_wrap(~province) +
  geom_smooth( method = lm, color ='red', data = yield_province[ as.numeric(yield_province$YEAR)< 2003,])+
  geom_smooth( method = lm, color ='red', data = yield_province[ as.numeric(yield_province$YEAR) > 2003 &  as.numeric(yield_province$YEAR) < 2011 ,])+
  geom_smooth( method = lm, color ='red', data = yield_province[ as.numeric(yield_province$YEAR) >=2011,])+
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 2003, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(1987, 2020, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Aggregated Yield Rate over Time (Provincial)") 

# disaggregated by small vs large

plot_yield <- ggplot(yield_national_bysize, aes(x = Year, y = yield_national, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Aggregated Yield Rate over Time by Producer Size") 

plot_yield_province <- ggplot(yield_province_bysize, aes(x = Year, y = yield_national, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Aggregated Yield Rate over Time by Producer Size") +
  facet_wrap(~Province)


## fertilizer usage trend

pres_plot_2 <- ggplot(yield_national %>% filter(Year >= 2000 & Year != 2020) %>% mutate(fertilizer_national = ifelse(Year < 2007, NA,fertilizer_national)), aes(x = Year, y = fertilizer_national)) +
  geom_point()+ geom_line() +
  geom_smooth( method = lm, color ='red', data = yield_national[ as.numeric(yield_national$Year)< 2011 & as.numeric(yield_national$Year) > 2006,])+
  geom_smooth( method = lm, color ='red', data = yield_national[ as.numeric(yield_national$Year) >=2011 & as.numeric(yield_national$Year) !=2020,])+
  geom_vline(aes(xintercept=policy_indicator,color=factor(policy_indicator)),linetype='dashed') +
  scale_color_manual(labels=c("FISP Introduced","FISP Scaled Up"), 
                     values = c("darkgreen","blue"),name="Policy Timing",na.translate=FALSE) +  scale_x_continuous( breaks = seq(2000, 2022, by = 5)) +
  theme_bw() +
  theme(legend.position = "none")+
  ylab("Fertilizer usage (mt/ha)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Fertilizer Usage Rate over Time") 


# disaggregated by small vs large

plot_fertilizer <- ggplot(yield_national_bysize %>% filter(Year >= 2007 & Year != 2020), aes(x = Year, y = fertilizer_national, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Fertilizer Usage Rate over Time by Producer Size") 

plot_fertilizer_province <- ggplot(yield_province_bysize %>% filter(Year >= 2007 & Year != 2020), aes(x = Year, y = fertilizer_national, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Fertilizer Usage Rate over Time by Producer Size") +
  facet_wrap(~Province)

plot_fertilizer_abs_province <- ggplot(yield_province_bysize %>% filter(Year >= 2007 & Year != 2020), aes(x = Year, y = `Fertiliser Basal (MT)`, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Fertilizer Usage over Time by Producer Size") +
  facet_wrap(~Province)

## land cultivation size trend

ggplot(yield_national, aes(x = Year, y = `Area Planted (Ha)`)) +
  geom_point()+ geom_line() +
  geom_smooth( method = lm, color ='red', data = yield_national[ as.numeric(yield_national$Year)< 2011,])+
  geom_smooth( method = lm, color ='red', data = yield_national[ as.numeric(yield_national$Year) >=2011,])+
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 2003, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Overall Planted Area over Time") 

# disaggregated by small vs large

ggplot(yield_national_bysize, aes(x = Year, y = `Area Planted (Ha)`, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Overall Planted Area over Time by Producer Size") 

ggplot(yield_province_bysize, aes(x = Year, y = `Area Planted (Ha)`, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Overall Planted Area over Time by Producer Size") +
  facet_wrap(~Province)

## production trend

ggplot(yield_national, aes(x = Year, y = `Expected Production (MT)`)) +
  geom_point()+ geom_line() +
  geom_smooth( method = lm, color ='red', data = yield_national[ as.numeric(yield_national$Year)< 2011,])+
  geom_smooth( method = lm, color ='red', data = yield_national[ as.numeric(yield_national$Year) >=2011,])+
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 2003, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Overall Production over Time") 

# disaggregated by small vs large

plot_production <-  ggplot(yield_national_bysize, aes(x = Year, y = `Expected Production (MT)`, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Overall Production over Time by Producer Size") 

plot_production_province <-  ggplot(yield_province_bysize, aes(x = Year, y = `Expected Production (MT)`, color = Category)) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Overall Production over Time by Producer Size") +
  facet_wrap(~Province)

## small vs large contribution to overall production trend 

share_small <- yield_national_bysize %>% pivot_wider(names_from = "Category", values_from = `Expected Production (MT)`, id_cols = 'Year') %>%
  mutate(share_small = SM / (LS + SM))

ggplot(share_small, aes(x = Year, y = share_small)) +
  geom_point()+ geom_line() +
  geom_smooth( method = lm, color ='red', data = share_small[ as.numeric(share_small$Year)< 2011,])+
  geom_smooth( method = lm, color ='red', data = share_small[ as.numeric(share_small$Year) >=2011,])+
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Smallholder Share of Production over Time") 

## payouts

pres_plot_3 <- ggplot(payouts_national %>% filter(year %in% c(2000:2021)), aes(x = year, y = payout )) +
  geom_point()+ geom_line() +
  geom_vline(aes(xintercept=policy_indicator,color=factor(policy_indicator)),linetype='dashed') +
  scale_color_manual(labels=c("FISP Introduced","FISP Scaled Up"), 
                     values = c("darkgreen","blue"),name="Policy Timing",na.translate=FALSE) +
  scale_x_continuous( breaks = seq(2000, 2021, by = 5)) +
  theme_bw() +
  ylab("Cumulative drought index") +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Met. Drought Severity Over Time") 

ggarrange(pres_plot_1,pres_plot_2,pres_plot_3,ncol=1,common.legend = TRUE)

ggplot(payouts_national %>% filter(year %in% c(1987:2020)), aes(x = year, y = payout )) +
  geom_point()+ geom_line() +
  geom_vline(xintercept = 2003, linetype="dotted", color = "black", size=1) +
  geom_vline(xintercept = 2011, linetype="dotted", color = "black", size=1) +
  scale_x_continuous( breaks = seq(1987, 2020, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Total District Payouts Over Time (Full Series)") 


ggarrange(plot_production,plot_yield,plot_fertilizer,nrow=3,ncol=1,
          common.legend = TRUE, align="v")

## payouts district level regression

payouts <- left_join(payouts,farmer_groups,by="district")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

yield_alltypes <- yield_disagg %>%
  mutate(fertiliser_rate = (as.numeric(`Fertiliser Basal (MT)`) + as.numeric(`Fertiliser Top (MT)`)) / as.numeric(`Area Planted (Ha)`)) %>%
  group_by(`Year(num)`,District) %>%
  summarise_if(is.numeric,~sum(.x,na.rm=T))

payouts_joined <- left_join(payouts,admin2_crosswalk,by=c("district"="district_2022")) %>%
  left_join(wrsi,by=c("year" = "year", "district" = "ADM2_NAME")) %>%
  group_by(year,district_2000) %>%
  summarise(payout = sum(payout,na.rm=T), group = getmode(group), wrsi = mean(wrsi,na.rm=T)) %>%
  filter(year >= 2001 & year <= 2020) %>%
  left_join(yield_alltypes,by=c("district_2000" = "District","year"="Year(num)")) %>%
  rename_at(c(6,7,8),~c("planting","production","yield")) %>%
  mutate(era = ifelse(year %in% c(2001:2006),1,ifelse(year %in% (2007:2011),2,3)))

payouts_joined$index <- c(1:nrow(payouts_joined))

payouts_province <- payouts %>% group_by(year,province) %>%
  summarise(payout = sum(payout,na.rm=T)) %>%
  filter(province != "Muchinga" & year %in% c(1987:2020)) %>%
  left_join(yield_province,by=c("year" = "YEAR","province" = "province"))

payouts_province$index <- c(1:nrow(payouts_province))

payouts_joined <- payouts_joined %>% group_by(district_2000) %>% mutate(wrsi_std = scale(wrsi)[,1]) %>% ungroup()

library(plm)
mod1 <- plm(yield ~ as.numeric(year) + payout,data=payouts_joined
                     ,index=c("district_2000","year"),effect='individual',model="random")
summary(mod1)
mod1_residuals <- data.frame("resid_m1" = as.numeric(mod1$residuals), "index" = as.numeric(names(mod1$residuals)))
payouts_joined <- left_join(payouts_joined,mod1_residuals,by="index")
mod1_resid_trend <- plm(resid_m1 ~ lag(resid_m1,c(1:5)),data=payouts_joined
                        ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod1_resid_trend)
plot(log(mod1$residuals^2))
mean(abs(mod1$residuals))

mod2 <- plm(yield ~ payout,data=payouts_joined
            ,index=c("district_2000","year"),effect='individual',model="fd")
summary(mod2)
mod2_residuals <- data.frame("resid_m2" = as.numeric(mod2$residuals), "index" = as.numeric(names(mod2$residuals)))
payouts_joined <- left_join(payouts_joined,mod2_residuals,by="index")
mod2_resid_trend <- plm(resid_m2 ~ lag(resid_m2,c(1:5)),data=payouts_joined
                        ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod2_resid_trend)


mod3 <- plm(yield ~ factor(group) + payout*factor(era),data=payouts_joined
            ,index=c("district_2000","year"),effect='individual',model="random")
summary(mod3)
mod3_residuals <- data.frame("resid_m3" = as.numeric(mod3$residuals), "index" = as.numeric(names(mod3$residuals)))
payouts_joined <- left_join(payouts_joined,mod3_residuals,by="index")
mod3_resid_trend <- plm(resid_m3 ~ lag(resid_m3,c(1:5)),data=payouts_joined
                        ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod3_resid_trend)
plot(log(mod3$residuals^2))
mean(abs(mod3$residuals))

mod4 <- plm(production ~ as.numeric(year) + payout,data=payouts_joined
            ,index=c("district_2000","year"),effect='individual',model="random")
summary(mod4)
mod4_residuals <- data.frame("resid_m4" = as.numeric(mod4$residuals), "index" = as.numeric(names(mod4$residuals)))
payouts_joined <- left_join(payouts_joined,mod4_residuals,by="index")
mod4_resid_trend <- plm(resid_m4 ~ lag(resid_m4,c(1:5)),data=payouts_joined
                        ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod4_resid_trend)
plot(log(mod4$residuals^2))

mod5 <- plm(production ~ factor(group) + payout*factor(era),data=payouts_joined
            ,index=c("district_2000","year"),effect='individual',model="random")
summary(mod5)
mod5_residuals <- data.frame("resid_m5" = as.numeric(mod5$residuals), "index" = as.numeric(names(mod5$residuals)))
payouts_joined <- left_join(payouts_joined,mod5_residuals,by="index")
mod5_resid_trend <- plm(resid_m5 ~ lag(resid_m5,c(1:5)),data=payouts_joined
                        ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod5_resid_trend)
plot(log(mod5$residuals^2))

mod6 <- plm(planting ~ as.numeric(year) + payout,data=payouts_joined
            ,index=c("district_2000","year"),effect='individual',model="random")
summary(mod6)
mod6_residuals <- data.frame("resid_m6" = as.numeric(mod6$residuals), "index" = as.numeric(names(mod6$residuals)))
payouts_joined <- left_join(payouts_joined,mod6_residuals,by="index")
mod6_resid_trend <- plm(resid_m6 ~ lag(resid_m6,c(1:5)),data=payouts_joined
                        ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod6_resid_trend)
plot(log(mod6$residuals^2))

mod7 <- plm(planting~ factor(group) + payout*factor(era),data=payouts_joined
            ,index=c("district_2000","year"),effect='individual',model="random")
summary(mod7)
mod7_residuals <- data.frame("resid_m7" = as.numeric(mod7$residuals), "index" = as.numeric(names(mod7$residuals)))
payouts_joined <- left_join(payouts_joined,mod7_residuals,by="index")
mod7_resid_trend <- plm(resid_m7 ~ lag(resid_m7,c(1:5)),data=payouts_joined
                        ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod7_resid_trend)
plot(log(mod7$residuals^2))

mod8 <- plm(yield ~ as.numeric(year) + payout + fertiliser_rate,data=payouts_joined %>% filter(year >= 2007 & year != 2020)
            ,index=c("district_2000","year"),effect='individual',model="random")
summary(mod8)
mod8_residuals <- data.frame("resid_m8" = as.numeric(mod8$residuals), "index" = as.numeric(names(mod8$residuals)))
payouts_joined <- left_join(payouts_joined,mod8_residuals,by="index")
mod8_resid_trend <- plm(resid_m8 ~ lag(resid_m8,c(1:5)),data=payouts_joined
                        ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod8_resid_trend)
plot(log(mod8$residuals^2))

mod9 <- plm(yield ~ as.numeric(year) + payout,data=payouts_province
            ,index=c("province","year"),effect='individual',model="random")
summary(mod9)
mod9_residuals <- data.frame("resid_m9" = as.numeric(mod9$residuals), "index" = as.numeric(names(mod9$residuals)))
payouts_province <- left_join(payouts_province,mod9_residuals,by="index")
mod9_resid_trend <- plm(resid_m9 ~ lag(resid_m9,c(1:5)),data=payouts_province
                        ,index=c("province","year"),effect='individual',model="within")
summary(mod9_resid_trend)
plot(log(mod9$residuals^2))

mod10 <- plm(yield ~ payout,data=payouts_province
             ,index=c("province","year"),effect='individual',model="fd")
summary(mod10)
mod10_residuals <- data.frame("resid_m10" = as.numeric(mod10$residuals), "index" = as.numeric(names(mod10$residuals)))
payouts_province <- left_join(payouts_province,mod10_residuals,by="index")
mod10_resid_trend <- plm(resid_m10 ~ lag(resid_m10,c(1:5)),data=payouts_province
                         ,index=c("province","year"),effect='individual',model="within")
summary(mod10_resid_trend)

mod11 <- plm(yield ~ as.numeric(year) + wrsi_std,data=payouts_joined
             ,index=c("district_2000","year"),effect='individual',model="random")
summary(mod11)
mod11_residuals <- data.frame("resid_m11" = as.numeric(mod11$residuals), "index" = as.numeric(names(mod11$residuals)))
payouts_joined <- left_join(payouts_joined,mod11_residuals,by="index")
mod11_resid_trend <- plm(resid_m11 ~ lag(resid_m11,c(1:5)),data=payouts_joined
                         ,index=c("district_2000","year"),effect='individual',model="within")
summary(mod11_resid_trend)

## plot yield and payouts by farmer group, time period

group_2000 <- farmer_groups %>% left_join(admin2_crosswalk,by=c("district" = "district_2022")) %>%
  group_by(district_2000) %>%
  summarise(group = getmode(group))

yield_group <- yield_disagg %>% 
  left_join(group_2000,by=c("District" = "district_2000")) %>%
  dplyr::select(-c(District,Category,Province,`Agri-season`,`Year(num)`)) %>%
  mutate_all(as.numeric) %>%
  group_by(Year,group) %>%
  summarise_all(~sum(.x,na.rm=T)) %>%
  mutate(yield_group = `Expected Production (MT)` / `Area Planted (Ha)`,
         fertilizer_group = (`Fertiliser Basal (MT)` + `Fertiliser Top (MT)`) / `Area Planted (Ha)`  ) %>%
  ungroup() %>%
  filter(Year > 2000 & !is.na(group)) %>% ## for now
  mutate(era = ifelse(Year %in% c(2001:2006),1,ifelse(Year %in% (2007:2011),2,3))) %>%
  mutate(worst_years_dum = ifelse (
    Year %in% c(2002,2019) & group == 1,1,ifelse(
      Year %in% c(2005,2019) & group == 2,1,ifelse(
        Year %in% c(2002,2005,2015,2019) & group == 3,1,NA)
      )
    )) %>%
  mutate(worst_years = ifelse(worst_years_dum == 1,Year,NA))
  
payouts_group <- payouts %>%
  group_by(year,group) %>%
  summarise(payout = sum(payout,na.rm=T)) %>%
  mutate(era = ifelse(year %in% c(2001:2006),1,ifelse(year %in% (2007:2011),2,3))) %>%
  mutate(worst_years_dum = ifelse (
    year %in% c(2002,2019) & group == 1,1,ifelse(
      year %in% c(2005,2019) & group == 2,1,ifelse(
        year %in% c(2002,2005,2015,2019) & group == 3,1,NA)
    )
  )) %>%
  mutate(worst_years = ifelse(worst_years_dum == 1,year,NA))

yield_group_plot <- ggplot(yield_group %>%
                      mutate(group = factor(group,labels = c("South","North","Central"))) 
                             ,aes(x=Year,y=yield_group,color=factor(era,
                          labels=c("Before FISP","During scale-up","After scale-up") ))) +
  geom_point()+ geom_line() +
  geom_vline(aes(xintercept=worst_years),linetype='dashed',color="orange",size=1) +
  facet_wrap(~group) +
  theme_bw() +
  ylab("Yield (mt/ha)") +
  ylim(c(0,3)) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 5)) +
  theme(legend.position = "bottom")+
  labs(color = "Policy era") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ggtitle("Yield (top farmer years shown as dotted lines)")

production_group_plot <- ggplot(yield_group,aes(x=Year,y=`Expected Production (MT)`,color=factor(era))) +
  geom_point()+ geom_line() +
  geom_vline(aes(xintercept=worst_years),linetype='dashed') +
  facet_wrap(~group) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

planting_group_plot <- ggplot(yield_group,aes(x=Year,y=`Area Planted (Ha)`,color=factor(era))) +
  geom_point()+ geom_line() +
  facet_wrap(~group) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

fertilizer_group_plot <- ggplot(yield_group %>% filter(Year >= 2007 & Year != 2020),aes(x=Year,y=fertilizer_group,color=factor(era))) +
  geom_point()+ geom_line() +
  facet_wrap(~group) +
  scale_x_continuous( breaks = seq(2007, 2022, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

payouts_group_plot <- ggplot(payouts_group %>%
        mutate(group = factor(group,labels = c("South","North","Central")))  
                             %>% filter(year %in% c(2001:2022)),aes(x=year,y=payout,color=factor(era,
                              labels=c("Before FISP","During scale-up","After scale-up") ))) +
  geom_point()+ geom_line() +
  geom_vline(aes(xintercept=worst_years),linetype='dashed',color="orange",size=1) +
  facet_wrap(~group) +
  theme_bw() +
  ylab("Drought severity") +
  scale_x_continuous( breaks = seq(2001, 2022, by = 5)) +
  theme(legend.position = "bottom")+
  labs(color = "Policy era") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ggtitle("Met. drought index")

ggarrange(planting_group_plot,production_group_plot,yield_group_plot,payouts_group_plot,nrow=4)
ggarrange(yield_group_plot,payouts_group_plot,common.legend = TRUE, nrow=2)


# predicted yield against actual

group_pred <- payouts_joined %>% ungroup() %>% filter(!is.na(yield)) %>%
  mutate(mod1_pred = as.numeric(predict(mod1)), mod3_pred = as.numeric(predict(mod3))) %>%
  group_by(year,group) %>%
  summarise(mod1_pred = mean(mod1_pred,na.rm=T),mod3_pred = mean(mod3_pred,na.rm=T), payout = mean(payout,na.rm=T), 
            wrsi = mean(wrsi,na.rm = T)) %>%
  mutate(era = ifelse(year %in% c(2001:2006),1,ifelse(year %in% (2007:2011),2,3))) %>%
  mutate(worst_years_dum = ifelse (
    year %in% c(2002,2019) & group == 1,1,ifelse(
      year %in% c(2005,2019) & group == 2,1,ifelse(
        year %in% c(2002,2005,2015,2019) & group == 3,1,NA)
    )
  )) %>%
  mutate(worst_years = ifelse(worst_years_dum == 1,year,NA)) %>%
  left_join(yield_group %>% dplyr::select(Year,group,yield_group),by=c("year" = "Year","group"="group"))

yield_group_predM1_plot <- ggplot(group_pred %>%
                                    mutate(group = factor(group,labels = c("South","North","Central"))) 
                                  ,aes(x=year,y=mod1_pred,color=factor(era))) +
  geom_point()+ geom_line() +
  geom_vline(aes(xintercept=worst_years),linetype='dashed',color="orange",size=1) +
  # geom_hline(yintercept=0,color="purple") +
  theme_bw() +
  ylim(c(0,3)) +
  facet_wrap(~group) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

yield_group_predM3_plot <- ggplot(group_pred %>%
      mutate(group = factor(group,labels = c("South","North","Central"))) 
                                    ,aes(x=year,y=mod3_pred,color=factor(era))) +
  geom_point()+ geom_line() +
  geom_vline(aes(xintercept=worst_years),linetype='dashed',color="orange",size=1) +
  # geom_hline(yintercept=0,color="purple") +
  theme_bw() +
  ylim(c(0,3)) +
  facet_wrap(~group) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 1)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

ggarrange(yield_group_plot,yield_group_predM1_plot,yield_group_predM3_plot,nrow=3)

yield_pred_v_actual <- ggplot(group_pred %>%
                                pivot_longer(c(mod1_pred,mod3_pred,yield_group)) %>%
                                    mutate(group = factor(group,labels = c("South","North","Central"))) 
                                  ,aes(x=year,y=value,color=name,group=factor(era))) +
  geom_point() +
  geom_vline(aes(xintercept=worst_years),linetype='dashed',color="orange",size=1) +
  # geom_hline(yintercept=0,color="purple") +
  theme_bw() +
  ylim(c(0,3)) +
  facet_wrap(~group) +
  scale_x_continuous( breaks = seq(2001, 2022, by = 1)) +
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

ggarrange(yield_pred_v_actual)

yield_percentiles <- group_pred %>%
  group_by(group) %>%
  mutate(m1_pctile = percent_rank(mod1_pred),
         m3_pctile = percent_rank(mod3_pred),
         actual_pctile = percent_rank(yield_group)) %>%
  ungroup()

pctile_plot_1 <- ggplot(yield_percentiles %>% mutate(worst_years_dum = ifelse(is.na(worst_years_dum),1,2)) %>%
         mutate(year_lab = ifelse(worst_years_dum==2,year,NA))
       ,aes(x=m1_pctile,y=actual_pctile, size=worst_years_dum,color=factor(era,
                                  labels=c("Before FISP","During scale-up","After scale-up")))) + 
  scale_size_continuous(range=c(1,3)) +
  geom_point(aes(shape = factor(group,labels=c("South","North","Central") ))) +
  geom_text(aes(label=year_lab),nudge_y=0.05,nudge_x = -0.05) +
  geom_abline(intercept = 0,slope=1,linetype="dashed") +
  theme_bw() +
  xlab("Naive prediction %ile") +
  ylab("Actual yield %ile") +
  guides(size="none",shape="legend",color="legend") +
  labs(color = "Policy era", shape = "Farming region") +
  theme(legend.position = 'bottom')

pctile_plot_2 <- ggplot(yield_percentiles %>% mutate(worst_years_dum = ifelse(is.na(worst_years_dum),1,2)) %>%
         mutate(year_lab = ifelse(worst_years_dum==2,year,NA))
       ,aes(x=m3_pctile,y=actual_pctile, size=worst_years_dum,color=factor(era,
                        labels=c("Before FISP","During scale-up","After scale-up")))) + 
  scale_size_continuous(range=c(1,3)) +
  geom_point(aes(shape = factor(group,labels=c("South","North","Central") ))) +
  geom_text(aes(label=year_lab),nudge_y=0.05,nudge_x = -0.05) +
  geom_abline(intercept = 0,slope=1,linetype="dashed") +
  theme_bw() +
  xlab("Prediction with context %ile") +
  ylab("Actual yield %ile") +
  guides(size="none",shape="legend",color="legend") +
  labs(color = "Policy era",shape="Farming region") +
  theme(legend.position = 'bottom')

ggarrange(pctile_plot_1,pctile_plot_2,common.legend = TRUE, nrow=1)

ggplot(group_pred %>% mutate(era = factor(era,levels=c(1:3),labels=c("Before FISP","During scale-up","After scale-up"))) %>%
         mutate(worst_years_dum = ifelse(is.na(worst_years_dum),1,2)) 
       %>% mutate(year_lab = ifelse(worst_years_dum==2,year,NA)),
       aes(x=payout,y=yield_group,color=factor(group,labels=c("South","North","Central")),group=factor(group))) +
  geom_point(aes(size = worst_years_dum, shape = factor(group))) +
  scale_size_continuous(range=c(1,3)) +
  scale_color_brewer(palette="Set2") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  geom_text(aes(label=year_lab),nudge_y=0.05,nudge_x = -0.05) +
  facet_wrap(~era) +
  theme_bw() +
  ylab("Yield (mt/ha)") +
  xlab("Drought severity index") +
  labs(color = "Farming region") +
  guides(size="none", color = "legend", shape = "none") +
  theme(legend.position = 'bottom')

model_diff <- ggplot(group_pred %>%
         mutate(group = factor(group,labels = c("South","North","Central")))  
         ,aes(x=year,y=mod3_pred-mod1_pred,color=factor(era))) +
  geom_point()+ geom_line() +
  geom_vline(aes(xintercept=worst_years),linetype='dashed',color="orange",size=1) +
  geom_hline(yintercept=0,color="purple") +
  theme_bw() +
  facet_wrap(~group) +
  ylab("Yield difference m1-m2") +
  ggtitle("Model bias without context") +
  scale_x_continuous( breaks = seq(2001, 2022, by = 5)) +
  ylim(c(-0.5,0.5)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

ggarrange(yield_group_plot,payouts_group_plot,nrow=2,align="h")

write.csv(payouts_joined,"time_series_joined.csv")

group_pred %>% ungroup() %>%
  group_by(group) %>% 
  summarise(r1 = cor(x=mod1_pred,y=yield_group,use="complete.obs",method="spearman"),
            r3 = cor(x=mod3_pred,y=yield_group,use="complete.obs",method="spearman"))

payouts_joined %>% ungroup() %>% filter(!is.na(yield)) %>%
  mutate(mod1_pred = as.numeric(predict(mod1)), mod3_pred = as.numeric(predict(mod3))) %>%
  group_by(group) %>% 
  summarise(r1 = cor(x=mod1_pred,y=yield,use="complete.obs",method="spearman"),
            r3 = cor(x=mod3_pred,y=yield,use="complete.obs",method="spearman"))

