#Michelle Chen + additions from Max Mauerman 
#Last updated: July 13, 2023

library(utils)
library(dplyr)
library(readxl)
library(tibble)
library(tidyr)
library(base)
library(datasets)
library(ggplot2)
library(stringr)
library(readr)
library(methods)
library(stats)

setwd("") ## add repository directory here

Org_CFS_Maize_Data <- read_excel("/Data/Yield/CFS Maize Data District Data Compiled.xlsx")
View(Org_CFS_Maize_Data)
# 
# Current_Districts <- read_excel("~/Downloads/Current_Districts.xlsx")
# View(Current_Districts)
#*****************************************
#*Note Do not need to compile this section
#*this is to find the needed alterations for Names of district and provinces 

# Names <-
#   Org_CFS_Maize_Data %>%
#   distinct(Province) %>%
#   arrange(Province)
# 
# District_Names <-
#   Org_CFS_Maize_Data %>%
#   distinct(District) %>%
#   arrange(District)
# 
# Names <-
#   Names %>%
#     mutate(PRovinces = str_replace_all(Province, "-", " ")) %>%
#     mutate(PROvinces = str_to_title(PRovinces))
# 
# District_Names <-
#   District_Names %>%
#   mutate(DIstrict = str_replace_all(District, "-", " ")) %>%
#   mutate(DIStrict = str_remove_all(DIstrict, "'")) %>%
#   mutate(DIStrict = str_to_title(DIStrict))
# str(Org_CFS_Maize_Data)

#************************************************************************************************************************
#PART 1: CLEANING THE DATA
#****************************************
#*fix namings for Category columns

Org_CFS_Maize_Data %>% 
  distinct(Category)
Org_CFS_Maize_Data<-
  Org_CFS_Maize_Data %>% 
  mutate(Category = str_replace_all(Category, "Large Scale", "LS")) %>%
  mutate(Category = str_replace_all(Category, "Small Scale", "SM")) %>% 
  mutate(Category = str_replace_all(Category, "Small Scaleall & Medium", "SM")) %>% 
  mutate(Category = str_replace_all(Category, "SMall & Medium", "SM")) 
#****************************************


#***********************************
#*part 2
#*fixes all the names in provinces and district in actual data set 
#* ALSO NEEED TO FIX THE - IN THE OTHER COLUMNS IE ROW 75
#Org_CFS_Maize_Data <- Org_CFS_Maize_Data[,-c(13,14)]
Org_CFS_Maize_Data <-
  Org_CFS_Maize_Data %>% 
  mutate(`Agri-season` = str_replace_all(`Agri-season`, "2021/22" , "2021/2022")) %>% 
  mutate(`Year(num)` = as.numeric( substr(`Agri-season`, 6,9))) %>% 
  mutate(Year =  substr(`Agri-season`, 6,9)) %>% 
  mutate(District = str_replace_all(District, "-", " ")) %>% 
  mutate(District = str_to_title(District)) %>% 
  mutate(Province = str_replace_all(Province, "-", " ")) %>% 
  mutate(Province = str_to_title(Province)) 

#Special cases
rep_Prov = c('Northwestern' = 'North Western', 'Nothern' = 'Northern') 
Org_CFS_Maize_Data$Province <- str_replace_all(Org_CFS_Maize_Data$Province, rep_Prov) 
rep_Dis = c('Chipata North' = 'Chipata', 'Lusaka Rural' = 'Lusaka', 'Lusaka Urban' = 'Lusaka', 
            'Mufilira'= 'Mufulira', 'Mufurila' ='Mufulira', 'Ndola Rural' ='Ndola', 
            'Ndola Urban' = 'Ndola', 'Kabwe Urban' = 'Kabwe',"Shangombo" = "Shang'ombo" )  
Org_CFS_Maize_Data$District <- str_replace_all(Org_CFS_Maize_Data$District, rep_Dis) 
#*********************************** 

#***********************************
#Recalc the yeild rate, 
Org_CFS_Maize_Data <-
  Org_CFS_Maize_Data %>% 
  mutate(`Yield Rate (MT/Ha)` = `Expected Production (MT)`/`Area Planted (Ha)`) 
#***********************************

#***********************************

#*********************************** 
#* replace all of the 0 with NA in area planted, area to be harvested, etc 
#* Experiment$Score <- replace(Experiment$Score, Experiment$Score == 0, NA)

Org_CFS_Maize_Data$`Area Planted (Ha)` <- replace( Org_CFS_Maize_Data$`Area Planted (Ha)`, Org_CFS_Maize_Data$`Area Planted (Ha)` == 0 |
                                                     Org_CFS_Maize_Data$`Area Planted (Ha)` == '-' | 
                                                     Org_CFS_Maize_Data$`Area Planted (Ha)` == '.', NA)
Org_CFS_Maize_Data$`Area to be Harvested (Ha)` <- replace( Org_CFS_Maize_Data$`Area to be Harvested (Ha)`, Org_CFS_Maize_Data$`Area to be Harvested (Ha)` == 0 |
                                                             Org_CFS_Maize_Data$`Area to be Harvested (Ha)` == '-' |
                                                             Org_CFS_Maize_Data$`Area to be Harvested (Ha)` == '.', NA)
Org_CFS_Maize_Data$`Expected Production (MT)` <- replace( Org_CFS_Maize_Data$`Expected Production (MT)`, Org_CFS_Maize_Data$`Expected Production (MT)` == 0 |
                                                            Org_CFS_Maize_Data$`Expected Production (MT)` == '-' |
                                                            Org_CFS_Maize_Data$`Expected Production (MT)` == '.', NA)
Org_CFS_Maize_Data$`Yield Rate (MT/Ha)` <- replace( Org_CFS_Maize_Data$`Yield Rate (MT/Ha)`, Org_CFS_Maize_Data$`Yield Rate (MT/Ha)` == 0 |
                                                      Org_CFS_Maize_Data$`Yield Rate (MT/Ha)` == '-' |
                                                      Org_CFS_Maize_Data$`Yield Rate (MT/Ha)` == '.' , NA)
Org_CFS_Maize_Data$`Expected Sales (MT)` <- replace( Org_CFS_Maize_Data$`Expected Sales (MT)`, Org_CFS_Maize_Data$`Expected Sales (MT)` == 0 |
                                                       Org_CFS_Maize_Data$`Expected Sales (MT)` == '-' |
                                                       Org_CFS_Maize_Data$`Expected Sales (MT)` == '.', NA)
Org_CFS_Maize_Data$`Fertiliser Basal (MT)` <- replace( Org_CFS_Maize_Data$`Fertiliser Basal (MT)`, Org_CFS_Maize_Data$`Fertiliser Basal (MT)` == 0 |
                                                         Org_CFS_Maize_Data$`Fertiliser Basal (MT)` == '-' |
                                                         Org_CFS_Maize_Data$`Fertiliser Basal (MT)` == '.', NA)
Org_CFS_Maize_Data$`Fertiliser Top (MT)` <- replace( Org_CFS_Maize_Data$`Fertiliser Top (MT)`, Org_CFS_Maize_Data$`Fertiliser Top (MT)` == 0 |
                                                       Org_CFS_Maize_Data$`Fertiliser Top (MT)` == '-' |
                                                       Org_CFS_Maize_Data$`Fertiliser Top (MT)` == '.', NA)
#***********************************
# recalculates the columns with combining the small and large scales 
# fit a trend line for before 2011 and post 2011
# also adding a error bar in the trendline 
Combined_CFS_Maize_Data <-
  Org_CFS_Maize_Data %>% 
  group_by(District, `Agri-season`) %>% 
  summarize(Province = Province, Year = Year,
            `Area Planted (Ha)` = sum(`Area Planted (Ha)`),
            `Area to be Harvested (Ha)` = sum(as.numeric(`Area to be Harvested (Ha)`)),
            `Expected Production (MT)`  = sum(`Expected Production (MT)`),
            `Yield Rate (MT/Ha)` = `Expected Production (MT)`/`Area Planted (Ha)`,
            `Expected Sales (MT)` = sum(as.numeric(`Expected Sales (MT)`)),
            `Fertiliser Basal (MT)` = sum(as.numeric(`Fertiliser Basal (MT)`)),
            `Fertiliser Top (MT)` = sum(as.numeric(`Fertiliser Top (MT)`)))%>% 
  ungroup()

#***********************************

#************************************************************************************************************************
#PART 2: PLOTTING THE MISSING DATA
#***********************************
#*Find the number of N/A in a column by years and plots it  
Synthesis_NA <-
  Org_CFS_Maize_Data %>% 
  group_by(`Agri-season`) %>% 
  summarize(`Area Planted (Ha)` = sum(is.na(`Area Planted (Ha)`)), 
            `Area to be Harvested (Ha)` = sum(is.na(`Area to be Harvested (Ha)`)),
            `Expected Production (MT)` = sum(is.na(`Expected Production (MT)`)),
            `Expected Sales (MT)` = sum(is.na(`Expected Sales (MT)`)),
            `Yield Rate (MT/Ha)` = sum(is.na(`Yield Rate (MT/Ha)`)),
            `Fertiliser Basal (MT)` = sum(is.na(`Fertiliser Basal (MT)`)),
            `Fertiliser Top (MT)`= sum(is.na(`Fertiliser Top (MT)`)),
            `Category`= sum(is.na(`Category`))) %>% 
  ungroup()
Synthesis_NA <-
  Synthesis_NA %>% 
  group_by(`Agri-season`) %>% 
  mutate(Num_NA = sum(`Area Planted (Ha)`, `Expected Production (MT)`,
                      `Expected Sales (MT)`,`Area to be Harvested (Ha)`,
                      `Yield Rate (MT/Ha)`, `Fertiliser Basal (MT)`,
                      `Fertiliser Top (MT)`,`Category`)) %>% 
  ungroup()
# Stacked bar graph of number of missing values by year 
Bar_graph_by_year<-
  Synthesis_NA %>% 
  pivot_longer(cols = c(`Area Planted (Ha)`,`Area to be Harvested (Ha)`, `Expected Production (MT)`,
                        `Yield Rate (MT/Ha)`, `Expected Sales (MT)`,`Fertiliser Basal (MT)`, `Fertiliser Top (MT)`,`Category`),
               names_to = 'variables',
               values_to = 'values')
Bar_graph_by_year %>% 
  group_by(`Agri-season`, values) %>% 
  ggplot(aes(x= `Agri-season`, y = values, group = variables, fill = variables)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# do the same for combined version of the data
Synthesis_NA <-
  Combined_CFS_Maize_Data %>% 
  group_by(`Agri-season`) %>% 
  summarize(`Area Planted (Ha)` = sum(is.na(`Area Planted (Ha)`)), 
            `Area to be Harvested (Ha)` = sum(is.na(`Area to be Harvested (Ha)`)),
            `Expected Production (MT)` = sum(is.na(`Expected Production (MT)`)),
            `Expected Sales (MT)` = sum(is.na(`Expected Sales (MT)`)),
            `Yield Rate (MT/Ha)` = sum(is.na(`Yield Rate (MT/Ha)`)),
            `Fertiliser Basal (MT)` = sum(is.na(`Fertiliser Basal (MT)`)),
            `Fertiliser Top (MT)`= sum(is.na(`Fertiliser Top (MT)`))) %>% 
  ungroup()
Synthesis_NA <-
  Synthesis_NA %>% 
  group_by(`Agri-season`) %>% 
  mutate(Num_NA = sum(`Area Planted (Ha)`, `Expected Production (MT)`,
                      `Expected Sales (MT)`,`Area to be Harvested (Ha)`,
                      `Yield Rate (MT/Ha)`, `Fertiliser Basal (MT)`,
                      `Fertiliser Top (MT)`)) %>% 
  ungroup()
# Stacked bar graph of number of missing values by year 
Bar_graph_by_year<-
  Synthesis_NA %>% 
  pivot_longer(cols = c(`Area Planted (Ha)`,`Area to be Harvested (Ha)`, `Expected Production (MT)`,
                        `Yield Rate (MT/Ha)`, `Expected Sales (MT)`,`Fertiliser Basal (MT)`, `Fertiliser Top (MT)`),
               names_to = 'variables',
               values_to = 'values')
Bar_graph_by_year %>% 
  group_by(`Agri-season`, values) %>% 
  ggplot(aes(x= `Agri-season`, y = values, group = variables, fill = variables)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# tally missingness by category 
Synthesis_NA <-
  Org_CFS_Maize_Data %>% 
  group_by(`Category`) %>% 
  summarize(`Area Planted (Ha)` = sum(is.na(`Area Planted (Ha)`)), 
            `Area to be Harvested (Ha)` = sum(is.na(`Area to be Harvested (Ha)`)),
            `Expected Production (MT)` = sum(is.na(`Expected Production (MT)`)),
            `Expected Sales (MT)` = sum(is.na(`Expected Sales (MT)`)),
            `Yield Rate (MT/Ha)` = sum(is.na(`Yield Rate (MT/Ha)`)),
            `Fertiliser Basal (MT)` = sum(is.na(`Fertiliser Basal (MT)`)),
            `Fertiliser Top (MT)`= sum(is.na(`Fertiliser Top (MT)`))) %>% 
  ungroup()
Synthesis_NA <-
  Synthesis_NA %>% 
  group_by(`Category`) %>% 
  mutate(Num_NA = sum(`Area Planted (Ha)`, `Expected Production (MT)`,
                      `Expected Sales (MT)`,`Area to be Harvested (Ha)`,
                      `Yield Rate (MT/Ha)`, `Fertiliser Basal (MT)`,
                      `Fertiliser Top (MT)`)) %>% 
  ungroup()
# Stacked bar graph of number of missing values by year 
Bar_graph_by_year<-
  Synthesis_NA %>% 
  pivot_longer(cols = c(`Area Planted (Ha)`,`Area to be Harvested (Ha)`, `Expected Production (MT)`,
                        `Yield Rate (MT/Ha)`, `Expected Sales (MT)`,`Fertiliser Basal (MT)`, `Fertiliser Top (MT)`),
               names_to = 'variables',
               values_to = 'values')
Bar_graph_by_year %>% 
  group_by(`Category`,values) %>% 
  ggplot(aes(x= `Category`, y = values, group = variables, fill = variables)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#***********************************


#***********************************
#* Given the districts and the year with missing data,
#* extrapolate the year with the most number of missing data by districts 
#* 
District_level <-
  Org_CFS_Maize_Data %>% 
  group_by(District,`Agri-season`) %>% 
  summarize(`Area Planted (Ha)` = sum(is.na(`Area Planted (Ha)`)),
            `Area to be Harvested (Ha)` = sum(is.na(`Area to be Harvested (Ha)`)),
            `Expected Production (MT)` = sum(is.na(`Expected Production (MT)`)),
            `Expected Sales (MT)` = sum(is.na(`Expected Sales (MT)`)),
            `Yield Rate (MT/Ha)` = sum(is.na(`Yield Rate (MT/Ha)`)),
            `Fertiliser Basal (MT)` = sum(is.na(`Fertiliser Basal (MT)`)),
            `Fertiliser Top (MT)`= sum(is.na(`Fertiliser Top (MT)`)),
            `Category`= sum(is.na(`Category`))) %>% 
  mutate(Num_Missing_District = sum( `Expected Production (MT)`,
                                     `Expected Sales (MT)`,`Area to be Harvested (Ha)`,
                                     `Yield Rate (MT/Ha)`, `Fertiliser Basal (MT)`,
                                     `Fertiliser Top (MT)`,`Category`)) %>% 
  ungroup()
District_level<- 
  District_level %>%
  group_by(District,`Agri-season`) %>% 
  mutate(Num_Missing_Data = sum( `Expected Production (MT)`, `Area Planted (Ha)`,
                                 `Expected Sales (MT)`,`Area to be Harvested (Ha)`,
                                 `Yield Rate (MT/Ha)`, `Fertiliser Basal (MT)`,
                                 `Fertiliser Top (MT)`,`Category`)) %>% 
  ungroup()

#pivot_longer adding the variabe  of cloles to rows 
#****************************

#***********************************
#* pivot the variables, Stacked bar graph of of the number of missing data vs district  
#*
Bar_graph<-
  District_level %>% 
  pivot_longer(cols = c(`Area Planted (Ha)`,`Area to be Harvested (Ha)`, `Expected Production (MT)`,
                        `Yield Rate (MT/Ha)`, `Expected Sales (MT)`,`Fertiliser Basal (MT)`, `Fertiliser Top (MT)`,`Category`),
               names_to = 'variables',
               values_to = 'values')
Bar_graph %>% 
  group_by(District, values) %>% 
  ggplot(aes(x= District, y = values, group = variables, fill = variables)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#***********************************

# write files for later use

library(writexl)
write_xlsx(Org_CFS_Maize_Data,"/Data/Yield/district_maize_disaggregate_cleaned.xlsx")
write_xlsx(Combined_CFS_Maize_Data,"/Data/Yield/district_maize_combined_cleaned.xlsx")
