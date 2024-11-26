# File: Sensitivity analysis - plural births
# Date: 3/22/2024
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","naniar","here","haven","prais","readxl")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
df <- read.csv2(here("data", "final", "california_2000_2019.csv"), header = T, sep = ";")
df1 <- read_excel(here("data", "final","ca_zcta_as_change_syr2syr3_27Feb2024.xlsx"))

#1a. set up data to be similar to main analysis
#Note: different total numbers because analyzing those who did not have singleton births

df <- df %>%
  mutate(plural = case_when(plural == 1 ~ 0, 
                            between(plural, 2, 8) ~ 1),
         mom_zipcode = as.character(mom_zipcode)) %>%
  filter(birthdate < "2012-01-01" & mom_age > 17 & plural == 1 & 
           !is.na(weight) & !is.na(leng_gestation)) #restrict to 2000-2011, mother age over 17, plural births, those with birthweight, and those who have a length of gestation

df <- df %>%
  mutate(weight = as.numeric(weight),
         leng_gestation_wks = leng_gestation / 7, #convert to weeks
         leng_gestation_wks = floor(leng_gestation_wks)) %>%
  filter(leng_gestation_wks > 21 & leng_gestation_wks < 43) %>% #apply Aris et al. (2019) cut-off (22-42 weeks) 
  group_by(ch_sex, leng_gestation_wks) %>%
  mutate(zscore = (weight - median(weight)) / sd(weight)) %>%
  mutate(keep = ifelse(leng_gestation_wks >= 37, 
                       (zscore > -5*sd(zscore) & zscore < 5*sd(zscore)), # greater than 37 weeks
                       (zscore > -4*sd(zscore) & zscore < 3*sd(zscore)))) %>% # less than 37 weeks
  ungroup() %>% 
  filter(keep == TRUE) #exclude low quality records

#1a. link to water data
df1 <- df1 %>% 
  rename_with(tolower) %>%
  select(c(zcta5ce10, final_wtd_as_avg20062011)) %>%
  rename(mom_zipcode = "zcta5ce10") %>%
  rename_all(~str_replace_all(.,"20062011","")) 

final_df <- left_join(df1, df, by = "mom_zipcode") 

final_df <- final_df %>% #restrict to areas with 2006-2011 measures
  filter(!is.na(X)) 

rm(df,df1)

#1a. set up data for analysis
plural <- final_df %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(plural = sum(plural)/nrow(.)) %>% 
  ungroup() %>%
  select(c(plural, month)) %>%
  unique()

#plural - rates
plural_rates <- plural %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% 
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model1 <- prais_winsten(plural ~ time + intervention + post_intervention,
                        index ='time', 
                        data = plural_rates)
summary(model1)

#CIs
model1$coefficients[4] - 1.96*(5.968e-06) #-1.251016e-05
model1$coefficients[4] + 1.96*(5.968e-06) #1.08844e-05

#per 100K births
(-8.129e-07)*100000 #-0.08129
(-1.251016e-05)*100000 #-1.251016
(1.08844e-05)*100000 #1.08844
