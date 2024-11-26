# File: Fuse with additional datasets and structure for final analysis
# Date: 12/12/2023
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","here","readxl")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
df <- read.csv2(here("data", "final", "california_2000_2011.csv"), header = T, sep = ";")
df1 <- read_excel(here("data", "final","ca_zcta_as_change_syr2syr3_27Feb2024.xlsx"))

#1a. clean water data
df1 <- df1 %>% 
  rename_with(tolower) %>%
  select(c(zcta5ce10, final_wtd_as_avg20062011)) %>%
  rename(mom_zipcode = "zcta5ce10") %>%
  rename_all(~str_replace_all(.,"20062011","")) 

#1b. join by values in health data
df <- df %>%
  mutate(mom_zipcode = as.character(mom_zipcode)) %>%
  filter(!is.na(weight) & !is.na(leng_gestation)) %>% #restrict to those with outcomes
  filter(plural == 1)  #restrict to singleton births 

final_df <- left_join(df, df1, by = "mom_zipcode") 

rm(df,df1)

#1c. mutate variables for use in table 1
final_df <- final_df %>%
  mutate(intervention = ifelse(birthdate < ymd("2006-01-23"), "0", 
                               "1"),
         foreign_born = ifelse(mom_birthplace == "Rest of the World" | mom_birthplace == "China" | 
                                 mom_birthplace == "Canada" | mom_birthplace == "Cuba" | 
                                 mom_birthplace == "Guam" | mom_birthplace == "Japan" | 
                                 mom_birthplace == "Mexico" | mom_birthplace == "Philipines" | 
                                 mom_birthplace == "Puerto Rico" | mom_birthplace == "Virgin Islands" | 
                                 mom_birthplace == "Vietnam" & !is.na(mom_birthplace), "Yes", "No"), 
         cali_born = ifelse(mom_birthplace == "California" & !is.na(mom_birthplace), "Yes", "No"),
         pay_prenatal = ifelse(pay_prenatal == 1 | pay_prenatal == 3 | pay_prenatal == 4 | 
                                 pay_prenatal == 6 | pay_prenatal == 8 | pay_prenatal == 10 | 
                                 pay_prenatal == 11 | pay_prenatal == 12, NA, pay_prenatal), #data entry errors
         pay_prenatal = case_when(pay_prenatal == 2 | pay_prenatal == 5 | pay_prenatal == 13 ~ 2, #collapse Medi-Cal and government support categories
                                  TRUE ~ .$pay_prenatal),
         pay_delivery = ifelse(pay_delivery == 1 | pay_delivery == 3 | pay_delivery == 4 | 
                                 pay_delivery == 6 | pay_delivery == 8 | pay_delivery == 10 | 
                                 pay_delivery == 11 | pay_delivery == 12, NA, pay_delivery),
         leng_gestation_wks = leng_gestation / 7, #convert to weeks
         leng_gestation_wks = floor(leng_gestation_wks), #round down by convention
         parity = ifelse(total_liv == "1", "1", "0"),
         mom_age = as.numeric(mom_age)) %>%
  filter(mom_age > 17) %>% #remove adolescent mothers 
  mutate(across(c(mom_multi_race1,fath_multi_race1), as.character), 
         mom_multi_race1 = na_if(mom_multi_race1, "8"),
         fath_multi_race1 = na_if(fath_multi_race1, "8"),
         birthdate = as.Date(birthdate),
         month = strftime(birthdate, "%m"),
         month = as.numeric(month),
         season = case_when(month %in%  9:11 ~ "Fall", 
                            month %in%  c(12, 1, 2)  ~ "Winter", #meteorological season
                            month %in%  3:5  ~ "Spring",
                            TRUE ~ "Summer")) %>%
  mutate(cutoff_as = case_when(final_wtd_as_avg < 1 ~ 1, 
                               between(final_wtd_as_avg, 1, 2) ~ 2, 
                               final_wtd_as_avg > 2 & final_wtd_as_avg <= 5 ~ 3, #arsenic cut-offs
                               between(final_wtd_as_avg, 5, 10) ~ 4, 
                               final_wtd_as_avg > 10 ~ 5,
                               TRUE ~ .$final_wtd_as_avg)) %>%
  select(-c(mom_eth1,mom_eth2,mom_eth3,fath_eth1,fath_eth2,fath_eth3,mom_birthplace,plural,total_liv,month)) 

#Note: total_liv was calculated to be inclusive of this birth, so a parity of 1 is this birth.

#1d. create outcome measures (Aris/Alexander suggested method)
df = final_df %>%
  mutate(weight = as.numeric(weight)) %>%
  filter(leng_gestation_wks > 21 & leng_gestation_wks < 43) %>% #apply Aris et al. (2019) cut-off (22-42 weeks) 
  group_by(ch_sex, leng_gestation_wks) %>%
  mutate(zscore = (weight - median(weight)) / sd(weight)) %>%
  mutate(keep = ifelse(leng_gestation_wks >= 37, 
                       (zscore > -5*sd(zscore) & zscore < 5*sd(zscore)), # greater than 37 weeks
                       (zscore > -4*sd(zscore) & zscore < 3*sd(zscore)))) %>% # less than 37 weeks
  ungroup() %>% 
  filter(keep == TRUE) #exclude low quality gestation lengths and weights 

final_df2 <- left_join(df, final_df)

final_df3 <- final_df2 %>%  
  select(-keep) %>%
  mutate(preterm = ifelse(leng_gestation_wks < 37 & !is.na(leng_gestation_wks), 1, 0),
         low_birthweight = ifelse(weight < 2500 & !is.na(weight), 1, 0),
         verylow_birthweight = ifelse(weight < 1500 & !is.na(weight), 1, 0)) %>%
  filter(!is.na(final_wtd_as_avg)) #restrict to those with arsenic measures 

write.csv2(final_df3, file= here("data","final","final_clean_california_2000_2011.csv"))