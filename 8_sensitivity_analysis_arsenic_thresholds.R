# File: Fuse with additional datasets and structure for zip codes that reduced arsenic by threshold
# Date: 10/7/2024
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","naniar","here","haven","prais","readxl")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
df1 <- read.csv2(here("data", "final", "final_clean_california_2000_2011.csv"), header = T, sep = ";")
df2 <- read_excel(here("data", "final","ca_zcta_as_change_syr2syr3_27Feb2024.xlsx")) #pre-intervention arsenic levels

#1a. clean water data
df2 <- df2 %>%
  rename_with(tolower) %>%
  select(c(zcta5ce10, wtd_as_avg20002005,final_wtd_as_avg20092011)) %>%
  rename(mom_zipcode = "zcta5ce10") %>%
  mutate(mom_zipcode = as.integer(mom_zipcode))

final_df <- left_join(df2, df1, by = "mom_zipcode") 

final_df <- final_df %>%
  filter(!is.na(wtd_as_avg20002005) & !is.na(final_wtd_as_avg20092011)) %>% 
  mutate(lowered_as_1 = ifelse(wtd_as_avg20002005 > 1 & final_wtd_as_avg20092011 <= 1, 1, 0),
         lowered_as_2 = ifelse(wtd_as_avg20002005 > 2 & final_wtd_as_avg20092011 <= 2, 1, 0),
         lowered_as_5 = ifelse(wtd_as_avg20002005 > 5 & final_wtd_as_avg20092011 <= 5, 1, 0)) %>%
  select(c(mom_zipcode,lowered_as_1,lowered_as_2,lowered_as_5)) %>%
  unique()

#1b. join by values in health data
final_df <- left_join(df1, final_df, by = "mom_zipcode") 

rm(df2,df1)

#1b. set up data for analysis
#Lower 5
preterm_lower_5 <- final_df %>%
  filter(lowered_as_5 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique() 

#preterm - rates
preterm_lower_5_rates <- preterm_lower_5 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model1 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_lower_5_rates)
summary(model1)

#CIs
model1$coefficients[4] - 1.96*(8.568e-07) #-8.877695e-06 
model1$coefficients[4] + 1.96*(8.568e-07) #-5.519039e-06

#per 100K births
(-7.198e-06)*100000 #-0.7198
(-8.877695e-06)*100000 #-0.8877695
(-5.519039e-06)*100000 #-0.5519039

rm(model1, preterm_lower_5, preterm_lower_5_rates)

#----------------------------------
low_birthweight_lower_5 <- final_df %>%
  filter(lowered_as_5 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low_birthweight = sum(low_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(low_birthweight, month)) %>%
  unique() 

#low birthweight - rates
low_birthweight_lower_5_rates <- low_birthweight_lower_5 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model2 <- prais_winsten(low_birthweight ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_lower_5_rates)
summary(model2)

#CIs
model2$coefficients[4] - 1.96*(7.202e-07) #-4.138135e-06
model2$coefficients[4] + 1.96*(7.202e-07) #-1.314951e-06

#per 100K births
(-2.727e-06)*100000 #-0.2727
(-4.138135e-06)*100000 #-0.4138135
(-1.314951e-06)*100000 #-0.1314951

rm(model2, low_birthweight_lower_5, low_birthweight_lower_5_rates)

#----------------------------------
verylow_birthweight_lower_5 <- final_df %>%
  filter(lowered_as_5 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(verylow_birthweight = sum(verylow_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(verylow_birthweight, month)) %>%
  unique() 

#low birthweight - rates
verylow_birthweight_lower_5_rates <- verylow_birthweight_lower_5 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model3 <- prais_winsten(verylow_birthweight ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_lower_5_rates)
summary(model3)

#CIs
model3$coefficients[4] - 1.96*(3.004e-07) #-1.362996e-06
model3$coefficients[4] + 1.96*(3.004e-07) #-1.854282e-07

#per 100K births
(-7.742e-07)*100000 #-0.07742
(-1.362996e-06)*100000 #-0.1362996
(-1.854282e-07)*100000 #-0.01854282

rm(model3, verylow_birthweight_lower_5, verylow_birthweight_lower_5_rates)

#-------------------------------
#small-for-gestational age - male
lms_ref_male =
  tibble(ch_sex = 1,
         leng_gestation_wks = 22:42,
         skewness = c(1.362, 1.435, 1.509, 1.559, 1.554, 1.484, 1.368, 1.238, 1.124, 1.041, 0.980,
                      0.934, 0.905, 0.872, 0.712, 0.486, 0.346, 0.355, 0.434, 0.498, 0.593),
         central_tendency = c(494.570, 595.591, 681.817, 777.477, 886.921, 1008.568, 1142.504, 1290.461,
                              1465.963, 1660.905, 1855.855, 2081.769, 2328.236, 2586.847, 2840.807, 3057.622,
                              3272.907, 3461.522, 3572.973, 3686.785, 3797.275),
         dispersion = c(0.142, 0.153, 0.168, 0.183, 0.196, 0.206, 0.209, 0.204, 0.196, 0.189,
                        0.182, 0.173, 0.163, 0.152, 0.144, 0.135, 0.125, 0.117, 0.112, 0.111, 0.116))

#small-for-gestational age - female
lms_ref_female =
  tibble(ch_sex = 2,
         leng_gestation_wks = 22:42,
         skewness = c(0.868, 1.061, 1.181, 1.207, 1.174, 1.123, 1.067, 1.001, 0.916, 0.818,
                      0.720, 0.659, 0.674, 0.680, 0.546, 0.369, 0.251, 0.232, 0.293, 0.364, 0.477),
         central_tendency = c(469.074, 552.366, 633.758, 725.641, 823.942, 937.473, 1062.797, 1208.956,
                              1375.672, 1555.810, 1758.998, 1978.934, 2225.948, 2487.121, 2731.347,
                              2936.346, 3143.923, 3324.318, 3436.793, 3546.078, 3623.167),
         dispersion = c(0.137, 0.156, 0.180, 0.200, 0.212, 0.216, 0.217, 0.213, 0.205, 0.196,
                        0.188, 0.177, 0.164, 0.153, 0.147, 0.139, 0.126, 0.117, 0.111, 0.110, 0.118))

lms_ref = rbind(lms_ref_male, lms_ref_female)

#calculate zscores and define weight cut points
small_for_gestational_lower_5 = full_join(final_df, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
  filter(!is.na(ch_sex)) %>%  # removes 40 observations where lacking sex variable
  mutate(zlms = ((weight / central_tendency)^skewness - 1) / (skewness*dispersion), # zscore calculation
         small_for_gestational = case_when(zlms <= -1.28 ~ 1,
                                           zlms > -1.28 ~ 0),
         birthdate = as.Date(birthdate)) %>%
  filter(lowered_as_5 == 1) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(small = sum(small_for_gestational)/nrow(.)) %>%  
  ungroup() %>%
  select(c(small, month)) %>%
  unique() 

#small for gestational - rates
small_for_gestational_rates_lower_5 <- small_for_gestational_lower_5 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model4 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_lower_5)
summary(model4)

#CIs
model4$coefficients[4] - 1.96*(1.070e-06) #-6.938243e-06
model4$coefficients[4] + 1.96*(1.070e-06) #-2.743843e-06

#per 100K births
(-4.841e-06)*100000 #-0.4841
(-6.938243e-06)*100000 #-0.6938243
(-2.743843e-06)*100000 #-0.2743843

rm(model4, small_for_gestational_lower_5, small_for_gestational_rates_lower_5,
   lms_ref_male,lms_ref_female,lms_ref)

#----------------------------------------
#Lower 2
preterm_lower_2 <- final_df %>%
  filter(lowered_as_2 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique() 

#preterm - rates
preterm_lower_2_rates <- preterm_lower_2 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model1 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_lower_2_rates)
summary(model1)

#CIs
model1$coefficients[4] - 1.96*(3.945e-07) #-4.87521e-06 
model1$coefficients[4] + 1.96*(3.945e-07) #-3.32877e-06

#per 100K births
(-4.102e-06)*100000 #-0.4102
(-4.87521e-06 )*100000 #-0.487521
(-3.32877e-06)*100000 #-0.332877

rm(model1, preterm_lower_2, preterm_lower_2_rates)

#----------------------------------
low_birthweight_lower_2 <- final_df %>%
  filter(lowered_as_2 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low_birthweight = sum(low_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(low_birthweight, month)) %>%
  unique() 

#low birthweight - rates
low_birthweight_lower_2_rates <- low_birthweight_lower_2 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model2 <- prais_winsten(low_birthweight ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_lower_2_rates)
summary(model2)

#CIs
model2$coefficients[4] - 1.96*(2.599e-07) 
model2$coefficients[4] + 1.96*(2.599e-07) 

#per 100K births
(-1.252e-06)*100000 #-0.1252
(-1.761591e-06)*100000 #-0.1761591
(-7.427832e-07)*100000 #-0.07427832

rm(model2, low_birthweight_lower_2, low_birthweight_lower_2_rates)

#----------------------------------
verylow_birthweight_lower_2 <- final_df %>%
  filter(lowered_as_2 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(verylow_birthweight = sum(verylow_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(verylow_birthweight, month)) %>%
  unique() 

#low birthweight - rates
verylow_birthweight_lower_2_rates <- verylow_birthweight_lower_2 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model3 <- prais_winsten(verylow_birthweight ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_lower_2_rates)
summary(model3)

#CIs
model3$coefficients[4] - 1.96*(6.420e-08) #-2.655488e-07
model3$coefficients[4] + 1.96*(6.420e-08) #-1.388478e-08

#per 100K births
(-1.397e-07)*100000 #-0.01397
(-2.655488e-07)*100000 #-0.02655488
(-1.388478e-08)*100000 #-0.001388478

rm(model3, verylow_birthweight_lower_2, verylow_birthweight_lower_2_rates)

#-------------------------------
#small-for-gestational age - male
lms_ref_male =
  tibble(ch_sex = 1,
         leng_gestation_wks = 22:42,
         skewness = c(1.362, 1.435, 1.509, 1.559, 1.554, 1.484, 1.368, 1.238, 1.124, 1.041, 0.980,
                      0.934, 0.905, 0.872, 0.712, 0.486, 0.346, 0.355, 0.434, 0.498, 0.593),
         central_tendency = c(494.570, 595.591, 681.817, 777.477, 886.921, 1008.568, 1142.504, 1290.461,
                              1465.963, 1660.905, 1855.855, 2081.769, 2328.236, 2586.847, 2840.807, 3057.622,
                              3272.907, 3461.522, 3572.973, 3686.785, 3797.275),
         dispersion = c(0.142, 0.153, 0.168, 0.183, 0.196, 0.206, 0.209, 0.204, 0.196, 0.189,
                        0.182, 0.173, 0.163, 0.152, 0.144, 0.135, 0.125, 0.117, 0.112, 0.111, 0.116))

#small-for-gestational age - female
lms_ref_female =
  tibble(ch_sex = 2,
         leng_gestation_wks = 22:42,
         skewness = c(0.868, 1.061, 1.181, 1.207, 1.174, 1.123, 1.067, 1.001, 0.916, 0.818,
                      0.720, 0.659, 0.674, 0.680, 0.546, 0.369, 0.251, 0.232, 0.293, 0.364, 0.477),
         central_tendency = c(469.074, 552.366, 633.758, 725.641, 823.942, 937.473, 1062.797, 1208.956,
                              1375.672, 1555.810, 1758.998, 1978.934, 2225.948, 2487.121, 2731.347,
                              2936.346, 3143.923, 3324.318, 3436.793, 3546.078, 3623.167),
         dispersion = c(0.137, 0.156, 0.180, 0.200, 0.212, 0.216, 0.217, 0.213, 0.205, 0.196,
                        0.188, 0.177, 0.164, 0.153, 0.147, 0.139, 0.126, 0.117, 0.111, 0.110, 0.118))

lms_ref = rbind(lms_ref_male, lms_ref_female)

#calculate zscores and define weight cut points
small_for_gestational_lower_2 = full_join(final_df, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
  filter(!is.na(ch_sex)) %>%  # removes 40 observations where lacking sex variable
  mutate(zlms = ((weight / central_tendency)^skewness - 1) / (skewness*dispersion), # zscore calculation
         small_for_gestational = case_when(zlms <= -1.28 ~ 1,
                                           zlms > -1.28 ~ 0),
         birthdate = as.Date(birthdate)) %>%
  filter(lowered_as_2 == 1) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(small = sum(small_for_gestational)/nrow(.)) %>% 
  ungroup() %>%
  select(c(small, month)) %>%
  unique() 

#small for gestational - rates
small_for_gestational_rates_lower_2 <- small_for_gestational_lower_2 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model4 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_lower_2)
summary(model4)

#CIs
model4$coefficients[4] - 1.96*(7.988e-07) #-4.939545e-06
model4$coefficients[4] + 1.96*(7.988e-07) #-1.808249e-06 

#per 100K births
(-3.374e-06)*100000 #-0.3374
(-4.939545e-06)*100000 #-0.4939545
(-1.808249e-06 )*100000 #-0.1808249

rm(model4, small_for_gestational_lower_2, small_for_gestational_rates_lower_2,
   lms_ref_male,lms_ref_female,lms_ref)
#----------------------------------------
#Lower 1
preterm_lower_1 <- final_df %>%
  filter(lowered_as_1 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique() 

#preterm - rates
preterm_lower_1_rates <- preterm_lower_1 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model1 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_lower_1_rates)
summary(model1)

#CIs
model1$coefficients[4] - 1.96*(3.100e-07) #-4.005487e-06 
model1$coefficients[4] + 1.96*(3.100e-07) #-2.790287e-06

#per 100K births
(-3.398e-06)*100000 #-0.3398
(-4.005487e-06 )*100000 #-0.4005487
(-2.790287e-06)*100000 #-0.2790287

rm(model1, preterm_lower_1, preterm_lower_1_rates)

#----------------------------------
low_birthweight_lower_1 <- final_df %>%
  filter(lowered_as_1 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low_birthweight = sum(low_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(low_birthweight, month)) %>%
  unique() 

#low birthweight - rates
low_birthweight_lower_1_rates <- low_birthweight_lower_1 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model2 <- prais_winsten(low_birthweight ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_lower_1_rates)
summary(model2)

#CIs
model2$coefficients[4] - 1.96*(1.982e-07) #-1.795903e-06 
model2$coefficients[4] + 1.96*(1.982e-07) #-1.018959e-06

#per 100K births
(-1.407e-06)*100000 #-0.1407
(-1.795903e-06)*100000 #-0.1795903
(-1.018959e-06)*100000 #-0.1018959

rm(model2, low_birthweight_lower_1, low_birthweight_lower_1_rates)

#----------------------------------
verylow_birthweight_lower_1 <- final_df %>%
  filter(lowered_as_1 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(verylow_birthweight = sum(verylow_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(verylow_birthweight, month)) %>%
  unique() 

#low birthweight - rates
verylow_birthweight_lower_1_rates <- verylow_birthweight_lower_1 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model3 <- prais_winsten(verylow_birthweight ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_lower_1_rates)
summary(model3)

#CIs
model3$coefficients[4] - 1.96*(4.969e-08) #-2.803975e-07
model3$coefficients[4] + 1.96*(4.969e-08) #-8.561272e-08

#per 100K births
(-1.830e-07)*100000 #-0.0183
(-2.803975e-07)*100000 #-0.02803975
(-8.561272e-08)*100000 #-0.008561272

rm(model3, verylow_birthweight_lower_1, verylow_birthweight_lower_1_rates)

#-------------------------------
#small-for-gestational age - male
lms_ref_male =
  tibble(ch_sex = 1,
         leng_gestation_wks = 22:42,
         skewness = c(1.362, 1.435, 1.509, 1.559, 1.554, 1.484, 1.368, 1.238, 1.124, 1.041, 0.980,
                      0.934, 0.905, 0.872, 0.712, 0.486, 0.346, 0.355, 0.434, 0.498, 0.593),
         central_tendency = c(494.570, 595.591, 681.817, 777.477, 886.921, 1008.568, 1142.504, 1290.461,
                              1465.963, 1660.905, 1855.855, 2081.769, 2328.236, 2586.847, 2840.807, 3057.622,
                              3272.907, 3461.522, 3572.973, 3686.785, 3797.275),
         dispersion = c(0.142, 0.153, 0.168, 0.183, 0.196, 0.206, 0.209, 0.204, 0.196, 0.189,
                        0.182, 0.173, 0.163, 0.152, 0.144, 0.135, 0.125, 0.117, 0.112, 0.111, 0.116))

#small-for-gestational age - female
lms_ref_female =
  tibble(ch_sex = 2,
         leng_gestation_wks = 22:42,
         skewness = c(0.868, 1.061, 1.181, 1.207, 1.174, 1.123, 1.067, 1.001, 0.916, 0.818,
                      0.720, 0.659, 0.674, 0.680, 0.546, 0.369, 0.251, 0.232, 0.293, 0.364, 0.477),
         central_tendency = c(469.074, 552.366, 633.758, 725.641, 823.942, 937.473, 1062.797, 1208.956,
                              1375.672, 1555.810, 1758.998, 1978.934, 2225.948, 2487.121, 2731.347,
                              2936.346, 3143.923, 3324.318, 3436.793, 3546.078, 3623.167),
         dispersion = c(0.137, 0.156, 0.180, 0.200, 0.212, 0.216, 0.217, 0.213, 0.205, 0.196,
                        0.188, 0.177, 0.164, 0.153, 0.147, 0.139, 0.126, 0.117, 0.111, 0.110, 0.118))

lms_ref = rbind(lms_ref_male, lms_ref_female)

#calculate zscores and define weight cut points
small_for_gestational_lower_1 = full_join(final_df, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
  filter(!is.na(ch_sex)) %>%  # removes 40 observations where lacking sex variable
  mutate(zlms = ((weight / central_tendency)^skewness - 1) / (skewness*dispersion), # zscore calculation
         small_for_gestational = case_when(zlms <= -1.28 ~ 1,
                                           zlms > -1.28 ~ 0),
         birthdate = as.Date(birthdate)) %>%
  filter(lowered_as_1 == 1) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(small = sum(small_for_gestational)/nrow(.)) %>%  
  ungroup() %>%
  select(c(small, month)) %>%
  unique() 

#small for gestational - rates
small_for_gestational_rates_lower_1 <- small_for_gestational_lower_1 %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model4 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_lower_1)
summary(model4)

#CIs
model4$coefficients[4] - 1.96*(5.371e-07) #-3.536491e-06
model4$coefficients[4] + 1.96*(5.371e-07) #-1.431059e-06

#per 100K births
(-2.484e-06)*100000 #-0.2484
(-3.536491e-06)*100000 #-0.3536491
(-1.431059e-06)*100000 #-0.1431059

rm(model4, small_for_gestational_lower_1, small_for_gestational_rates_lower_1,
   lms_ref_male,lms_ref_female,lms_ref)
