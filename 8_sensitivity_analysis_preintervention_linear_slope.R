# File: ITS analysis using pre-intervention linear slope
# Date: 10/7/2024
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","naniar","here","haven","prais")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
final_df <- read.csv2(here("data", "final", "final_clean_california_2000_2011.csv"), header = T, sep = ";")

#set up data for analysis
preterm <- final_df %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates
preterm_rates <- preterm %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() %>% 
  filter(intervention == 0) #based on pre-intervention trend

model1 <- lm(preterm ~ month, data = preterm_rates)

summary(model1)

#CIs
model1$coefficients[2] - 1.96*(6.550e-09) #3.089841e-08
model1$coefficients[2] + 1.96*(6.550e-09) #5.657441e-08 

#per 100K births
(4.374e-08)*100000 #0.004374
(3.089841e-08)*100000 #0.003089841
(5.657441e-08 )*100000 #0.005657441

#------------------------------------------------------------------------------
#low birthweight
low_birthweight <- final_df %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates
low_birthweight_rates <- low_birthweight %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() %>% 
  filter(intervention == 0) #based on pre-intervention trend

model2 <- lm(low ~ month, data = low_birthweight_rates)

summary(model2)

#CIs
model2$coefficients[2] - 1.96*(2.913e-09) #2.290853e-08
model2$coefficients[2] + 1.96*(2.913e-09) #3.432749e-08

#per 100K births
(2.862e-08)*100000 #0.002862
(2.290853e-08)*100000 #0.002290853
(3.432749e-08)*100000 #0.003432749

#------------------------------------------------------------------------------
#Very low birthweight
verylow_birthweight <- final_df %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>%  
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates
verylow_birthweight_rates <- verylow_birthweight %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() %>% 
  filter(intervention == 0) #based on pre-intervention trend

model3 <- lm(very ~ month, data = verylow_birthweight_rates)

summary(model3)

#CIs
model3$coefficients[2] - 1.96*(6.257e-10) #2.549381e-09
model3$coefficients[2] + 1.96*(6.257e-10) #5.002125e-09

#per 100K births
(3.776e-09)*100000 #0.0003776
(2.549381e-09)*100000 #0.0002549381
(5.002125e-09)*100000 #0.0005002125

#------------------------------------------------------------------------------
#Small-for-gestational

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
small_for_gestational = full_join(final_df, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
  filter(!is.na(ch_sex)) %>%  # removes 40 observations where lacking sex variable
  mutate(zlms = ((weight / central_tendency)^skewness - 1) / (skewness*dispersion), # zscore calculation
         small_for_gestational = case_when(zlms <= -1.28 ~ 1,
                                           zlms > -1.28 ~ 0),
         birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(small = sum(small_for_gestational)/nrow(.)) %>% 
  ungroup() %>%
  select(c(small, month)) %>%
  unique() 

#small for gestational - rates
small_for_gestational_rates <- small_for_gestational %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() %>% 
  filter(intervention == 0) #based on pre-intervention trend

model4 <- lm(small ~ month, data = small_for_gestational_rates)

summary(model4)

#CIs
model4$coefficients[2] - 1.96*(7.471e-09) #4.309382e-08 
model4$coefficients[2] + 1.96*(7.471e-09) #7.238014e-08

#per 100K births
(5.774e-08)*100000 #0.005774
(4.309382e-08)*100000 #0.004309382
(7.238014e-08)*100000 #0.007238014

