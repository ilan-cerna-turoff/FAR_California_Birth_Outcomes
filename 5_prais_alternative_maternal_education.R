# File: ITS analysis - maternal education levels
# Date: 7/15/2024
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","naniar","here","haven","prais")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
final_df <- read.csv2(here("data", "final", "final_clean_california_2000_2011.csv"), header = T, sep = ";")

#high_education - set up data for analysis
preterm_high_education <- final_df %>%
  filter(mom_years_edu == 3) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for high_education
preterm_rates_high_education <- preterm_high_education %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model1 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_high_education)
summary(model1)

#CIs
model1$coefficients[4] - 1.96*(1.290e-06) #-7.838453e-06  
model1$coefficients[4] + 1.96*(1.290e-06) #-2.781653e-06

#per 100K births
(-5.310e-06)*100000 #-0.531
(-7.838453e-06 )*100000 #-0.7838453
(-2.781653e-06)*100000 #-0.2781653

#residual plot
res <- resid(model1)*100000
plot(fitted(model1), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_highedu_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model1))
plot(acf, main = "")
# quartz.save("preterm_highedu_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model1)) 
plot(pacf, main = "")
# quartz.save("preterm_highedu_pacf.png", dpi = 300)

#Durbin-Watson
summary(model1)
#------------------------------------------------------------------------------
#low education - set up data for analysis
preterm_low_education <- final_df %>%
  filter(mom_years_edu == 1 | mom_years_edu == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for low education
preterm_rates_low_education <- preterm_low_education %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model2 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_low_education)
summary(model2)

#CIs
model2$coefficients[4] - 1.96*(1.178e-06) #-5.075382e-06
model2$coefficients[4] + 1.96*(1.178e-06) #-4.576217e-07 

#per 100K births
(-2.767e-06)*100000 #-0.2767
(-5.075382e-06)*100000 #-0.5075382
(-4.576217e-07)*100000 #-0.04576217

#residual plot
res <- resid(model2)*100000
plot(fitted(model2), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_loweedu_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model2))
plot(acf, main = "")
# quartz.save("preterm_loweedu_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model2)) 
plot(pacf, main = "")
# quartz.save("preterm_loweedu_pacf.png", dpi = 300)

#Durbin-Watson
summary(model2)
#------------------------------------------------------------------------------
#high_education - low birthweight
low_birthweight_high_education <- final_df %>%
  filter(mom_years_edu == 3) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for high_education
low_birthweight_rates_high_education <- low_birthweight_high_education %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model3 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_high_education)
summary(model3)

#CIs
model3$coefficients[4] - 1.96*(6.968e-07) #  -3.484162e-06
model3$coefficients[4] + 1.96*(6.968e-07) # -7.527057e-07 

#per 100K births
(-2.118e-06)*100000 # -0.2118
(-3.484162e-06)*100000 #-0.3484162
(-7.527057e-07)*100000 # -0.07527057

#residual plot
res <- resid(model3)*100000
plot(fitted(model3), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_highedu_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model3))
plot(acf, main = "")
# quartz.save("lowbirth_highedu_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model3)) 
plot(pacf, main = "")
# quartz.save("lowbirth_highedu_pacf.png", dpi = 300)

#Durbin-Watson
summary(model3)
#------------------------------------------------------------------------------
#low_education - low birthweight 
low_birthweight_low_education <- final_df %>%
  filter(mom_years_edu == 1 | mom_years_edu == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for low_education
low_birthweight_rates_low_education <- low_birthweight_low_education %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model4 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_low_education)
summary(model4)

#CIs
model4$coefficients[4] - 1.96*(7.665e-07) # -2.079052e-06 
model4$coefficients[4] + 1.96*(7.665e-07) # 9.256277e-07

#per 100K births
(-5.767e-07)*100000 #-0.05767
(-2.079052e-06)*100000 #-0.2079052
(9.256277e-07)*100000 # 0.09256277

#residual plot
res <- resid(model4)*100000
plot(fitted(model4), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_lowedu_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model4))
plot(acf, main = "")
# quartz.save("lowbirth_lowedu_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model4)) 
plot(pacf, main = "")
# quartz.save("lowbirth_lowedu_pacf.png", dpi = 300)

#Durbin-Watson
summary(model4)
#------------------------------------------------------------------------------
#high_education very low birthweight
verylow_birthweight_high_education <- final_df %>%
  filter(mom_years_edu == 3) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>%  
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for high_education
verylow_birthweight_rates_high_education <- verylow_birthweight_high_education %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model5 <- prais_winsten(very ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_rates_high_education)
summary(model5)

#CIs
model5$coefficients[4] - 1.96*(9.006e-08) # -4.832546e-07
model5$coefficients[4] + 1.96*(9.006e-08) #  -1.302194e-07

#per 100K births
(-3.067e-07)*100000 #-0.03067
(-4.832546e-07)*100000 #-0.04832546
( -1.302194e-07)*100000 # -0.01302194

#residual plot
res <- resid(model5)*100000
plot(fitted(model5), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_highedu_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model5))
plot(acf, main = "")
# quartz.save("verylowbirth_highedu_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model5)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_highedu_pacf.png", dpi = 300)

#Durbin-Watson
summary(model5)
#------------------------------------------------------------------------------
#low education very low birthweight
verylow_birthweight_low_education <- final_df %>%
  filter(mom_years_edu == 1 | mom_years_edu == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>%  
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for low education
verylow_birthweight_rates_low_education <- verylow_birthweight_low_education %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model6 <- prais_winsten(very ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_rates_low_education)
summary(model6)

#CIs
model6$coefficients[4] - 1.96*(1.059e-07) #  -2.69421e-07 
model6$coefficients[4] + 1.96*(1.059e-07) #1.45707e-07

#per 100K births
(-6.186e-08)*100000 #-0.006186
(-2.69421e-07 )*100000 #-0.0269421
(1.45707e-07)*100000 # 0.0145707

#residual plot
res <- resid(model6)*100000
plot(fitted(model6), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_lowedu_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model6))
plot(acf, main = "")
# quartz.save("verylowbirth_lowedu_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model6)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_lowedu_pacf.png", dpi = 300)

#Durbin-Watson
summary(model6)
#------------------------------------------------------------------------------
#high_education small-for-gestational

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

final_df_high_education <- final_df %>%
  filter(mom_years_edu == 3) 
  
#calculate zscores and define weight cut points
small_for_gestational_high_education = full_join(final_df_high_education, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
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

#small for gestational - rates high_educations
small_for_gestational_rates_high_education <- small_for_gestational_high_education %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model7 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_high_education)
summary(model7)

#CIs
model7$coefficients[4] - 1.96*(1.530e-06) #-6.980688e-06 
model7$coefficients[4] + 1.96*(1.530e-06) # -9.830877e-07 

#per 100K births
(-3.982e-06)*100000 #-0.3982
(-6.980688e-06 )*100000 #-0.6980688
(-9.830877e-07 )*100000 # -0.09830877

#residual plot
res <- resid(model7)*100000
plot(fitted(model7), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_highedu_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model7))
plot(acf, main = "")
# quartz.save("sga_highedu_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model7)) 
plot(pacf, main = "")
# quartz.save("sga_highedu_pacf.png", dpi = 300)

#Durbin-Watson
summary(model7)
#------------------------------------------------------------------------------
#low_education small-for-gestational

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

final_df_low_education <- final_df %>%
  filter(mom_years_edu == 1 | mom_years_edu == 2) 

#calculate zscores and define weight cut points
small_for_gestational_low_education = full_join(final_df_low_education, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
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

#small for gestational - rates high_educations
small_for_gestational_rates_low_education <- small_for_gestational_low_education %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model8 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_low_education)
summary(model8)

#CIs
model8$coefficients[4] - 1.96*(1.711e-06) # -4.388062e-06 
model8$coefficients[4] + 1.96*(1.711e-06) # 2.319058e-06

#per 100K births
(-1.035e-06)*100000 #-0.1035
(-4.388062e-06)*100000 #-0.4388062
(2.319058e-06)*100000 # 0.2319058

#residual plot
res <- resid(model8)*100000
plot(fitted(model8), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_lowedu_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model8))
plot(acf, main = "")
# quartz.save("sga_lowedu_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model8)) 
plot(pacf, main = "")
# quartz.save("sga_lowedu_pacf.png", dpi = 300)

#Durbin-Watson
summary(model8)
