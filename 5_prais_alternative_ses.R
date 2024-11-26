# File: ITS analysis - SES
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

#high_ses - set up data for analysis
preterm_high_ses <- final_df %>%
  filter(pay_prenatal == 6 | pay_prenatal == 7 | pay_prenatal == 8 | pay_prenatal == 9) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for high_ses
preterm_rates_high_ses <- preterm_high_ses %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model1 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_high_ses)
summary(model1)

#CIs
model1$coefficients[4] - 1.96*(2.931e-07) #-2.633807e-06
model1$coefficients[4] + 1.96*(2.931e-07) #-1.484855e-06

#per 100K births
(-2.059e-06)*100000 #-0.2059
(-2.633807e-06)*100000 #-0.2633807
(-1.484855e-06)*100000 #-0.1484855

#residual plot
res <- resid(model1)*100000
plot(fitted(model1), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_private_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model1))
plot(acf, main = "")
# quartz.save("preterm_private_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model1)) 
plot(pacf, main = "")
# quartz.save("preterm_private_pacf.png", dpi = 300)

#Durbin-Watson
summary(model1)
#------------------------------------------------------------------------------
#low ses - set up data for analysis
preterm_low_ses <- final_df %>%
  filter(pay_prenatal == 1 | pay_prenatal == 2 | pay_prenatal == 3 | pay_prenatal == 4 |             
           pay_prenatal == 11 | pay_prenatal == 12) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for low ses
preterm_rates_low_ses <- preterm_low_ses %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model2 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_low_ses)
summary(model2)

#CIs
model2$coefficients[4] - 1.96*(4.043e-07) #-6.941804e-06
model2$coefficients[4] + 1.96*(4.043e-07) #-5.356948e-06 

#per 100K births
(-6.149e-06)*100000 #-0.6149
(-6.941804e-06)*100000 #-0.6941804
(-5.356948e-06)*100000 #-0.5356948

#residual plot
res <- resid(model2)*100000
plot(fitted(model2), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_public_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model2))
plot(acf, main = "")
# quartz.save("preterm_public_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model2)) 
plot(pacf, main = "")
# quartz.save("preterm_public_pacf.png", dpi = 300)

#Durbin-Watson
summary(model2)
#------------------------------------------------------------------------------
#high_ses - low birthweight
low_birthweight_high_ses <- final_df %>%
  filter(pay_prenatal == 6 | pay_prenatal == 7 | pay_prenatal == 8 | pay_prenatal == 9) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for high_ses
low_birthweight_rates_high_ses <- low_birthweight_high_ses %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model3 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_high_ses)
summary(model3)

#CIs
model3$coefficients[4] - 1.96*(1.829e-07) # -1.035451e-06 
model3$coefficients[4] + 1.96*(1.829e-07) # -3.184828e-07

#per 100K births
(-6.770e-07)*100000 #-0.0677
(-1.035451e-06)*100000 #-0.1035451
(-3.184828e-07)*100000 # -0.03184828

#residual plot
res <- resid(model3)*100000
plot(fitted(model3), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_private_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model3))
plot(acf, main = "")
# quartz.save("lowbirth_private_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model3)) 
plot(pacf, main = "")
# quartz.save("lowbirth_private_pacf.png", dpi = 300)

#Durbin-Watson
summary(model3)
#------------------------------------------------------------------------------
#low_ses low birthweight 
low_birthweight_low_ses <- final_df %>%
  filter(pay_prenatal == 1 | pay_prenatal == 2 | pay_prenatal == 3 | pay_prenatal == 4 |            
           pay_prenatal == 11 | pay_prenatal == 12) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for low_ses
low_birthweight_rates_low_ses <- low_birthweight_low_ses %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model4 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_low_ses)
summary(model4)

#CIs
model4$coefficients[4] - 1.96*(3.267e-07) #  -2.633084e-06 
model4$coefficients[4] + 1.96*(3.267e-07) # -1.35242e-06

#per 100K births
(-1.993e-06)*100000 #-0.06985
(-2.633084e-06 )*100000 #-0.1022727
(-1.35242e-06)*100000 # -0.03743585

#residual plot
res <- resid(model4)*100000
plot(fitted(model4), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_public_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model4))
plot(acf, main = "")
# quartz.save("lowbirth_public_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model4)) 
plot(pacf, main = "")
# quartz.save("lowbirth_public_pacf.png", dpi = 300)

#Durbin-Watson
summary(model4)
#------------------------------------------------------------------------------
#high_ses very low birthweight
verylow_birthweight_high_ses <- final_df %>%
  filter(pay_prenatal == 6 | pay_prenatal == 7 | pay_prenatal == 8 | pay_prenatal == 9) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for high_ses
verylow_birthweight_rates_high_ses <- verylow_birthweight_high_ses %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model5 <- prais_winsten(very ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_rates_high_ses)
summary(model5)

#CIs
model5$coefficients[4] - 1.96*(3.980e-08) # -1.637378e-07
model5$coefficients[4] + 1.96*(3.980e-08) # -7.721759e-09  

#per 100K births
(-8.573e-08)*100000 #-0.008573
(-1.637378e-07)*100000 #-0.01637378
(-7.721759e-09 )*100000 # -0.0007721759

#residual plot
res <- resid(model5)*100000
plot(fitted(model5), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_private_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model5))
plot(acf, main = "")
# quartz.save("verylowbirth_private_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model5)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_private_pacf.png", dpi = 300)

#Durbin-Watson
summary(model5)
#------------------------------------------------------------------------------
#low ses very low birthweight
verylow_birthweight_low_ses <- final_df %>%
  filter(pay_prenatal == 1 | pay_prenatal == 2 | pay_prenatal == 3 | pay_prenatal == 4 |    
           pay_prenatal == 11 | pay_prenatal == 12) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for low ses
verylow_birthweight_rates_low_ses <- verylow_birthweight_low_ses %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model6 <- prais_winsten(very ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_rates_low_ses)
summary(model6)

#CIs
model6$coefficients[4] - 1.96*(5.444e-08) # -3.591516e-07 
model6$coefficients[4] + 1.96*(5.444e-08) # -1.457468e-07 

#per 100K births
(-2.525e-07)*100000 #-0.02525
( -3.591516e-07 )*100000 #-0.03591516
(-1.457468e-07 )*100000 #  -0.01457468

#residual plot
res <- resid(model6)*100000
plot(fitted(model6), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_public_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model6))
plot(acf, main = "")
# quartz.save("verylowbirth_public_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model6)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_public_pacf.png", dpi = 300)

#Durbin-Watson
summary(model6)
#------------------------------------------------------------------------------
#high_ses small-for-gestational

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

final_df_high_ses <- final_df %>%
  filter(pay_prenatal == 6 | pay_prenatal == 7 | pay_prenatal == 8 | pay_prenatal == 9) 

#calculate zscores and define weight cut points
small_for_gestational_high_ses = full_join(final_df_high_ses, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
  filter(!is.na(ch_sex)) %>%  # removes 40 observations where lacking sex variable
  mutate(zlms = ((weight / central_tendency)^skewness - 1) / (skewness*dispersion), # zscore calculation
         small_for_gestational = case_when(zlms <= -1.28 ~ 1,
                                           zlms > -1.28 ~ 0),
         birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(small = sum(small_for_gestational)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(small, month)) %>%
  unique() 

#small for gestational - rates high_sess
small_for_gestational_rates_high_ses <- small_for_gestational_high_ses %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model7 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_high_ses)
summary(model7)

#CIs
model7$coefficients[4] - 1.96*(4.668e-07) #-1.711339e-06 
model7$coefficients[4] + 1.96*(4.668e-07) # 1.18517e-07

#per 100K births
(-7.964e-07)*100000 #-0.4476
(-1.711339e-06 )*100000 #-0.6270911
(1.18517e-07)*100000 # -0.2680583

#residual plot
res <- resid(model7)*100000
plot(fitted(model7), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_private_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model7))
plot(acf, main = "")
# quartz.save("sga_private_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model7)) 
plot(pacf, main = "")
# quartz.save("sga_private_pacf.png", dpi = 300)

#Durbin-Watson
summary(model7)
#------------------------------------------------------------------------------
#low_ses small-for-gestational

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

final_df_low_ses <- final_df %>%
  filter(pay_prenatal == 1 | pay_prenatal == 2 | pay_prenatal == 3 | pay_prenatal == 4 |            
           pay_prenatal == 11 | pay_prenatal == 12) 

#calculate zscores and define weight cut points
small_for_gestational_low_ses = full_join(final_df_low_ses, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
  filter(!is.na(ch_sex)) %>%  # removes 40 observations where lacking sex variable
  mutate(zlms = ((weight / central_tendency)^skewness - 1) / (skewness*dispersion), # zscore calculation
         small_for_gestational = case_when(zlms <= -1.28 ~ 1,
                                           zlms > -1.28 ~ 0),
         birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(small = sum(small_for_gestational)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(small, month)) %>%
  unique() 

#small for gestational - rates low_ses
small_for_gestational_rates_low_ses <- small_for_gestational_low_ses %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model8 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_low_ses)
summary(model8)

#CIs
model8$coefficients[4] - 1.96*(9.743e-07) #  -6.063886e-06 
model8$coefficients[4] + 1.96*(9.743e-07) # -2.24463e-06 

#per 100K births
(-4.154e-06)*100000 #-0.4154
(-6.063886e-06 )*100000 #-0.6063886
(-2.24463e-06 )*100000 # -0.224463

#residual plot
res <- resid(model8)*100000
plot(fitted(model8), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_public_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model8))
plot(acf, main = "")
# quartz.save("sga_public_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model8)) 
plot(pacf, main = "")
# quartz.save("sga_public_pacf.png", dpi = 300)

#Durbin-Watson
summary(model8)