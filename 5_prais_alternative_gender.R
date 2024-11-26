# File: ITS analysis - gender
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

#girls - set up data for analysis
preterm_girls <- final_df %>%
  filter(ch_sex == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for girls
preterm_rates_girls <- preterm_girls %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model1 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_girls)
summary(model1)

#CIs
model1$coefficients[4] - 1.96*(2.750e-07) #-4.769387e-06  
model1$coefficients[4] + 1.96*(2.750e-07) #-3.691387e-06

#per 100K births
(-4.230e-06)*100000 #-0.423
(-4.769387e-06)*100000 #-0.4769387
(-3.691387e-06)*100000 #-0.3691387

#residual plot
res <- resid(model1)*100000
plot(fitted(model1), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_girls_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model1))
plot(acf, main = "")
# quartz.save("preterm_girls_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model1)) 
plot(pacf, main = "")
# quartz.save("preterm_girls_pacf.png", dpi = 300)

#Durbin-Watson
summary(model1)

#------------------------------------------------------------------------------
#boys - set up data for analysis
preterm_boys <- final_df %>%
  filter(ch_sex == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for boys
preterm_rates_boys <- preterm_boys %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model2 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_boys)
summary(model2)

#CIs
model2$coefficients[4] - 1.96*(3.913e-07) #-5.287265e-06  
model2$coefficients[4] + 1.96*(3.913e-07) #-3.753369e-06

#per 100K births
(-4.520e-06)*100000 #-0.452
(-5.287265e-06)*100000 #-0.5287265
(-3.753369e-06)*100000 #-0.3753369

#residual plot
res <- resid(model2)*100000
plot(fitted(model2), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_boys_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model2))
plot(acf, main = "")
# quartz.save("preterm_boys_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model2)) 
plot(pacf, main = "")
# quartz.save("preterm_boys_pacf.png", dpi = 300)

#Durbin-Watson
summary(model2)

#------------------------------------------------------------------------------
#girls - low birthweight
low_birthweight_girls <- final_df %>%
  filter(ch_sex == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for girls
low_birthweight_rates_girls <- low_birthweight_girls %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model3 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_girls)
summary(model3)

#CIs
model3$coefficients[4] - 1.96*(2.308e-07) #-2.016921e-06
model3$coefficients[4] + 1.96*(2.308e-07) # -1.112185e-06

#per 100K births
(-1.565e-06)*100000 #-0.1565
(-2.016921e-06)*100000 #-0.2016921
(-1.112185e-06)*100000 # -0.1112185

#residual plot
res <- resid(model3)*100000
plot(fitted(model3), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_girls_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model3))
plot(acf, main = "")
# quartz.save("lowbirth_girls_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model3)) 
plot(pacf, main = "")
# quartz.save("lowbirth_girls_pacf.png", dpi = 300)

#Durbin-Watson
summary(model3)

#------------------------------------------------------------------------------
#boys low birthweight 
low_birthweight_boys <- final_df %>%
  filter(ch_sex == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for boys
low_birthweight_rates_boys <- low_birthweight_boys %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model4 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_boys)
summary(model4)

#CIs
model4$coefficients[4] - 1.96*(2.363e-07) # -1.877821e-06
model4$coefficients[4] + 1.96*(2.363e-07) # -9.515248e-07

#per 100K births
(-1.415e-06)*100000 #-0.1415
(-1.877821e-06)*100000 #-0.1877821
(-9.515248e-07)*100000 # -0.09515248

#residual plot
res <- resid(model4)*100000
plot(fitted(model4), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_boys_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model4))
plot(acf, main = "")
# quartz.save("lowbirth_boys_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model4)) 
plot(pacf, main = "")
# quartz.save("lowbirth_boys_pacf.png", dpi = 300)

#Durbin-Watson
summary(model4)

#------------------------------------------------------------------------------
#girls very low birthweight
verylow_birthweight_girls <- final_df %>%
  filter(ch_sex == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for girls
verylow_birthweight_rates_girls <- verylow_birthweight_girls %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model5 <- prais_winsten(very ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_rates_girls)
summary(model5)

#CIs
model5$coefficients[4] - 1.96*(4.321e-08) # -3.247066e-07 
model5$coefficients[4] + 1.96*(4.321e-08) # -1.553234e-07

#per 100K births
(-2.400e-07)*100000 #-0.024
(-3.247066e-07)*100000 #-0.03247066
(-1.553234e-07)*100000 # -0.01553234

#residual plot
res <- resid(model5)*100000
plot(fitted(model5), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_girls_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model5))
plot(acf, main = "")
# quartz.save("verylowbirth_girls_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model5)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_girls_pacf.png", dpi = 300)

#Durbin-Watson
summary(model5)

#------------------------------------------------------------------------------
#boys very low birthweight
verylow_birthweight_boys <- final_df %>%
  filter(ch_sex == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>%  
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for boys
verylow_birthweight_rates_boys <- verylow_birthweight_boys %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model6 <- prais_winsten(very ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_rates_boys)
summary(model6)

#CIs
model6$coefficients[4] - 1.96*(5.001e-08) # -2.575007e-07
model6$coefficients[4] + 1.96*(5.001e-08) #-6.146153e-08

#per 100K births
(-1.595e-07)*100000 #-0.01595
(-2.575007e-07)*100000 #-0.02575007
(-6.146153e-08)*100000 # -0.006146153

#residual plot
res <- resid(model6)*100000
plot(fitted(model6), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_boys_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model6))
plot(acf, main = "")
# quartz.save("verylowbirth_boys_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model6)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_boys_pacf.png", dpi = 300)

#Durbin-Watson
summary(model6)

#------------------------------------------------------------------------------
#Girls small-for-gestational

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

final_df_female <- final_df %>% 
  filter(ch_sex == 2)

#calculate zscores and define weight cut points
small_for_gestational_female = full_join(final_df_female, lms_ref_female, by = c("ch_sex", "leng_gestation_wks")) %>% 
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
small_for_gestational_rates_female <- small_for_gestational_female %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model7 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_female)
summary(model7)

#CIs
model7$coefficients[4] - 1.96*(6.745e-07) #-3.878761e-06
model7$coefficients[4] + 1.96*(6.745e-07) # -1.234721e-06 

#per 100K births
(-2.557e-06)*100000 #-0.2738
(-3.878761e-06)*100000 #-0.02743692
(-1.234721e-06 )*100000 # -0.01219204

#residual plot
res <- resid(model7)*100000
plot(fitted(model7), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_girls_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model7))
plot(acf, main = "")
# quartz.save("sga_girls_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model7)) 
plot(pacf, main = "")
# quartz.save("sga_girls_pacf.png", dpi = 300)

#Durbin-Watson
summary(model7)

#------------------------------------------------------------------------------
#Boys small-for-gestational

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

final_df_male <- final_df %>% 
  filter(ch_sex == 1)

#calculate zscores and define weight cut points
small_for_gestational_male = full_join(final_df_male, lms_ref_male, by = c("ch_sex", "leng_gestation_wks")) %>% 
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
small_for_gestational_rates_male <- small_for_gestational_male %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model8 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_male)
summary(model8)

#CIs
model8$coefficients[4] - 1.96*(6.222e-07) #-4.140955e-06 
model8$coefficients[4] + 1.96*(6.222e-07) #-1.701931e-06 

#per 100K births
(-2.921e-06)*100000 #-0.2738
(-4.140955e-06 )*100000 #-0.02743692
(-1.701931e-06 )*100000 # -0.01219204

#residual plot
res <- resid(model8)*100000
plot(fitted(model8), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_boys_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model8))
plot(acf, main = "")
# quartz.save("sga_boys_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model8)) 
plot(pacf, main = "")
# quartz.save("sga_boys_pacf.png", dpi = 300)

#Durbin-Watson
summary(model8)