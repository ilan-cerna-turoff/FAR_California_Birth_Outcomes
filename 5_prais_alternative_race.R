# File: ITS analysis - race
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

#white 
preterm_white <- final_df %>%
  filter(mom_multi_race1 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for white
preterm_rates_white <- preterm_white %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model1 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_white)
summary(model1)

#CIs
model1$coefficients[4] - 1.96*(3.336e-07) #-5.54657e-06 
model1$coefficients[4] + 1.96*(3.336e-07) #-4.238858e-06 

#per 100K births
(-4.893e-06)*100000 #-0.4893
(-5.54657e-06 )*100000 #-0.554657
(-4.238858e-06 )*100000 #-0.4238858

#residual plot
res <- resid(model1)*100000
plot(fitted(model1), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_white_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model1))
plot(acf, main = "")
# quartz.save("preterm_white_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model1)) 
plot(pacf, main = "")
# quartz.save("preterm_white_pacf.png", dpi = 300)

#Durbin-Watson
summary(model1)
#------------------------------------------------------------------------------
#black 
preterm_black <- final_df %>%
  filter(mom_multi_race1 == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for black
preterm_rates_black <- preterm_black %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model2 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_black)
summary(model2)

#CIs
model2$coefficients[4] - 1.96*(7.697e-07) #-4.254539e-06 
model2$coefficients[4] + 1.96*(7.697e-07) #-1.237315e-06

#per 100K births
(-2.746e-06)*100000 #-0.2746
(-4.254539e-06 )*100000 #-0.4254539
(-1.237315e-06)*100000 #-0.1237315

#residual plot
res <- resid(model2)*100000
plot(fitted(model2), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_black_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model2))
plot(acf, main = "")
# quartz.save("preterm_black_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model2)) 
plot(pacf, main = "")
# quartz.save("preterm_black_pacf.png", dpi = 300)

#Durbin-Watson
summary(model2)
#------------------------------------------------------------------------------
#Native 
preterm_native <- final_df %>%
  filter(mom_multi_race1 == 3) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for native
preterm_rates_native <- preterm_native %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model3 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_native)
summary(model3)

#CIs
model3$coefficients[4] - 1.96*(1.501e-06 ) #-7.082228e-06
model3$coefficients[4] + 1.96*(1.501e-06 ) #-1.198308e-06 

#per 100K births
(-4.140e-06)*100000 #-0.414
(-7.082228e-06)*100000 #-0.7082228
(-1.198308e-06 )*100000 # -0.1198308

#residual plot
res <- resid(model3)*100000
plot(fitted(model3), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_native_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model3))
plot(acf, main = "")
# quartz.save("preterm_native_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model3)) 
plot(pacf, main = "")
# quartz.save("preterm_native_pacf.png", dpi = 300)

#Durbin-Watson
summary(model3)
#------------------------------------------------------------------------------
#Asian
preterm_asian <- final_df %>%
  filter(mom_multi_race1 == 4) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for asian
preterm_rates_asian <- preterm_asian %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model4 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_asian)
summary(model4)

#CIs
model4$coefficients[4] - 1.96*(3.616e-07) # -4.242437e-06
model4$coefficients[4] + 1.96*(3.616e-07) #-2.824965e-06 

#per 100K births
(-3.534e-06)*100000 #-0.3534
(-4.242437e-06)*100000 #-0.4242437
(-2.824965e-06 )*100000 # -0.2824965

#residual plot
res <- resid(model4)*100000
plot(fitted(model4), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_asian_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model4))
plot(acf, main = "")
# quartz.save("preterm_asian_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model4)) 
plot(pacf, main = "")
# quartz.save("preterm_asian_pacf.png", dpi = 300)

#Durbin-Watson
summary(model4)
#------------------------------------------------------------------------------
#Pacific Islander
preterm_pac_islander <- final_df %>%
  filter(mom_multi_race1 == 5) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for pac islander
preterm_rates_pac_islander <- preterm_pac_islander %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model5 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_pac_islander)
summary(model5)

#CIs
model5$coefficients[4] - 1.96*(1.504e-06) # -9.451372e-06 
model5$coefficients[4] + 1.96*(1.504e-06) #-3.555692e-06 

#per 100K births
(-6.503e-06)*100000 #-0.6503
(-9.451372e-06 )*100000 #-0.9451372
(-3.555692e-06 )*100000 # -0.3555692

#residual plot
res <- resid(model5)*100000
plot(fitted(model5), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_pacislander_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model5))
plot(acf, main = "")
# quartz.save("preterm_pacislander_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model5)) 
plot(pacf, main = "")
# quartz.save("preterm_pacislander_pacf.png", dpi = 300)

#Durbin-Watson
summary(model5)
#------------------------------------------------------------------------------
#Two or more races
preterm_multiracial <- final_df %>%
  filter(mom_multi_race1 == 7) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(preterm = sum(preterm)/nrow(.)) %>% 
  ungroup() %>%
  select(c(preterm, month)) %>%
  unique()

#preterm - rates for multiracial
preterm_rates_multiracial <- preterm_multiracial %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model6 <- prais_winsten(preterm ~ time + intervention + post_intervention,
                        index ='time', 
                        data = preterm_rates_multiracial)
summary(model6)

#CIs
model6$coefficients[4] - 1.96*(8.223e-07) # -2.282687e-06 
model6$coefficients[4] + 1.96*(8.223e-07) #9.407293e-07

#per 100K births
(-6.710e-07)*100000 #-0.0671
(-2.282687e-06 )*100000 #-0.2282687
(9.407293e-07)*100000 # 0.09407293

#residual plot
res <- resid(model6)*100000
plot(fitted(model6), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("preterm_twoormoreraces_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model6))
plot(acf, main = "")
# quartz.save("preterm_twoormoreraces_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model6)) 
plot(pacf, main = "")
# quartz.save("preterm_twoormoreraces_pacf.png", dpi = 300)

#Durbin-Watson
summary(model6)
#------------------------------------------------------------------------------
#white - low birthweight
low_birthweight_white <- final_df %>%
  filter(mom_multi_race1 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for white
low_birthweight_rates_white <- low_birthweight_white %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model7 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_white)
summary(model7)

#CIs
model7$coefficients[4] - 1.96*(2.351e-07) # -2.230046e-06
model7$coefficients[4] + 1.96*(2.351e-07) # -1.308454e-06 

#per 100K births
(-1.769e-06)*100000 #-0.1769
(-2.230046e-06 )*100000 #-0.2230046
(-1.308454e-06)*100000 # -0.1308454

#residual plot
res <- resid(model7)*100000
plot(fitted(model7), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_white_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model7))
plot(acf, main = "")
# quartz.save("lowbirth_white_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model7)) 
plot(pacf, main = "")
# quartz.save("lowbirth_white_pacf.png", dpi = 300)

#Durbin-Watson
summary(model7)
#------------------------------------------------------------------------------
#black low birthweight 
low_birthweight_black <- final_df %>%
  filter(mom_multi_race1 == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for black
low_birthweight_rates_black <- low_birthweight_black %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model8 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_black)
summary(model8)

#CIs
model8$coefficients[4] - 1.96*(5.761e-07) #  -2.56817e-06 
model8$coefficients[4] + 1.96*(5.761e-07) #-3.098575e-07 

#per 100K births
(-1.439e-06)*100000 #-0.1439
(-1.022727e-06)*100000 #-0.1022727 
(-3.743585e-07)*100000 #-0.03743585

#residual plot
res <- resid(model8)*100000
plot(fitted(model8), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_black_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model8))
plot(acf, main = "")
# quartz.save("lowbirth_black_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model8)) 
plot(pacf, main = "")
# quartz.save("lowbirth_black_pacf.png", dpi = 300)

#Durbin-Watson
summary(model8)
#------------------------------------------------------------------------------
#Native low birthweight 
low_birthweight_native <- final_df %>%
  filter(mom_multi_race1 == 3) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for native
low_birthweight_rates_native <- low_birthweight_native %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model9 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_native)
summary(model9)

#CIs
model9$coefficients[4] - 1.96*(9.014e-07) # -3.623347e-06 
model9$coefficients[4] + 1.96*(9.014e-07) # -8.985943e-08 

#per 100K births
(-1.857e-06)*100000 #-0.1857
(-3.623347e-06 )*100000 #-0.3623347
(-8.985943e-08 )*100000 # -0.008985943

#residual plot
res <- resid(model9)*100000
plot(fitted(model9), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_native_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model9))
plot(acf, main = "")
# quartz.save("lowbirth_native_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model9)) 
plot(pacf, main = "")
# quartz.save("lowbirth_native_pacf.png", dpi = 300)

#Durbin-Watson
summary(model9)
#------------------------------------------------------------------------------
#Asian low birthweight 
low_birthweight_asian <- final_df %>%
  filter(mom_multi_race1 == 4) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for asian
low_birthweight_rates_asian <- low_birthweight_asian %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model10 <- prais_winsten(low ~ time + intervention + post_intervention,
                        index ='time', 
                        data = low_birthweight_rates_asian)
summary(model10)

#CIs
model10$coefficients[4] - 1.96*(2.819e-07) # -1.460305e-06  
model10$coefficients[4] + 1.96*(2.819e-07) #-3.552565e-07

#per 100K births
(-9.078e-07)*100000 # -0.09078
(-1.460305e-06 )*100000 #-0.1460305
(-3.552565e-07)*100000 # -0.03552565

#residual plot
res <- resid(model10)*100000
plot(fitted(model10), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_asian_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model10))
plot(acf, main = "")
# quartz.save("lowbirth_asian_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model10)) 
plot(pacf, main = "")
# quartz.save("lowbirth_asian_pacf.png", dpi = 300)

#Durbin-Watson
summary(model10)
#------------------------------------------------------------------------------
#pac_islander low birthweight 
low_birthweight_pac_islander <- final_df %>%
  filter(mom_multi_race1 == 5) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for pac_islander
low_birthweight_rates_pac_islander <- low_birthweight_pac_islander %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model11 <- prais_winsten(low ~ time + intervention + post_intervention,
                         index ='time', 
                         data = low_birthweight_rates_pac_islander)
summary(model11)

#CIs
model11$coefficients[4] - 1.96*(9.825e-07) # -4.143346e-06  
model11$coefficients[4] + 1.96*(9.825e-07) #-2.919461e-07

#per 100K births
(-2.218e-06)*100000 # -0.2218
(-4.143346e-06 )*100000 #-0.4143346
(-2.919461e-07)*100000 # -0.02919461

#residual plot
res <- resid(model11)*100000
plot(fitted(model11), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_pacislander_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model11))
plot(acf, main = "")
# quartz.save("lowbirth_pacislander_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model11)) 
plot(pacf, main = "")
# quartz.save("lowbirth_pacislander_pacf.png", dpi = 300)

#Durbin-Watson
summary(model11)
#------------------------------------------------------------------------------
#Two or more races - low birthweight 
low_birthweight_multiracial <- final_df %>%
  filter(mom_multi_race1 == 7) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(low = sum(low_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(low, month)) %>%
  unique() 

#low_birthweight - rates for multiracial
low_birthweight_rates_multiracial <- low_birthweight_multiracial %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model12 <- prais_winsten(low ~ time + intervention + post_intervention,
                         index ='time', 
                         data = low_birthweight_rates_multiracial)
summary(model12)

#CIs
model12$coefficients[4] - 1.96*(5.517e-07 ) # 1.705047e-07 
model12$coefficients[4] + 1.96*(5.517e-07 ) #2.333169e-06 

#per 100K births
(1.252e-06)*100000 # 0.1252
(1.705047e-07 )*100000 #0.01705047
(2.333169e-06 )*100000 # 0.2333169

#residual plot
res <- resid(model12)*100000
plot(fitted(model12), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("lowbirth_twoormoreraces_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model12))
plot(acf, main = "")
# quartz.save("lowbirth_twoormoreraces_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model12)) 
plot(pacf, main = "")
# quartz.save("lowbirth_twoormoreraces_pacf.png", dpi = 300)

#Durbin-Watson
summary(model12)
#------------------------------------------------------------------------------
#white very low birthweight
verylow_birthweight_white <- final_df %>%
  filter(mom_multi_race1 == 1) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for white
verylow_birthweight_rates_white <- verylow_birthweight_white %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model13 <- prais_winsten(very ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_rates_white)
summary(model13)

#CIs
model13$coefficients[4] - 1.96*(4.083e-08) #  -3.302942e-07
model13$coefficients[4] + 1.96*(4.083e-08) # -1.702406e-07

#per 100K births
( -2.503e-07)*100000 #-0.02503
(-3.302942e-07)*100000 #-0.03302942
(-1.702406e-07)*100000 #-0.01702406

#residual plot
res <- resid(model13)*100000
plot(fitted(model13), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_white_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model13))
plot(acf, main = "")
# quartz.save("verylowbirth_white_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model13)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_white_pacf.png", dpi = 300)

#Durbin-Watson
summary(model13)
#------------------------------------------------------------------------------
#black very low birthweight
verylow_birthweight_black <- final_df %>%
  filter(mom_multi_race1 == 2) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for blacks
verylow_birthweight_rates_black <- verylow_birthweight_black %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model14 <- prais_winsten(very ~ time + intervention + post_intervention,
                        index ='time', 
                        data = verylow_birthweight_rates_black)
summary(model14)

#CIs
model14$coefficients[4] - 1.96*(2.195e-07) #-6.090026e-07
model14$coefficients[4] + 1.96*(2.195e-07) #2.514374e-07

#per 100K births
(-1.788e-07)*100000 #-0.01788
(-6.090026e-07 )*100000 #-0.06090026
(2.514374e-07)*100000 #  0.02514374

#residual plot
res <- resid(model14)*100000
plot(fitted(model14), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_black_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model14))
plot(acf, main = "")
# quartz.save("verylowbirth_black_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model14)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_black_pacf.png", dpi = 300)

#Durbin-Watson
summary(model14)
#------------------------------------------------------------------------------
#Native very low birthweight
verylow_birthweight_native <- final_df %>%
  filter(mom_multi_race1 == 3) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for natives
verylow_birthweight_rates_native <- verylow_birthweight_native %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model15 <- prais_winsten(very ~ time + intervention + post_intervention,
                         index ='time', 
                         data = verylow_birthweight_rates_native)
summary(model15)

#CIs
model15$coefficients[4] - 1.96*(4.077e-07) #-1.175618e-06
model15$coefficients[4] + 1.96*(4.077e-07) # 4.22566e-07 

#per 100K births
(-3.765e-07)*100000 #-0.01788
(-1.175618e-06)*100000 #-0.06090026
( 4.22566e-07 )*100000 #  0.02514374

#residual plot
res <- resid(model15)*100000
plot(fitted(model15), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_native_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model15))
plot(acf, main = "")
# quartz.save("verylowbirth_native_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model15)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_native_pacf.png", dpi = 300)

#Durbin-Watson
summary(model15)
#------------------------------------------------------------------------------
#Asian very low birthweight
verylow_birthweight_asian <- final_df %>%
  filter(mom_multi_race1 == 4) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for asians
verylow_birthweight_rates_asian <- verylow_birthweight_asian %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model16 <- prais_winsten(very ~ time + intervention + post_intervention,
                         index ='time', 
                         data = verylow_birthweight_rates_asian)
summary(model16)

#CIs
model16$coefficients[4] - 1.96*(8.706e-08) #-3.470809e-07
model16$coefficients[4] + 1.96*(8.706e-08) #-5.805696e-09 

#per 100K births
(-1.764e-07)*100000 #-0.01764
(-3.470809e-07)*100000 #-0.03470809
(-5.805696e-09 )*100000 # -0.0005805696

#residual plot
res <- resid(model16)*100000
plot(fitted(model16), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_asian_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model16))
plot(acf, main = "")
# quartz.save("verylowbirth_asian_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model16)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_asian_pacf.png", dpi = 300)

#Durbin-Watson
summary(model16)
#------------------------------------------------------------------------------
#Pacific Islander very low birthweight
verylow_birthweight_pac_islander <- final_df %>%
  filter(mom_multi_race1 == 5) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for pac_islanders
verylow_birthweight_rates_pac_islander <- verylow_birthweight_pac_islander %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model17 <- prais_winsten(very ~ time + intervention + post_intervention,
                         index ='time', 
                         data = verylow_birthweight_rates_pac_islander)
summary(model17)

#CIs
model17$coefficients[4] - 1.96*(4.386e-07) #-1.151777e-06
model17$coefficients[4] + 1.96*(4.386e-07) #5.675349e-07

#per 100K births
(-2.921e-07)*100000 #-0.02921
(-1.151777e-06)*100000 #-0.1151777
(5.675349e-07)*100000 #  0.05675349

#residual plot
res <- resid(model17)*100000
plot(fitted(model17), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_pacislander_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model17))
plot(acf, main = "")
# quartz.save("verylowbirth_pacislander_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model17)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_pacislander_pacf.png", dpi = 300)

#Durbin-Watson
summary(model17)
#------------------------------------------------------------------------------
#two or more racial groups very low birthweight
verylow_birthweight_multiracial <- final_df %>%
  filter(mom_multi_race1 == 7) %>%
  mutate(birthdate = as.Date(birthdate)) %>%
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  mutate(very = sum(verylow_birthweight)/nrow(.)) %>% #100000 * 
  ungroup() %>%
  select(c(very, month)) %>%
  unique() 

#verylow_birthweight - rates for multiracials
verylow_birthweight_rates_multiracial <- verylow_birthweight_multiracial %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model18 <- prais_winsten(very ~ time + intervention + post_intervention,
                         index ='time', 
                         data = verylow_birthweight_rates_multiracial)
summary(model18)

#CIs
model18$coefficients[4] - 1.96*(2.143e-07) #3.90039e-08 
model18$coefficients[4] + 1.96*(2.143e-07) # 8.790599e-07 

#per 100K births
(4.590e-07)*100000 #0.0459
(3.90039e-08 )*100000 #0.00390039
( 8.790599e-07 )*100000 # 0.08790599

#residual plot
res <- resid(model18)*100000
plot(fitted(model18), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("verylowbirth_twoormoreraces_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model18))
plot(acf, main = "")
# quartz.save("verylowbirth_twoormoreraces_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model18)) 
plot(pacf, main = "")
# quartz.save("verylowbirth_twoormoreraces_pacf.png", dpi = 300)

#Durbin-Watson
summary(model18)
#------------------------------------------------------------------------------
#white small-for-gestational

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

final_df_white <- final_df %>%
  filter(mom_multi_race1 == 1) 
  
#calculate zscores and define weight cut points
small_for_gestational_white = full_join(final_df_white, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
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

#small for gestational - rates 
small_for_gestational_rates_white <- small_for_gestational_white %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model19 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_white)
summary(model19)

#CIs
model19$coefficients[4] - 1.96*(6.622e-07) #-4.767345e-06
model19$coefficients[4] + 1.96*(6.622e-07) # -2.171521e-06 

#per 100K births
(-3.469e-06)*100000 #-0.3469
(-4.767345e-06)*100000 #-0.4767345
(-2.171521e-06 )*100000 # -0.2171521

#residual plot
res <- resid(model19)*100000
plot(fitted(model19), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_white_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model19))
plot(acf, main = "")
# quartz.save("sga_white_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model19)) 
plot(pacf, main = "")
# quartz.save("sga_white_pacf.png", dpi = 300)

#Durbin-Watson
summary(model19)
#------------------------------------------------------------------------------
#black small-for-gestational

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

final_df_black <- final_df %>%
  filter(mom_multi_race1 == 2) 

#calculate zscores and define weight cut points
small_for_gestational_black = full_join(final_df_black, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
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

#small for gestational - rates 
small_for_gestational_rates_black <- small_for_gestational_black %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model20 <- prais_winsten(small ~ time + intervention + post_intervention,
                        index ='time', 
                        data = small_for_gestational_rates_black)
summary(model20)

#CIs
model20$coefficients[4] - 1.96*(1.028e-06) #  -2.308458e-06 
model20$coefficients[4] + 1.96*(1.028e-06) # 1.721302e-06 

#per 100K births
(-2.936e-07)*100000 #-0.02936
( -2.308458e-06 )*100000 #-0.2308458
(1.721302e-06)*100000 # 0.1721302

#residual plot
res <- resid(model20)*100000
plot(fitted(model20), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_black_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model20))
plot(acf, main = "")
# quartz.save("sga_black_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model20)) 
plot(pacf, main = "")
# quartz.save("sga_black_pacf.png", dpi = 300)

#Durbin-Watson
summary(model20)
#------------------------------------------------------------------------------
#native small-for-gestational

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

final_df_native <- final_df %>%
  filter(mom_multi_race1 == 3) 

#calculate zscores and define weight cut points
small_for_gestational_native = full_join(final_df_native, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
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

#small for gestational - rates 
small_for_gestational_rates_native <- small_for_gestational_native %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model21 <- prais_winsten(small ~ time + intervention + post_intervention,
                         index ='time', 
                         data = small_for_gestational_rates_native)
summary(model21)

#CIs
model21$coefficients[4] - 1.96*(1.555e-06) # -8.067209e-06 
model21$coefficients[4] + 1.96*(1.555e-06) # -1.971609e-06

#per 100K births
(-5.019e-06)*100000 #-0.5019
(-8.067209e-06 )*100000 #-0.8067209
(-1.971609e-06)*100000 # -0.1971609

#residual plot
res <- resid(model21)*100000
plot(fitted(model21), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_native_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model21))
plot(acf, main = "")
# quartz.save("sga_native_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model21)) 
plot(pacf, main = "")
# quartz.save("sga_native_pacf.png", dpi = 300)

#Durbin-Watson
summary(model21)
#------------------------------------------------------------------------------
#asian small-for-gestational

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

final_df_asian <- final_df %>%
  filter(mom_multi_race1 == 4) 

#calculate zscores and define weight cut points
small_for_gestational_asian = full_join(final_df_asian, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
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

#small for gestational - rates 
small_for_gestational_rates_asian <- small_for_gestational_asian %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model22 <- prais_winsten(small ~ time + intervention + post_intervention,
                         index ='time', 
                         data = small_for_gestational_rates_asian)
summary(model22)

#CIs
model22$coefficients[4] - 1.96*(7.395e-07) # -3.22606e-06  
model22$coefficients[4] + 1.96*(7.395e-07) # -3.272205e-07

#per 100K births
(-1.777e-06)*100000 #-0.1777
(-3.22606e-06)*100000 #-0.322606
(-3.272205e-07)*100000 # -0.03272205

#residual plot
res <- resid(model22)*100000
plot(fitted(model22), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_asian_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model22))
plot(acf, main = "")
# quartz.save("sga_asian_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model22)) 
plot(pacf, main = "")
# quartz.save("sga_asian_pacf.png", dpi = 300)

#Durbin-Watson
summary(model22)
#------------------------------------------------------------------------------
#pac_islander small-for-gestational

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

final_df_pac_islander <- final_df %>%
  filter(mom_multi_race1 == 5) 

#calculate zscores and define weight cut points
small_for_gestational_pac_islander = full_join(final_df_pac_islander, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
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

#small for gestational - rates 
small_for_gestational_rates_pac_islander <- small_for_gestational_pac_islander %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model23 <- prais_winsten(small ~ time + intervention + post_intervention,
                         index ='time', 
                         data = small_for_gestational_rates_pac_islander)
summary(model23)

#CIs
model23$coefficients[4] - 1.96*(1.390e-06) # -7.177004e-06  
model23$coefficients[4] + 1.96*(1.390e-06) # -1.728204e-06

#per 100K births
(-4.453e-06)*100000 #-0.4453
(-7.177004e-06 )*100000 #-0.7177004
(-1.728204e-06)*100000 # -0.1728204

#residual plot
res <- resid(model23)*100000
plot(fitted(model23), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_pacislander_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model23))
plot(acf, main = "")
# quartz.save("sga_pacislander_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model23)) 
plot(pacf, main = "")
# quartz.save("sga_pacislander_pacf.png", dpi = 300)

#Durbin-Watson
summary(model23)
#------------------------------------------------------------------------------
#Two or more racial groups small-for-gestational

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

final_df_multiracial <- final_df %>%
  filter(mom_multi_race1 == 7) 

#calculate zscores and define weight cut points
small_for_gestational_multiracial = full_join(final_df_multiracial, lms_ref, by = c("ch_sex", "leng_gestation_wks")) %>% 
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

#small for gestational - rates 
small_for_gestational_rates_multiracial <- small_for_gestational_multiracial %>%
  mutate(intervention = ifelse(month < "2006-11-01", 0, 1),
         time = row_number()) %>% #approximately 9 months (gestation) after regulatory change. Rounded to first day of following month
  group_by(intervention) %>%
  mutate(post_intervention = row_number(),
         post_intervention = ifelse(intervention == 0, 0, post_intervention)) %>% 
  ungroup() 

model24 <- prais_winsten(small ~ time + intervention + post_intervention,
                         index ='time', 
                         data = small_for_gestational_rates_multiracial)
summary(model24)

#CIs
model24$coefficients[4] - 1.96*(9.307e-07) # 2.992774e-06  
model24$coefficients[4] + 1.96*(9.307e-07) # 6.641118e-06 

#per 100K births
(4.817e-06)*100000 #0.4817
(2.992774e-06 )*100000 #0.2992774
(6.641118e-06 )*100000 # 0.6641118

#residual plot
res <- resid(model24)*100000
plot(fitted(model24), res, xlab="Predicted Residuals", ylab="Actual Residuals")
abline(0,0, col = "blue")
# quartz.save("sga_twoormoreraces_residuals.png", dpi = 300)

#ACF
acf <- acf(resid(model24))
plot(acf, main = "")
# quartz.save("sga_twoormoreraces_acf.png", dpi = 300)

#PACF
pacf <- pacf(resid(model24)) 
plot(pacf, main = "")
# quartz.save("sga_twoormoreraces_pacf.png", dpi = 300)

#Durbin-Watson
summary(model24)
