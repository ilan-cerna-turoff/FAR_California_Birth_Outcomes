# File: ITS analysis
# Date: 1/7/2024
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","naniar","here","haven","forecast","patchwork")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
final_df <- read.csv2(here("data", "final", "final_clean_california_2000_2011.csv"), header = T, sep = ";")

#1a. set up data for analysis
preterm <- final_df %>%
  group_by(birthdate) %>% 
  tally(preterm == 1) %>% # number of preterm births per day
  ungroup() %>%
  rename(preterm = "n") %>%
  mutate(birthdate = as.Date(birthdate)) 

low_birthweight <- final_df %>%
  group_by(birthdate) %>% 
  tally(low_birthweight == 1) %>% # number of low birthweight births per day
  ungroup() %>%
  rename(low = "n") %>%
  mutate(birthdate = as.Date(birthdate)) 

verylow_birthweight <- final_df %>%
  group_by(birthdate) %>% 
  tally(verylow_birthweight == 1) %>% # number of very low birthweight births per day
  ungroup() %>%
  rename(very = "n") %>%
  mutate(birthdate = as.Date(birthdate)) 

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
  mutate(zlms = ((weight / central_tendency)^skewness - 1) / (skewness*dispersion), # zscore calculation
         small_for_gestational = case_when(zlms <= -1.28 ~ 1)) %>%
  group_by(birthdate) %>% 
  tally(small_for_gestational == 1) %>% # number of small_for_gestational births per day
  ungroup() %>%
  rename(small = "n") %>%
  mutate(intervention = case_when(birthdate < "2006-01-23" ~ 0,
                                  birthdate >= "2006-01-23" ~ 1),
         intervention = as.factor(intervention),
         birthdate = as.Date(birthdate))

#-------------------
#1c. visualize seasonality - date of birth
preterm_monthly <- preterm %>% 
  group_by(month = floor_date(birthdate, "month")) %>% #group by month
  dplyr::summarize(summary_variable = sum(preterm)) %>%
  ungroup() %>%
  mutate(intervention = case_when(month < "2006-11-01" ~ 0,
                                  month >= "2006-11-01" ~ 1),
         intervention = as.factor(intervention))

low_birthweight_monthly <- low_birthweight %>% 
  group_by(month = floor_date(birthdate, "month")) %>%
  dplyr::summarize(summary_variable = sum(low)) %>%
  ungroup() %>%
  mutate(intervention = case_when(month < "2006-11-01" ~ 0,
                                  month >= "2006-11-01" ~ 1),
         intervention = as.factor(intervention))

verylow_birthweight_monthly <- verylow_birthweight %>% 
  group_by(month = floor_date(birthdate, "month")) %>%
  dplyr::summarize(summary_variable = sum(very)) %>%
  ungroup() %>%
  mutate(intervention = case_when(month < "2006-11-01" ~ 0,
                                  month >= "2006-11-01" ~ 1),
         intervention = as.factor(intervention))

small_for_gestational_monthly <- small_for_gestational %>% 
  group_by(month = floor_date(birthdate, "month")) %>%
  dplyr::summarize(summary_variable = sum(small)) %>%
  ungroup() %>%
  mutate(intervention = case_when(month < "2006-11-01" ~ 0,
                                  month >= "2006-11-01" ~ 1),
         intervention = as.factor(intervention))

plot1 <- ggplot(preterm_monthly, aes(x = month, y = summary_variable)) +
  xlab(" ") + ylab("Preterm birth") + 
  theme_bw(base_size = 12) +
  geom_line(color = "darkcyan")  +
  geom_vline(xintercept = as.Date("2006-11-01"),
             linetype = 2, color = 2, linewidth = 1) +
  geom_text(aes(label = "Regulatory change", x = as.Date("2006-11-01"), y = Inf), vjust = -1) +
  theme(plot.margin = margin(.5,1,.5,1, "cm")) +
  theme(text=element_text(size=12,  family="serif"))  

# ggsave("plot1.png", plot = last_plot(), dpi = 300)

plot2 <- ggplot(low_birthweight_monthly, aes(x = month, y = summary_variable)) +
  xlab(" ") + ylab("Low birth weight") + 
  theme_bw(base_size = 12) +
  geom_line(color = "grey48")  +
  geom_vline(xintercept = as.Date("2006-11-01"),
             linetype = 2, color = 2, linewidth = 1) +
  geom_text(aes(label = "Regulatory change", x = as.Date("2006-11-01"), y = Inf), vjust = -1) +
  theme(plot.margin = margin(.5,1,.5,1, "cm")) +
  theme(text=element_text(size=12,  family="serif"))

# ggsave("plot2.png", plot = last_plot(), dpi = 300)

plot3 <- ggplot(verylow_birthweight_monthly, aes(x = month, y = summary_variable)) +
  xlab(" ") + ylab("Very low birth weight") + 
  theme_bw(base_size = 12) +
  geom_line(color = "darkslateblue")  +
  geom_vline(xintercept = as.Date("2006-11-01"),
             linetype = 2, color = 2, linewidth = 1) +
  geom_text(aes(label = "Regulatory change", x = as.Date("2006-11-01"), y = Inf), vjust = -1) +
  theme(plot.margin = margin(.5,1,.5,1, "cm")) +
  theme(text=element_text(size=12,  family="serif")) +
  scale_y_continuous(breaks=seq(200,600,by=40))

# ggsave("plot3.png", plot = last_plot(), dpi = 300)

plot4 <- ggplot(small_for_gestational_monthly, aes(x = month, y = summary_variable)) +
  xlab("Year") + ylab("Small-for-gestational age") + 
  theme_bw(base_size = 12) +
  geom_line(color = "goldenrod2")  + 
  geom_vline(xintercept = as.Date("2006-11-01"),
             linetype = 2, color = 2, linewidth = 1) +
  theme(plot.margin = margin(.5,1,.5,1, "cm")) +
  theme(text=element_text(size=12,  family="serif")) +
  scale_y_continuous(breaks=seq(3000,5000,by=450))

# ggsave("plot4.png", plot = last_plot(), dpi = 300)

combined_plots <- plot1 + plot2 + plot3 + plot4
combined_plots + plot_layout(ncol = 1, nrow = 4) + plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag.position = c(-.03,1.03)) + theme(plot.tag = element_text(face = "bold", size = 12, family = "serif"))

# ggsave("combined_plot1.png", plot = last_plot(), width = 8, height = 12, dpi = 300)

rm(plot1,plot2,plot3,plot4)
