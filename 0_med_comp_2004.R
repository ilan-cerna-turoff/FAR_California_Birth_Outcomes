# File: Medical complications set up - 2004
# Date: 11/30/2023
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","here","haven")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
#1a. read in columns that using
bsmf_2004 <- readr::read_fwf(here("data", "raw", "2000_2005","Nigra_BSMF_2004.dat"),
                             fwf_cols(ch_sex=c(58), plural=c(61), plural_ord=c(62),
                                      weight=c(63,66), birthdate=c(67,73), hosp_code=c(74,77), #these are the breakspaces for where the column begins and ends
                                      birth_countyplace=c(79, 80), mom_birthplace=c(98, 99),
                                      mom_age=c(100, 101), mom_years_edu=c(102, 103),
                                      mom_res_county=c(107, 108), census_tract=c(113, 118),
                                      fath_age=c(119, 120), fath_years_edu=c(121, 122),
                                      leng_gestation=c(143, 145), mon_prenatal=c(146),
                                      num_prenatal=c(147, 148), prev_livebirth_liv=c(153, 154),
                                      prev_livebirth_dead=c(155, 156), total_liv=c(157, 158),
                                      ch_total_ever=c(163, 164), last_liv_date=c(165, 168),
                                      preg_complic=c(193, 224), labor_complic=c(225, 242),
                                      fath_hisp=c(285), mom_hisp=c(286), mom_zipcode=c(287, 291),
                                      mom_state=c(292, 293),
                                      fath_eth1=c(340, 341), fath_eth2=c(342, 343),
                                      fath_eth3=c(344, 345), 
                                      mom_eth1=c(355, 356), mom_eth2=c(357, 358),
                                      mom_eth3=c(359, 360), pay_prenatal=c(366, 367),
                                      pay_delivery=c(368, 369), fath_multi_race1=c(371),
                                      mom_multi_race1=c(372), mom_city=c(441, 475)),
                             show_col_types = FALSE) 

#1b. Deal with problematic variable that does not parse well from .dat to .csv format
bsmf_2004 <- bsmf_2004 %>% 
  mutate(last_liv_date = str_replace(last_liv_date, "(^\\s+)|(\\s+$)", ""), #get rid of breakspace at beginning and end
         last_liv_date = str_trim(last_liv_date, "both"), #get rid of whitespace
         last_liv_date = case_when(startsWith(last_liv_date, "07") ~ "04",
                                   startsWith(last_liv_date, "09") ~ "04",
                                   TRUE ~ last_liv_date), #two entries that can correct from other information
         last_liv_date = as.numeric(substr(last_liv_date, 1, 2)), #just last 2 meaningful digits
         last_liv_date = ifelse(last_liv_date < 20, str_c("200", last_liv_date), str_c("19", last_liv_date)), #trick for when to add 2000 and 1900 for year
         last_liv_date = ifelse(as.numeric(last_liv_date) > 2005, NA, last_liv_date), #removes errors after dataset date
         last_liv_date = ifelse(last_liv_date == 1933, NA, last_liv_date)) #removes impossible date

#1c. Fix pregnancy and labor complications
preg_2004 <- bsmf_2004 %>%
  select(preg_complic) %>%
  mutate(condition1 = substr(preg_complic, 1,2), condition2 = substr(preg_complic, 3,4), 
         condition3 = substr(preg_complic, 5,6), condition4 = substr(preg_complic, 7,8),
         condition5 = substr(preg_complic, 9,10), condition6 = substr(preg_complic, 11,12),
         condition7 = substr(preg_complic, 13,14), condition8 = substr(preg_complic, 15,16),
         condition9 = substr(preg_complic, 17,18), condition10 = substr(preg_complic, 19,20),
         across(c(condition1:condition10), as.numeric)) %>%
  rowwise() %>%
  mutate(none_p = ifelse(c(condition1 == 0 | condition2 == 0 | condition3 == 0 |
                             condition4 == 0 | condition5 == 0 | condition6 == 0 |
                             condition7 == 0 | condition8 == 0 | condition9 == 0 |
                             condition10 == 0 ), 1, NA),
         preeclampsia_p = ifelse(c(condition1 == 1 | condition2 == 1 | condition3 == 1 |
                                     condition4 == 1 | condition5 == 1 | condition6 == 1 |
                                     condition7 == 1 | condition8 == 1 | condition9 == 1 |
                                     condition10 == 1 ), 1, NA),
         eclampsia_p = ifelse(c(condition1 == 2 | condition2 == 2 | condition3 == 2 |
                                  condition4 == 2 | condition5 == 2 | condition6 == 2 |
                                  condition7 == 2 | condition8 == 2 | condition9 == 2 |
                                  condition10 == 2 ), 1, NA),
         hypertension_p = ifelse(c(condition1 == 3 | condition2 == 3 | condition3 == 3 |
                                     condition4 == 3 | condition5 == 3 | condition6 == 3 |
                                     condition7 == 3 | condition8 == 3 | condition9 == 3 |
                                     condition10 == 3 ), 1, NA),
         renal_dis_p = ifelse(c(condition1 == 4 | condition2 == 4 | condition3 == 4 |
                                  condition4 == 4 | condition5 == 4 | condition6 == 4 |
                                  condition7 == 4 | condition8 == 4 | condition9 == 4 |
                                  condition10 == 4 ), 1, NA),
         pyelonephritis_p = ifelse(c(condition1 == 5 | condition2 == 5 | condition3 == 5 |
                                       condition4 == 5 | condition5 == 5 | condition6 == 5 |
                                       condition7 == 5 | condition8 == 5 | condition9 == 5 |
                                       condition10 == 5 ), 1, NA),
         anemia_p = ifelse(c(condition1 == 6 | condition2 == 6 | condition3 == 6 |
                               condition4 == 6 | condition5 == 6 | condition6 == 6 |
                               condition7 == 6 | condition8 == 6 | condition9 == 6 |
                               condition10 == 6 ), 1, NA),
         cardiac_dis_p = ifelse(c(condition1 == 7 | condition2 == 7 | condition3 == 7 |
                                    condition4 == 7 | condition5 == 7 | condition6 == 7 |
                                    condition7 == 7 | condition8 == 7 | condition9 == 7 |
                                    condition10 == 7 ), 1, NA),
         lung_dis_p = ifelse(c(condition1 == 8 | condition2 == 8 | condition3 == 8 |
                                 condition4 == 8 | condition5 == 8 | condition6 == 8 |
                                 condition7 == 8 | condition8 == 8 | condition9 == 8 |
                                 condition10 == 8 ), 1, NA),
         diabetes_p = ifelse(c(condition1 == 9 | condition2 == 9 | condition3 == 9 |
                                 condition4 == 9 | condition5 == 9 | condition6 == 9 |
                                 condition7 == 9 | condition8 == 9 | condition9 == 9 |
                                 condition10 == 9 ), 1, NA),
         rh_sensitization_p = ifelse(c(condition1 == 10 | condition2 == 10 | condition3 == 10 |
                                         condition4 == 10 | condition5 == 10 | condition6 == 10 |
                                         condition7 == 10 | condition8 == 10 | condition9 == 10 |
                                         condition10 == 10 ), 1, NA),
         hemoglobinopathy_p = ifelse(c(condition1 == 11 | condition2 == 11 | condition3 == 11 |
                                         condition4 == 11 | condition5 == 11 | condition6 == 11 |
                                         condition7 == 11 | condition8 == 11 | condition9 == 11 |
                                         condition10 == 11 ), 1, NA),
         uter_bleeding_p = ifelse(c(condition1 == 12 | condition2 == 12 | condition3 == 12 |
                                      condition4 == 12 | condition5 == 12 | condition6 == 12 |
                                      condition7 == 12 | condition8 == 12 | condition9 == 12 |
                                      condition10 == 12 ), 1, NA),
         poly_oligohydramnios_p = ifelse(c(condition1 == 13 | condition2 == 13 | condition3 == 13 |
                                             condition4 == 13 | condition5 == 13 | condition6 == 13 |
                                             condition7 == 13 | condition8 == 13 | condition9 == 13 |
                                             condition10 == 13 ), 1, NA),
         incomp_cervix_p = ifelse(c(condition1 == 14 | condition2 == 14 | condition3 == 14 |
                                      condition4 == 14 | condition5 == 14 | condition6 == 14 |
                                      condition7 == 14 | condition8 == 14 | condition9 == 14 |
                                      condition10 == 14 ), 1, NA),
         premat_lab_p = ifelse(c(condition1 == 15 | condition2 == 15 | condition3 == 15 |
                                   condition4 == 15 | condition5 == 15 | condition6 == 15 |
                                   condition7 == 15 | condition8 == 15 | condition9 == 15 |
                                   condition10 == 15 ), 1, NA),
         gen_herpes_p = ifelse(c(condition1 == 16 | condition2 == 16 | condition3 == 16 |
                                   condition4 == 16 | condition5 == 16 | condition6 == 16 |
                                   condition7 == 16 | condition8 == 16 | condition9 == 16 |
                                   condition10 == 16 ), 1, NA),
         oth_std_p = ifelse(c(condition1 == 17 | condition2 == 17 | condition3 == 17 |
                                condition4 == 17 | condition5 == 17 | condition6 == 17 |
                                condition7 == 17 | condition8 == 17 | condition9 == 17 |
                                condition10 == 17 ), 1, NA),
         hep_b_p = ifelse(c(condition1 == 18 | condition2 == 18 | condition3 == 18 |
                              condition4 == 18 | condition5 == 18 | condition6 == 18 |
                              condition7 == 18 | condition8 == 18 | condition9 == 18 |
                              condition10 == 18 ), 1, NA),
         rubella_p = ifelse(c(condition1 == 19 | condition2 == 19 | condition3 == 19 |
                                condition4 == 19 | condition5 == 19 | condition6 == 19 |
                                condition7 == 19 | condition8 == 19 | condition9 == 19 |
                                condition10 == 19 ), 1, NA),
         tobac_p = ifelse(c(condition1 == 20 | condition2 == 20 | condition3 == 20 |
                              condition4 == 20 | condition5 == 20 | condition6 == 20 |
                              condition7 == 20 | condition8 == 20 | condition9 == 20 |
                              condition10 == 20 ), 1, NA),
         above_gram_4000_p = ifelse(c(condition1 == 21 | condition2 == 21 | condition3 == 21 |
                                        condition4 == 21 | condition5 == 21 | condition6 == 21 |
                                        condition7 == 21 | condition8 == 21 | condition9 == 21 |
                                        condition10 == 21 ), 1, NA),
         bel_gram_2500_p = ifelse(c(condition1 == 22 | condition2 == 22 | condition3 == 22 |
                                      condition4 == 22 | condition5 == 22 | condition6 == 22 |
                                      condition7 == 22 | condition8 == 22 | condition9 == 22 |
                                      condition10 == 22 ), 1, NA),
         bel_weeks_37_p = ifelse(c(condition1 == 23 | condition2 == 23 | condition3 == 23 |
                                     condition4 == 23 | condition5 == 23 | condition6 == 23 |
                                     condition7 == 23 | condition8 == 23 | condition9 == 23 |
                                     condition10 == 23 ), 1, NA),
         cervix_cerciage_p = ifelse(c(condition1 == 24 | condition2 == 24 | condition3 == 24 |
                                        condition4 == 24 | condition5 == 24 | condition6 == 24 |
                                        condition7 == 24 | condition8 == 24 | condition9 == 24 |
                                        condition10 == 24 ), 1, NA),
         chor_vilius_p = ifelse(c(condition1 == 25 | condition2 == 25 | condition3 == 25 |
                                    condition4 == 25 | condition5 == 25 | condition6 == 25 |
                                    condition7 == 25 | condition8 == 25 | condition9 == 25 |
                                    condition10 == 25 ), 1, NA),
         amniocentesis_p = ifelse(c(condition1 == 26 | condition2 == 26 | condition3 == 26 |
                                      condition4 == 26 | condition5 == 26 | condition6 == 26 |
                                      condition7 == 26 | condition8 == 26 | condition9 == 26 |
                                      condition10 == 26 ), 1, NA),
         elec_fetal_mon_p = ifelse(c(condition1 == 27 | condition2 == 27 | condition3 == 27 |
                                       condition4 == 27 | condition5 == 27 | condition6 == 27 |
                                       condition7 == 27 | condition8 == 27 | condition9 == 27 |
                                       condition10 == 27 ), 1, NA),
         tocolysis_p = ifelse(c(condition1 == 28 | condition2 == 28 | condition3 == 28 |
                                  condition4 == 28 | condition5 == 28 | condition6 == 28 |
                                  condition7 == 28 | condition8 == 28 | condition9 == 28 |
                                  condition10 == 28 ), 1, NA),
         ultrasound_p = ifelse(c(condition1 == 29 | condition2 == 29 | condition3 == 29 |
                                   condition4 == 29 | condition5 == 29 | condition6 == 29 |
                                   condition7 == 29 | condition8 == 29 | condition9 == 29 |
                                   condition10 == 29 ), 1, NA),
         oth_cond_p = ifelse(c(condition1 == 30 | condition2 == 30 | condition3 == 30 |
                                 condition4 == 30 | condition5 == 30 | condition6 == 30 |
                                 condition7 == 30 | condition8 == 30 | condition9 == 30 |
                                 condition10 == 30 ), 1, NA)) %>%
  select(!starts_with("condition") & !preg_complic) %>%
  replace(is.na(.), 0)

#Note: 10 possible conditions listed in this year

labor_2004 <- bsmf_2004 %>%
  select(labor_complic) %>%
  mutate(condition1 = substr(labor_complic, 1,2), condition2 = substr(labor_complic, 3,4), 
         condition3 = substr(labor_complic, 5,6), condition4 = substr(labor_complic, 7,8),
         condition5 = substr(labor_complic, 9,10), condition6 = substr(labor_complic, 11,12),
         condition7 = substr(labor_complic, 13,14), condition8 = substr(labor_complic, 15,16),
         condition9 = substr(labor_complic, 17,18),    
         across(c(condition1:condition9), as.numeric)) %>%
  rowwise() %>%
  mutate(none_l = ifelse(c(condition1 == 0 | condition2 == 0 | condition3 == 0 |
                             condition4 == 0 | condition5 == 0 | condition6 == 0 |
                             condition7 == 0 | condition8 == 0 | condition9 == 0), 1, NA),
         preeclampsia_l = ifelse(c(condition1 == 1 | condition2 == 1 | condition3 == 1 |
                                     condition4 == 1 | condition5 == 1 | condition6 == 1 |
                                     condition7 == 1 | condition8 == 1 | condition9 == 1), 1, NA),
         eclampsia_l = ifelse(c(condition1 == 2 | condition2 == 2 | condition3 == 2 |
                                  condition4 == 2 | condition5 == 2 | condition6 == 2 |
                                  condition7 == 2 | condition8 == 2 | condition9 == 2), 1, NA),
         seizures_l = ifelse(c(condition1 == 3 | condition2 == 3 | condition3 == 3 |
                                 condition4 == 3 | condition5 == 3 | condition6 == 3 |
                                 condition7 == 3 | condition8 == 3 | condition9 == 3), 1, NA),
         fetopelvic_dis_l = ifelse(c(condition1 == 4 | condition2 == 4 | condition3 == 4 |
                                       condition4 == 4 | condition5 == 4 | condition6 == 4 |
                                       condition7 == 4 | condition8 == 4 | condition9 == 4), 1, NA),
         shoulder_dys_l = ifelse(c(condition1 == 5 | condition2 == 5 | condition3 == 5 |
                                     condition4 == 5 | condition5 == 5 | condition6 == 5 |
                                     condition7 == 5 | condition8 == 5 | condition9 == 5), 1, NA),
         breech_l = ifelse(c(condition1 == 6 | condition2 == 6 | condition3 == 6 |
                               condition4 == 6 | condition5 == 6 | condition6 == 6 |
                               condition7 == 6 | condition8 == 6 | condition9 == 6), 1, NA),
         precip_l = ifelse(c(condition1 == 7 | condition2 == 7 | condition3 == 7 |
                               condition4 == 7 | condition5 == 7 | condition6 == 7 |
                               condition7 == 7 | condition8 == 7 | condition9 == 7), 1, NA),
         prolong_l = ifelse(c(condition1 == 8 | condition2 == 8 | condition3 == 8 |
                                condition4 == 8 | condition5 == 8 | condition6 == 8 |
                                condition7 == 8 | condition8 == 8 | condition9 == 8), 1, NA),
         dysfunc_l = ifelse(c(condition1 == 9 | condition2 == 9 | condition3 == 9 |
                                condition4 == 9 | condition5 == 9 | condition6 == 9 |
                                condition7 == 9 | condition8 == 9 | condition9 == 9), 1, NA),
         premat_rupt_l = ifelse(c(condition1 == 10 | condition2 == 10 | condition3 == 10 |
                                    condition4 == 10 | condition5 == 10 | condition6 == 10 |
                                    condition7 == 10 | condition8 == 10 | condition9 == 10), 1, NA),
         induct_l = ifelse(c(condition1 == 11 | condition2 == 11 | condition3 == 11 |
                               condition4 == 11 | condition5 == 11 | condition6 == 11 |
                               condition7 == 11 | condition8 == 11 | condition9 == 11), 1, NA),
         stim_l = ifelse(c(condition1 == 12 | condition2 == 12 | condition3 == 12 |
                             condition4 == 12 | condition5 == 12 | condition6 == 12 |
                             condition7 == 12 | condition8 == 12 | condition9 == 12), 1, NA),
         abruptio_l = ifelse(c(condition1 == 13 | condition2 == 13 | condition3 == 13 |
                                 condition4 == 13 | condition5 == 13 | condition6 == 13 |
                                 condition7 == 13 | condition8 == 13 | condition9 == 13), 1, NA),
         placenta_prev_l = ifelse(c(condition1 == 14 | condition2 == 14 | condition3 == 14 |
                                      condition4 == 14 | condition5 == 14 | condition6 == 14 |
                                      condition7 == 14 | condition8 == 14 | condition9 == 14), 1, NA),
         oth_exc_bleed_l = ifelse(c(condition1 == 15 | condition2 == 15 | condition3 == 15 |
                                      condition4 == 15 | condition5 == 15 | condition6 == 15 |
                                      condition7 == 15 | condition8 == 15 | condition9 == 15), 1, NA),
         gen_herpes_l = ifelse(c(condition1 == 16 | condition2 == 16 | condition3 == 16 |
                                   condition4 == 16 | condition5 == 16 | condition6 == 16 |
                                   condition7 == 16 | condition8 == 16 | condition9 == 16), 1, NA),
         amnionitis_sepsis_l = ifelse(c(condition1 == 17 | condition2 == 17 | condition3 == 17 |
                                          condition4 == 17 | condition5 == 17 | condition6 == 17 |
                                          condition7 == 17 | condition8 == 17 | condition9 == 17), 1, NA),
         febrile_l = ifelse(c(condition1 == 18 | condition2 == 18 | condition3 == 18 |
                                condition4 == 18 | condition5 == 18 | condition6 == 18 |
                                condition7 == 18 | condition8 == 18 | condition9 == 18), 1, NA),
         meconium_l = ifelse(c(condition1 == 19 | condition2 == 19 | condition3 == 19 |
                                 condition4 == 19 | condition5 == 19 | condition6 == 19 |
                                 condition7 == 19 | condition8 == 19 | condition9 == 19), 1, NA),
         cord_prolapse_l = ifelse(c(condition1 == 20 | condition2 == 20 | condition3 == 20 |
                                      condition4 == 20 | condition5 == 20 | condition6 == 20 |
                                      condition7 == 20 | condition8 == 20 | condition9 == 20), 1, NA),
         fet_distress_l = ifelse(c(condition1 == 21 | condition2 == 21 | condition3 == 21 |
                                     condition4 == 21 | condition5 == 21 | condition6 == 21 |
                                     condition7 == 21 | condition8 == 21 | condition9 == 21), 1, NA),
         anesthetic_comp_l = ifelse(c(condition1 == 22 | condition2 == 22 | condition3 == 22 |
                                        condition4 == 22 | condition5 == 22 | condition6 == 22 |
                                        condition7 == 22 | condition8 == 22 | condition9 == 22), 1, NA),
         unsuccess_vag_l = ifelse(c(condition1 == 23 | condition2 == 23 | condition3 == 23 |
                                      condition4 == 23 | condition5 == 23 | condition6 == 23 |
                                      condition7 == 23 | condition8 == 23 | condition9 == 23), 1, NA),
         blood_trans_l = ifelse(c(condition1 == 24 | condition2 == 24 | condition3 == 24 |
                                    condition4 == 24 | condition5 == 24 | condition6 == 24 |
                                    condition7 == 24 | condition8 == 24 | condition9 == 24), 1, NA),
         mom_fac_transfer_l = ifelse(c(condition1 == 25 | condition2 == 25 | condition3 == 25 |
                                         condition4 == 25 | condition5 == 25 | condition6 == 25 |
                                         condition7 == 25 | condition8 == 25 | condition9 == 25), 1, NA),
         amniocentesis_l = ifelse(c(condition1 == 26 | condition2 == 26 | condition3 == 26 |
                                      condition4 == 26 | condition5 == 26 | condition6 == 26 |
                                      condition7 == 26 | condition8 == 26 | condition9 == 26), 1, NA),
         elec_fetal_mon_l = ifelse(c(condition1 == 27 | condition2 == 27 | condition3 == 27 |
                                       condition4 == 27 | condition5 == 27 | condition6 == 27 |
                                       condition7 == 27 | condition8 == 27 | condition9 == 27), 1, NA),
         tocolysis_l = ifelse(c(condition1 == 28 | condition2 == 28 | condition3 == 28 |
                                  condition4 == 28 | condition5 == 28 | condition6 == 28 |
                                  condition7 == 28 | condition8 == 28 | condition9 == 28), 1, NA),
         ultrasound_l = ifelse(c(condition1 == 29 | condition2 == 29 | condition3 == 29 |
                                   condition4 == 29 | condition5 == 29 | condition6 == 29 |
                                   condition7 == 29 | condition8 == 29 | condition9 == 29), 1, NA),
         mom_death_l = ifelse(c(condition1 == 30 | condition2 == 30 | condition3 == 30 |
                                  condition4 == 30 | condition5 == 30 | condition6 == 30 |
                                  condition7 == 30 | condition8 == 30 | condition9 == 30), 1, NA),
         other_l = ifelse(c(condition1 == 31 | condition2 == 31 | condition3 == 31 |
                              condition4 == 31 | condition5 == 31 | condition6 == 31 |
                              condition7 == 31 | condition8 == 31 | condition9 == 31), 1, NA)) %>%
  select(!starts_with("condition") & !labor_complic) %>%
  replace(is.na(.), 0) 

bsmf <- bind_cols(bsmf_2004, preg_2004, labor_2004)

write.csv(bsmf, file= here("data","intermediate","bsmf_2004_int.csv"))