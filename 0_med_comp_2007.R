# File: Medical complications set up - 2007
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
bsmf_2007 <- readr::read_fwf(here("data", "raw", "2007_2008","Nigra_BSMF_2007.dat"),
                             fwf_cols(ch_sex=c(58), plural=c(61), plural_ord=c(62),
                                      weight=c(63, 66), birthdate=c(67, 73), hosp_code=c(74, 77), #these are the breakspaces for where the column begins and ends
                                      birth_countyplace=c(79, 80), mom_birthplace=c(98, 99), 
                                      mom_age=c(100, 101), mom_years_edu=c(102), 
                                      mom_res_county=c(103, 104), apgar1=c(105, 106),
                                      apgar5=c(107, 108), apgar10=c(109, 110),
                                      census_tract=c(113, 118), 
                                      fath_age=c(119, 120), fath_years_edu=c(121), 
                                      leng_gestation=c(143, 145), mon_prenatal=c(146), 
                                      num_prenatal=c(147, 148), prev_livebirth_liv=c(153, 154), 
                                      prev_livebirth_dead=c(155, 156), total_liv=c(157, 158), 
                                      ch_total_ever=c(163, 164), last_liv_date=c(165, 168),  
                                      num_cigarettes=c(183,184), num_cigarettes_tri1=c(185,186),
                                      num_cigarettes_tri2=c(187,188), num_cigarettes_tri3=c(189,190),
                                      preg_complic=c(193, 224), labor_complic=c(225, 242), 
                                      mom_height=c(263, 265), mom_weight_prepreg=c(266, 268),
                                      wic_received=c(272),
                                      fath_hisp=c(285), mom_hisp=c(286), mom_zipcode=c(287, 291), 
                                      mom_state=c(292, 293), 
                                      fath_eth1=c(340, 341), fath_eth2=c(342, 343), 
                                      fath_eth3=c(344, 345), 
                                      mom_eth1=c(355, 356), mom_eth2=c(357, 358), 
                                      mom_eth3=c(359, 360), pay_prenatal=c(366, 367), 
                                      pay_delivery=c(368, 369), fath_multi_race1=c(371), 
                                      mom_multi_race1=c(372), mom_city=c(447, 481)),
                             show_col_types = FALSE) 

#1b. Deal with problematic variable that does not parse well from .dat to .csv format
bsmf_2007 <- bsmf_2007 %>% 
  mutate(last_liv_date = str_replace(last_liv_date, "(^\\s+)|(\\s+$)", ""), #get rid of breakspace at beginning and end
         last_liv_date = str_trim(last_liv_date, "both"), #get rid of whitespace
         last_liv_date = as.numeric(substr(last_liv_date, 1, 2)), #just last 2 meaningful digits
         last_liv_date = ifelse(last_liv_date < 20, str_c("200", last_liv_date), str_c("19", last_liv_date)), #trick for when to add 2000 and 1900 for year
         last_liv_date = ifelse(as.numeric(last_liv_date) > 2008, NA, last_liv_date)) #removes errors after dataset date

  #1c. Fix pregnancy and labor complications
preg_2007 <- bsmf_2007 %>%
  select(preg_complic) %>%
  mutate(condition1 = substr(preg_complic, 1,2), condition2 = substr(preg_complic, 3,4), 
         condition3 = substr(preg_complic, 5,6), condition4 = substr(preg_complic, 7,8),
         condition5 = substr(preg_complic, 9,10), condition6 = substr(preg_complic, 11,12),
         condition7 = substr(preg_complic, 13,14), condition8 = substr(preg_complic, 15,16),
         condition9 = substr(preg_complic, 17,18), condition10 = substr(preg_complic, 19,20),
         condition11 = substr(preg_complic, 21,22), condition12 = substr(preg_complic, 23,24), 
         across(c(condition1:condition12), as.numeric)) %>%
  rowwise() %>%
  mutate(none_p = ifelse(c(condition1 == 0 | condition2 == 0 | condition3 == 0 |
                             condition4 == 0 | condition5 == 0 | condition6 == 0 |
                             condition7 == 0 | condition8 == 0 | condition9 == 0 |
                             condition10 == 0 | condition11 == 0 | condition12 == 0), 1, NA),
         preeclampsia_p = ifelse(c(condition1 == 1 | condition2 == 1 | condition3 == 1 |
                                     condition4 == 1 | condition5 == 1 | condition6 == 1 |
                                     condition7 == 1 | condition8 == 1 | condition9 == 1 |
                                     condition10 == 1 | condition11 == 1 | condition12 == 1), 1, NA),
         eclampsia_p = ifelse(c(condition1 == 2 | condition2 == 2 | condition3 == 2 |
                                  condition4 == 2 | condition5 == 2 | condition6 == 2 |
                                  condition7 == 2 | condition8 == 2 | condition9 == 2 |
                                  condition10 == 2 | condition11 == 2 | condition12 == 2), 1, NA),
         hypertension_p = ifelse(c(condition1 == 3 | condition2 == 3 | condition3 == 3 |
                                     condition4 == 3 | condition5 == 3 | condition6 == 3 |
                                     condition7 == 3 | condition8 == 3 | condition9 == 3 |
                                     condition10 == 3 | condition11 == 3 | condition12 == 3), 1, NA),
         diabetes_p = ifelse(c(condition1 == 9 | condition2 == 9 | condition3 == 9 | #maybe different...pre-pregnancy?
                                 condition4 == 9 | condition5 == 9 | condition6 == 9 |
                                 condition7 == 9 | condition8 == 9 | condition9 == 9 |
                                 condition10 == 9 | condition11 == 9 | condition12 == 9), 1, NA),
         gen_herpes_p = ifelse(c(condition1 == 16 | condition2 == 16 | condition3 == 16 | #herpes simplex virus (HSV) in this round
                                   condition4 == 16 | condition5 == 16 | condition6 == 16 |
                                   condition7 == 16 | condition8 == 16 | condition9 == 16 |
                                   condition10 == 16 | condition11 == 16 | condition12 == 16), 1, NA),
         hep_b_p = ifelse(c(condition1 == 18 | condition2 == 18 | condition3 == 18 |
                              condition4 == 18 | condition5 == 18 | condition6 == 18 |
                              condition7 == 18 | condition8 == 18 | condition9 == 18 |
                              condition10 == 18 | condition11 == 18 | condition12 == 18), 1, NA),
         bel_weeks_37_p = ifelse(c(condition1 == 23 | condition2 == 23 | condition3 == 23 |
                                     condition4 == 23 | condition5 == 23 | condition6 == 23 |
                                     condition7 == 23 | condition8 == 23 | condition9 == 23 |
                                     condition10 == 23 | condition11 == 23 | condition12 == 23), 1, NA),
         cervix_cerciage_p = ifelse(c(condition1 == 24 | condition2 == 24 | condition3 == 24 |
                                        condition4 == 24 | condition5 == 24 | condition6 == 24 |
                                        condition7 == 24 | condition8 == 24 | condition9 == 24 |
                                        condition10 == 24 | condition11 == 24 | condition12 == 24), 1, NA),
         tocolysis_p = ifelse(c(condition1 == 28 | condition2 == 28 | condition3 == 28 |
                                  condition4 == 28 | condition5 == 28 | condition6 == 28 |
                                  condition7 == 28 | condition8 == 28 | condition9 == 28 |
                                  condition10 == 28 | condition11 == 28 | condition12 == 28), 1, NA),
         oth_cond_p = ifelse(c(condition1 == 30 | condition2 == 30 | condition3 == 30 |
                                 condition4 == 30 | condition5 == 30 | condition6 == 30 |
                                 condition7 == 30 | condition8 == 30 | condition9 == 30 |
                                 condition10 == 30 | condition11 == 30 | condition12 == 30), 1, NA),
         diabetes_gest_p = ifelse(c(condition1 == 31 | condition2 == 31 | condition3 == 31 |
                                      condition4 == 31 | condition5 == 31 | condition6 == 31 |
                                      condition7 == 31 | condition8 == 31 | condition9 == 31 |
                                      condition10 == 31 | condition11 == 31 | condition12 == 31), 1, NA),
         fibroids_p = ifelse(c(condition1 == 32 | condition2 == 32 | condition3 == 32 |
                                 condition4 == 32 | condition5 == 32 | condition6 == 32 |
                                 condition7 == 32 | condition8 == 32 | condition9 == 32 |
                                 condition10 == 32 | condition11 == 32 | condition12 == 32), 1, NA),
         asthma_p = ifelse(c(condition1 == 33 | condition2 == 33 | condition3 == 33 |
                               condition4 == 33 | condition5 == 33 | condition6 == 33 |
                               condition7 == 33 | condition8 == 33 | condition9 == 33 |
                               condition10 == 33 | condition11 == 33 | condition12 == 33), 1, NA),
         mult_preg_p = ifelse(c(condition1 == 34 | condition2 == 34 | condition3 == 34 |
                                  condition4 == 34 | condition5 == 34 | condition6 == 34 |
                                  condition7 == 34 | condition8 == 34 | condition9 == 34 |
                                  condition10 == 34 | condition11 == 34 | condition12 == 34), 1, NA),
         rest_interut_grow_p = ifelse(c(condition1 == 35 | condition2 == 35 | condition3 == 35 |
                                          condition4 == 35 | condition5 == 35 | condition6 == 35 |
                                          condition7 == 35 | condition8 == 35 | condition9 == 35 |
                                          condition10 == 35 | condition11 == 35 | condition12 == 35), 1, NA),
         oth_poor_preg_out_p = ifelse(c(condition1 == 36 | condition2 == 36 | condition3 == 36 |
                                          condition4 == 36 | condition5 == 36 | condition6 == 36 |
                                          condition7 == 36 | condition8 == 36 | condition9 == 36 |
                                          condition10 == 36 | condition11 == 36 | condition12 == 36), 1, NA),
         success_ext_cephalic_ver_p = ifelse(c(condition1 == 37 | condition2 == 37 | condition3 == 37 |
                                                 condition4 == 37 | condition5 == 37 | condition6 == 37 |
                                                 condition7 == 37 | condition8 == 37 | condition9 == 37 |
                                                 condition10 == 37 | condition11 == 37 | condition12 == 37), 1, NA),
         fail_ext_cephalic_ver_p = ifelse(c(condition1 == 38 | condition2 == 38 | condition3 == 38 |
                                              condition4 == 38 | condition5 == 38 | condition6 == 38 |
                                              condition7 == 38 | condition8 == 38 | condition9 == 38 |
                                              condition10 == 38 | condition11 == 38 | condition12 == 38), 1, NA),
         expert_consult_high_risk_p = ifelse(c(condition1 == 39 | condition2 == 39 | condition3 == 39 |
                                                 condition4 == 39 | condition5 == 39 | condition6 == 39 |
                                                 condition7 == 39 | condition8 == 39 | condition9 == 39 |
                                                 condition10 == 39 | condition11 == 39 | condition12 == 39), 1, NA),
         fertil_drug_insemin_p = ifelse(c(condition1 == 40 | condition2 == 40 | condition3 == 40 |
                                            condition4 == 40 | condition5 == 40 | condition6 == 40 |
                                            condition7 == 40 | condition8 == 40 | condition9 == 40 |
                                            condition10 == 40 | condition11 == 40 | condition12 == 40), 1, NA),
         assist_repro_tech_p = ifelse(c(condition1 == 41 | condition2 == 41 | condition3 == 41 |
                                          condition4 == 41 | condition5 == 41 | condition6 == 41 |
                                          condition7 == 41 | condition8 == 41 | condition9 == 41 |
                                          condition10 == 41 | condition11 == 41 | condition12 == 41), 1, NA),
         chlamydia_p = ifelse(c(condition1 == 42 | condition2 == 42 | condition3 == 42 |
                                  condition4 == 42 | condition5 == 42 | condition6 == 42 |
                                  condition7 == 42 | condition8 == 42 | condition9 == 42 |
                                  condition10 == 42 | condition11 == 42 | condition12 == 42), 1, NA),
         gonorrhea_p = ifelse(c(condition1 == 43 | condition2 == 43 | condition3 == 43 |
                                  condition4 == 43 | condition5 == 43 | condition6 == 43 |
                                  condition7 == 43 | condition8 == 43 | condition9 == 43 |
                                  condition10 == 43 | condition11 == 43 | condition12 == 43), 1, NA),
         groupb_strep_p = ifelse(c(condition1 == 44 | condition2 == 44 | condition3 == 44 |
                                     condition4 == 44 | condition5 == 44 | condition6 == 44 |
                                     condition7 == 44 | condition8 == 44 | condition9 == 44 |
                                     condition10 == 44 | condition11 == 44 | condition12 == 44), 1, NA),
         hepc_p = ifelse(c(condition1 == 45 | condition2 == 45 | condition3 == 45 |
                             condition4 == 45 | condition5 == 45 | condition6 == 45 |
                             condition7 == 45 | condition8 == 45 | condition9 == 45 |
                             condition10 == 45 | condition11 == 45 | condition12 == 45), 1, NA),
         syphilis_p = ifelse(c(condition1 == 46 | condition2 == 46 | condition3 == 46 |
                                 condition4 == 46 | condition5 == 46 | condition6 == 46 |
                                 condition7 == 46 | condition8 == 46 | condition9 == 46 |
                                 condition10 == 46 | condition11 == 46 | condition12 == 46), 1, NA),
         cytomegalovirus_p = ifelse(c(condition1 == 47 | condition2 == 47 | condition3 == 47 |
                                        condition4 == 47 | condition5 == 47 | condition6 == 47 |
                                        condition7 == 47 | condition8 == 47 | condition9 == 47 |
                                        condition10 == 47 | condition11 == 47 | condition12 == 47), 1, NA),
         listeria_p = ifelse(c(condition1 == 48 | condition2 == 48 | condition3 == 48 |
                                 condition4 == 48 | condition5 == 48 | condition6 == 48 |
                                 condition7 == 48 | condition8 == 48 | condition9 == 48 |
                                 condition10 == 48 | condition11 == 48 | condition12 == 48), 1, NA),
         parvovirus_p = ifelse(c(condition1 == 49 | condition2 == 49 | condition3 == 49 |
                                   condition4 == 49 | condition5 == 49 | condition6 == 49 |
                                   condition7 == 49 | condition8 == 49 | condition9 == 49 |
                                   condition10 == 49 | condition11 == 49 | condition12 == 49), 1, NA),
         toxoplasmosis_p = ifelse(c(condition1 == 50 | condition2 == 50 | condition3 == 50 |
                                      condition4 == 50 | condition5 == 50 | condition6 == 50 |
                                      condition7 == 50 | condition8 == 50 | condition9 == 50 |
                                      condition10 == 50 | condition11 == 50 | condition12 == 50), 1, NA),
         chlamydia_screen_p = ifelse(c(condition1 == 51 | condition2 == 51 | condition3 == 51 |
                                         condition4 == 51 | condition5 == 51 | condition6 == 51 |
                                         condition7 == 51 | condition8 == 51 | condition9 == 51 |
                                         condition10 == 51 | condition11 == 51 | condition12 == 51), 1, NA),
         gonorrhea_screen_p = ifelse(c(condition1 == 52 | condition2 == 52 | condition3 == 52 |
                                         condition4 == 52 | condition5 == 52 | condition6 == 52 |
                                         condition7 == 52 | condition8 == 52 | condition9 == 52 |
                                         condition10 == 52 | condition11 == 52 | condition12 == 52), 1, NA),
         groupb_strep_screen_p = ifelse(c(condition1 == 53 | condition2 == 53 | condition3 == 53 |
                                            condition4 == 53 | condition5 == 53 | condition6 == 53 |
                                            condition7 == 53 | condition8 == 53 | condition9 == 53 |
                                            condition10 == 53 | condition11 == 53 | condition12 == 53), 1, NA),
         hepb_screen_p = ifelse(c(condition1 == 54 | condition2 == 54 | condition3 == 54 |
                                    condition4 == 54 | condition5 == 54 | condition6 == 54 |
                                    condition7 == 54 | condition8 == 54 | condition9 == 54 |
                                    condition10 == 54 | condition11 == 54 | condition12 == 54), 1, NA),
         hiv_screen_p = ifelse(c(condition1 == 55 | condition2 == 55 | condition3 == 55 |
                                   condition4 == 55 | condition5 == 55 | condition6 == 55 |
                                   condition7 == 55 | condition8 == 55 | condition9 == 55 |
                                   condition10 == 55 | condition11 == 55 | condition12 == 55), 1, NA),
         syphilis_screen_p = ifelse(c(condition1 == 56 | condition2 == 56 | condition3 == 56 |
                                        condition4 == 56 | condition5 == 56 | condition6 == 56 |
                                        condition7 == 56 | condition8 == 56 | condition9 == 56 |
                                        condition10 == 56 | condition11 == 56 | condition12 == 56), 1, NA)) %>%
  select(!starts_with("condition") & !preg_complic) %>%
  replace(is.na(.), 0)

#Note: 12 possible conditions listed in this year

labor_2007 <- bsmf_2007 %>%
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
         precip_l = ifelse(c(condition1 == 7 | condition2 == 7 | condition3 == 7 |
                               condition4 == 7 | condition5 == 7 | condition6 == 7 |
                               condition7 == 7 | condition8 == 7 | condition9 == 7), 1, NA),
         prolong_l = ifelse(c(condition1 == 8 | condition2 == 8 | condition3 == 8 |
                                condition4 == 8 | condition5 == 8 | condition6 == 8 |
                                condition7 == 8 | condition8 == 8 | condition9 == 8), 1, NA),
         premat_rupt_l = ifelse(c(condition1 == 10 | condition2 == 10 | condition3 == 10 |
                                    condition4 == 10 | condition5 == 10 | condition6 == 10 |
                                    condition7 == 10 | condition8 == 10 | condition9 == 10), 1, NA),
         induct_l = ifelse(c(condition1 == 11 | condition2 == 11 | condition3 == 11 |
                               condition4 == 11 | condition5 == 11 | condition6 == 11 |
                               condition7 == 11 | condition8 == 11 | condition9 == 11), 1, NA),
         stim_l = ifelse(c(condition1 == 12 | condition2 == 12 | condition3 == 12 | #augmentation of labor? question if it is the same
                             condition4 == 12 | condition5 == 12 | condition6 == 12 |
                             condition7 == 12 | condition8 == 12 | condition9 == 12), 1, NA),
         abruptio_l = ifelse(c(condition1 == 13 | condition2 == 13 | condition3 == 13 |
                                 condition4 == 13 | condition5 == 13 | condition6 == 13 |
                                 condition7 == 13 | condition8 == 13 | condition9 == 13), 1, NA),
         amnionitis_sepsis_l = ifelse(c(condition1 == 17 | condition2 == 17 | condition3 == 17 | #Chorioamnionitis same thing?
                                          condition4 == 17 | condition5 == 17 | condition6 == 17 |
                                          condition7 == 17 | condition8 == 17 | condition9 == 17), 1, NA),
         meconium_l = ifelse(c(condition1 == 19 | condition2 == 19 | condition3 == 19 |
                                 condition4 == 19 | condition5 == 19 | condition6 == 19 |
                                 condition7 == 19 | condition8 == 19 | condition9 == 19), 1, NA),
         cord_prolapse_l = ifelse(c(condition1 == 20 | condition2 == 20 | condition3 == 20 |
                                      condition4 == 20 | condition5 == 20 | condition6 == 20 |
                                      condition7 == 20 | condition8 == 20 | condition9 == 20), 1, NA),
         blood_trans_l = ifelse(c(condition1 == 24 | condition2 == 24 | condition3 == 24 |
                                    condition4 == 24 | condition5 == 24 | condition6 == 24 |
                                    condition7 == 24 | condition8 == 24 | condition9 == 24), 1, NA),
         mom_fac_transfer_l = ifelse(c(condition1 == 25 | condition2 == 25 | condition3 == 25 |
                                         condition4 == 25 | condition5 == 25 | condition6 == 25 |
                                         condition7 == 25 | condition8 == 25 | condition9 == 25), 1, NA),
         other_l = ifelse(c(condition1 == 31 | condition2 == 31 | condition3 == 31 |
                              condition4 == 31 | condition5 == 31 | condition6 == 31 |
                              condition7 == 31 | condition8 == 31 | condition9 == 31), 1, NA),
         nonvertex_pres_l = ifelse(c(condition1 == 32 | condition2 == 32 | condition3 == 32 |
                                       condition4 == 32 | condition5 == 32 | condition6 == 32 |
                                       condition7 == 32 | condition8 == 32 | condition9 == 32), 1, NA),
         steroids_fetal_lung_l = ifelse(c(condition1 == 33 | condition2 == 33 | condition3 == 33 |
                                            condition4 == 33 | condition5 == 33 | condition6 == 33 |
                                            condition7 == 33 | condition8 == 33 | condition9 == 33), 1, NA),
         antibiotics_l = ifelse(c(condition1 == 34 | condition2 == 34 | condition3 == 34 |
                                    condition4 == 34 | condition5 == 34 | condition6 == 34 |
                                    condition7 == 34 | condition8 == 34 | condition9 == 34), 1, NA),
         chorioamnionitis_l = ifelse(c(condition1 == 35 | condition2 == 35 | condition3 == 35 |
                                         condition4 == 35 | condition5 == 35 | condition6 == 35 |
                                         condition7 == 35 | condition8 == 35 | condition9 == 35), 1, NA),
         fetal_intol_l = ifelse(c(condition1 == 36 | condition2 == 36 | condition3 == 36 |
                                    condition4 == 36 | condition5 == 36 | condition6 == 36 |
                                    condition7 == 36 | condition8 == 36 | condition9 == 36), 1, NA),
         epidural_anes_l = ifelse(c(condition1 == 37 | condition2 == 37 | condition3 == 37 |
                                      condition4 == 37 | condition5 == 37 | condition6 == 37 |
                                      condition7 == 37 | condition8 == 37 | condition9 == 37), 1, NA),
         mem_rupture_l = ifelse(c(condition1 == 38 | condition2 == 38 | condition3 == 38 |
                                    condition4 == 38 | condition5 == 38 | condition6 == 38 |
                                    condition7 == 38 | condition8 == 38 | condition9 == 38), 1, NA),
         placenta_insuf_l = ifelse(c(condition1 == 39 | condition2 == 39 | condition3 == 39 |
                                       condition4 == 39 | condition5 == 39 | condition6 == 39 |
                                       condition7 == 39 | condition8 == 39 | condition9 == 39), 1, NA),
         perineal_lacer_l = ifelse(c(condition1 == 40 | condition2 == 40 | condition3 == 40 |
                                       condition4 == 40 | condition5 == 40 | condition6 == 40 |
                                       condition7 == 40 | condition8 == 40 | condition9 == 40), 1, NA),
         rupture_uterus_l = ifelse(c(condition1 == 41 | condition2 == 41 | condition3 == 41 |
                                       condition4 == 41 | condition5 == 41 | condition6 == 41 |
                                       condition7 == 41 | condition8 == 41 | condition9 == 41), 1, NA),
         unplan_hysterect_l = ifelse(c(condition1 == 42 | condition2 == 42 | condition3 == 42 |
                                         condition4 == 42 | condition5 == 42 | condition6 == 42 |
                                         condition7 == 42 | condition8 == 42 | condition9 == 42), 1, NA),
         admiss_icu_l = ifelse(c(condition1 == 43 | condition2 == 43 | condition3 == 43 |
                                   condition4 == 43 | condition5 == 43 | condition6 == 43 |
                                   condition7 == 43 | condition8 == 43 | condition9 == 43), 1, NA),
         unplanned_operat_l = ifelse(c(condition1 == 44 | condition2 == 44 | condition3 == 44 |
                                         condition4 == 44 | condition5 == 44 | condition6 == 44 |
                                         condition7 == 44 | condition8 == 44 | condition9 == 44), 1, NA)) %>%
  select(!starts_with("condition") & !labor_complic) %>%
  replace(is.na(.), 0) 

bsmf <- bind_cols(bsmf_2007, preg_2007, labor_2007)

write.csv(bsmf, file= here("data","intermediate","bsmf_2007_int.csv"))