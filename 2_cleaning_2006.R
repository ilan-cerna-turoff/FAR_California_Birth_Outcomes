# File: Cleaning 2006
# Date: 11/24/2023
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","naniar","here","haven")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
#1a. read in files
df <- read.csv2(here("data", "intermediate", "bsmf_2006.csv"), header = T, sep = ",") 

#1b. clean and make comparable
df <- df %>%
  mutate(across(c(ch_sex, plural, plural_ord, fath_hisp, 
                  mom_hisp, fath_multi_race1, mom_multi_race1), ~na_if(., 9)), #clean up missingness
         weight = if_else(weight == 9999, NA, 
                          if_else(weight == 9998, NA, weight)), 
         hosp_code = if_else(hosp_code == 999, NA, hosp_code), #Note: 9999 out of state, 0 home, 998 in transit
         across(c(mom_age, mom_years_edu, fath_age, fath_years_edu, num_prenatal, 
                  prev_livebirth_liv, prev_livebirth_dead, total_liv, ch_total_ever, 
                  pay_prenatal, pay_delivery, fath_eth1, mom_eth1), ~na_if(., 99)),
         mon_prenatal = if_else(mon_prenatal == "-", NA, mon_prenatal), 
         census_tract = if_else(census_tract == "<UNK>0", NA, 
                                if_else(census_tract == "999999", NA, census_tract)), 
         ch_total_ever = if_else(ch_total_ever == 95, NA, ch_total_ever), #error in total children ever given other responses
         across(c(ch_total_ever, fath_eth1, mom_eth1), ~na_if(., 98)),
         mom_zipcode = if_else(mom_zipcode < 90000 | mom_zipcode > 96162, NA, mom_zipcode), 
         mom_city = if_else(mom_city == "-", NA, 
                            if_else(mom_city == ".", NA,
                                    if_else(mom_city == "\\", NA, mom_city))), 
         mom_city = str_to_lower(mom_city)) %>%  #make cities lowercase for cleaning
  filter(mom_state == "5" & str_detect(birth_countyplace, "\\d")) %>% #restrict to birth parent who resides in California & child born in California 
  mutate(birthdate = as.character(birthdate),
         birthdate = as.Date(birthdate, format="%Y%m%d"), #format dates 
         diff_last_liv_date = year - last_liv_date) %>% 
  select(-c(mom_state,last_liv_date)) %>% #get rid of variable not using
  filter(!is.na(mom_res_county) & !is.na(mom_zipcode)) %>% #remove all missing county and zipcode 
  mutate(across(c(mom_res_county, birth_countyplace), as.numeric),
         across(c(mom_res_county, birth_countyplace), as.factor)) %>% #clean up order
  filter(!is.na(birthdate)) %>% #need observations with child's birthdate
  mutate(mom_birthplace = case_when(mom_birthplace == "AK" ~ "Alaska",
                                    mom_birthplace == "AL" ~ "Alabama",
                                    mom_birthplace == "AR" ~ "Arkansas",
                                    mom_birthplace == "AZ" ~ "Arizona",
                                    mom_birthplace == "CA" ~ "California",
                                    mom_birthplace == "CH" ~ "China",
                                    mom_birthplace == "CN" ~ "Canada",
                                    mom_birthplace == "CO" ~ "Colorado",
                                    mom_birthplace == "CT" ~ "Connecticut",
                                    mom_birthplace == "CU" ~ "Cuba",
                                    mom_birthplace == "DC" ~ "Washington DC",
                                    mom_birthplace == "DE" ~ "Delaware",
                                    mom_birthplace == "FL" ~ "Florida",
                                    mom_birthplace == "GA" ~ "Georgia",
                                    mom_birthplace == "GU" ~ "Guam",
                                    mom_birthplace == "HI" ~ "Hawaii",
                                    mom_birthplace == "IA" ~ "Iowa",
                                    mom_birthplace == "ID" ~ "Idaho",
                                    mom_birthplace == "IL" ~ "Illinois",
                                    mom_birthplace == "IN" ~ "Indiana",
                                    mom_birthplace == "JA" ~ "Japan",
                                    mom_birthplace == "KS" ~ "Kansas",
                                    mom_birthplace == "KY" ~ "Kentucky",
                                    mom_birthplace == "LA" ~ "Louisiana",
                                    mom_birthplace == "MA" ~ "Massachusetts",
                                    mom_birthplace == "MD" ~ "Maryland",
                                    mom_birthplace == "ME" ~ "Maine",
                                    mom_birthplace == "MI" ~ "Michigan",
                                    mom_birthplace == "MN" ~ "Minnesota",
                                    mom_birthplace == "MO" ~ "Missouri",
                                    mom_birthplace == "MS" ~ "Mississippi",
                                    mom_birthplace == "MT" ~ "Montana",
                                    mom_birthplace == "MX" ~ "Mexico",
                                    mom_birthplace == "NB" ~ "Nebraska",
                                    mom_birthplace == "NC" ~ "North Carolina",
                                    mom_birthplace == "ND" ~ "North Dakota",
                                    mom_birthplace == "NH" ~ "New Hampshire",
                                    mom_birthplace == "NJ" ~ "New Jersey",
                                    mom_birthplace == "NM" ~ "New Mexico",
                                    mom_birthplace == "NV" ~ "Nevada",
                                    mom_birthplace == "NY" ~ "New York",
                                    mom_birthplace == "OH" ~ "Ohio",
                                    mom_birthplace == "OK" ~ "Oklahoma",
                                    mom_birthplace == "OR" ~ "Oregon",
                                    mom_birthplace == "PA" ~ "Pennsylvania",
                                    mom_birthplace == "PI" ~ "Philipines",
                                    mom_birthplace == "PR" ~ "Puerto Rico",
                                    mom_birthplace == "RE" ~ "Rest of the World",
                                    mom_birthplace == "RI" ~ "Rhode Island",
                                    mom_birthplace == "SC" ~ "South Carolina",
                                    mom_birthplace == "SD" ~ "South Dakota",
                                    mom_birthplace == "TN" ~ "Tennessee",
                                    mom_birthplace == "TX" ~ "Texas",
                                    mom_birthplace == "UT" ~ "Utah",
                                    mom_birthplace == "VA" ~ "Virginia",
                                    mom_birthplace == "VI" ~ "Virgin Islands",
                                    mom_birthplace == "VN" ~ "Vietnam",
                                    mom_birthplace == "VT" ~ "Vermont",
                                    mom_birthplace == "WA" ~ "Washington",
                                    mom_birthplace == "WI" ~ "Wisconsin",
                                    mom_birthplace == "WV" ~ "West Virginia",
                                    mom_birthplace == "WY" ~ "Wyoming",
                                    TRUE ~ .$mom_birthplace))

#1c. make comparable
df <- df %>%
  mutate(mom_years_edu = if_else(mom_years_edu > 3 & mom_years_edu < 9 & !is.na(mom_years_edu), 3,
                                 if_else(mom_years_edu == 9, NA, mom_years_edu)),
         fath_years_edu = if_else(fath_years_edu > 3 & fath_years_edu < 9 & !is.na(fath_years_edu), 3,
                                  if_else(fath_years_edu == 9, NA, fath_years_edu)),
         fath_hisp = if_else(fath_hisp == 8 | fath_hisp == 5 & !is.na(fath_hisp), 6, fath_hisp), 
         mom_hisp = if_else(mom_hisp == 8 | mom_hisp == 5 & !is.na(mom_hisp), 6, mom_hisp))

# Note: need to collapse mom_years_edu and fath_years_edu to be comparable with 2006 and after data. 
# Created 8th grade or below, high school, or above high school levels of education

#1d. select variables that are used across years
df <- df %>%
  select(-c(X, X.1, plural_ord, mon_prenatal, diabetes_gest_p, fibroids_p, asthma_p, 
            mult_preg_p, rest_interut_grow_p, oth_poor_preg_out_p, success_ext_cephalic_ver_p,
            fail_ext_cephalic_ver_p, expert_consult_high_risk_p, fertil_drug_insemin_p, 
            assist_repro_tech_p, chlamydia_p, gonorrhea_p, groupb_strep_p, hepc_p, 
            syphilis_p, cytomegalovirus_p, listeria_p, parvovirus_p, toxoplasmosis_p,
            chlamydia_screen_p, gonorrhea_screen_p, groupb_strep_screen_p, hepb_screen_p,
            hiv_screen_p, syphilis_screen_p, nonvertex_pres_l, steroids_fetal_lung_l, 
            antibiotics_l, chorioamnionitis_l, fetal_intol_l, epidural_anes_l, 
            mem_rupture_l, placenta_insuf_l, perineal_lacer_l, rupture_uterus_l,
            unplan_hysterect_l, admiss_icu_l, unplanned_operat_l, preg_complic, 
            labor_complic, mom_city))
  
write.csv2(df, file= here("data","intermediate","bsmf_2006_int2.csv"))
