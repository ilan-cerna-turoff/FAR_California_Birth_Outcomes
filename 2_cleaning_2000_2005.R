# File: Cleaning 2000-2005
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
df <- read.csv2(here("data", "intermediate", "bsmf_2000_2005.csv"), header = T, sep = ",") 

#1b. clean 
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
  mutate(mom_years_edu = if_else(mom_years_edu < 9 & !is.na(mom_years_edu), 1, 
                                 if_else(mom_years_edu > 8 & mom_years_edu < 13 & !is.na(mom_years_edu), 2, 3)),
         fath_years_edu = if_else(fath_years_edu < 9 & !is.na(fath_years_edu), 1, 
                                 if_else(fath_years_edu > 8 & fath_years_edu < 13 & !is.na(fath_years_edu), 2, 3)),
         fath_hisp = if_else(fath_hisp == 8 | fath_hisp == 5 & !is.na(fath_hisp), 6, fath_hisp), 
         mom_hisp = if_else(mom_hisp == 8 | mom_hisp == 5 & !is.na(mom_hisp), 6, mom_hisp))

# Note: need to collapse mom_years_edu and fath_years_edu to be comparable with 2006 and after data. 
# Created 8th grade or below, high school, or above high school levels of education

#1d. select variables that are used across years
df <- df %>%
  select(-c(X, X.1, plural_ord, mon_prenatal, renal_dis_p, pyelonephritis_p, anemia_p,
            cardiac_dis_p, lung_dis_p, rh_sensitization_p, hemoglobinopathy_p,
            uter_bleeding_p, poly_oligohydramnios_p, incomp_cervix_p, premat_lab_p,
            oth_std_p, rubella_p, tobac_p, above_gram_4000_p, bel_gram_2500_p,
            chor_vilius_p, amniocentesis_p, elec_fetal_mon_p, ultrasound_p, 
            preeclampsia_l, eclampsia_l, seizures_l, fetopelvic_dis_l, shoulder_dys_l,
            breech_l, dysfunc_l, placenta_prev_l, oth_exc_bleed_l, gen_herpes_l, 
            febrile_l, fet_distress_l, anesthetic_comp_l, unsuccess_vag_l, 
            amniocentesis_l, elec_fetal_mon_l, tocolysis_l, ultrasound_l, 
            mom_death_l, mom_city))

write.csv2(df, file= here("data","intermediate","bsmf_2000_2005_int2.csv"))

