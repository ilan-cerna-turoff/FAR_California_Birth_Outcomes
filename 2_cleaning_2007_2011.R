# File: Cleaning 2007-2017
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
df1 <- read.csv2(here("data", "intermediate", "bsmf_2007_2008.csv"), header = T, sep = ",")
df2 <- read.csv2(here("data", "intermediate", "bsmf_2009_2010.csv"), header = T, sep = ",")
df3 <- read.csv2(here("data", "intermediate", "bsmf_2011.csv"), header = T, sep = ",") 

df3 <- df3 %>%
  filter(birth_countyplace < 59) %>%
  mutate(birth_countyplace = as.character(birth_countyplace)) #create comparability due to automatic read in functions in R

df <- bind_rows(df1,df2,df3)

rm(df1,df2,df3)

#1b. clean and make comparable
df <- df %>%
  mutate(across(c(ch_sex, plural, plural_ord, fath_hisp, 
                  mom_hisp, fath_multi_race1, mom_multi_race1), ~na_if(., 9)), #clean up missingness
         weight = if_else(weight == 9999, NA, 
                          if_else(weight == 9998, NA, weight)), 
         hosp_code = if_else(hosp_code == 999 | hosp_code == 996, NA, hosp_code), #Note: 9999 out of state, 0 home, 998 in transit
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
         mom_city = str_to_lower(mom_city),
         num_cigarettes = if_else(num_cigarettes == 99, NA, num_cigarettes),
         num_cigarettes_tri1 = if_else(num_cigarettes_tri1 == 99, NA, num_cigarettes_tri1),
         num_cigarettes_tri2 = if_else(num_cigarettes_tri2 == 99, NA, num_cigarettes_tri2),
         num_cigarettes_tri3 = if_else(num_cigarettes_tri3 == 99, NA, num_cigarettes_tri3),
         apgar1 = if_else(apgar1 == 99, NA, apgar1),
         apgar5 = if_else(apgar5 == 99, NA, apgar5),
         apgar10 = if_else(apgar10 == 99, NA, apgar10),
         mom_height = if_else(mom_height == 999, NA, mom_height),
         mom_weight_prepreg = if_else(mom_weight_prepreg == 999, NA, mom_weight_prepreg),
         wic_received = if_else(wic_received == "U", NA, wic_received)) %>%  #make cities lowercase for cleaning
  filter(mom_state == "5" & str_detect(birth_countyplace, "\\d")) %>% #restrict to birth parent who resides in California & child born in California 
  mutate(birthdate = as.character(birthdate),
         birthdate = as.Date(birthdate, format="%Y%m%d"), #format dates 
         diff_last_liv_date = year - last_liv_date) %>% 
  select(-c(mom_state,last_liv_date)) %>% #get rid of variable not using
  filter(!is.na(mom_res_county) & !is.na(mom_zipcode)) %>% #remove all missing county and zipcode 
  mutate(across(c(mom_res_county, birth_countyplace), as.numeric),
         across(c(mom_res_county, birth_countyplace), as.factor), #clean up order
         mom_height = if_else(mom_height == 900 | mom_height == 902 | 
                                mom_height == 903 | mom_height == 910, NA, mom_height),
         mom_height_in = mom_height*12) %>% #convert to inches
  select(-mom_height) %>%
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
                                    mom_birthplace == "101" ~ "Alabama",
                                    mom_birthplace == "102" ~ "Alaska",
                                    mom_birthplace == "103" ~ "Arizona",
                                    mom_birthplace == "104" ~ "Arkansas",
                                    mom_birthplace == "105" ~ "California",
                                    mom_birthplace == "106" ~ "Colorado",
                                    mom_birthplace == "107" ~ "Connecticut",
                                    mom_birthplace == "108" ~ "Delaware",
                                    mom_birthplace == "109" ~ "Washington DC",
                                    mom_birthplace == "110" ~ "Florida",
                                    mom_birthplace == "111" ~ "Georgia",
                                    mom_birthplace == "112" ~ "Hawaii",
                                    mom_birthplace == "113" ~ "Idaho",
                                    mom_birthplace == "114" ~ "Illinois",
                                    mom_birthplace == "115" ~ "Indiana",
                                    mom_birthplace == "116" ~ "Iowa",
                                    mom_birthplace == "117" ~ "Kansas",
                                    mom_birthplace == "118" ~ "Kentucky",
                                    mom_birthplace == "119" ~ "Louisiana",
                                    mom_birthplace == "120" ~ "Maine",
                                    mom_birthplace == "121" ~ "Maryland",
                                    mom_birthplace == "122" ~ "Massachusetts",
                                    mom_birthplace == "123" ~ "Michigan",
                                    mom_birthplace == "124" ~ "Minnesota",
                                    mom_birthplace == "125" ~ "Mississippi",
                                    mom_birthplace == "126" ~ "Missouri",
                                    mom_birthplace == "127" ~ "Montana",
                                    mom_birthplace == "128" ~ "Nebraska",
                                    mom_birthplace == "129" ~ "Nevada",
                                    mom_birthplace == "130" ~ "New Hampshire",
                                    mom_birthplace == "131" ~ "New Jersey",
                                    mom_birthplace == "132" ~ "New Mexico",
                                    mom_birthplace == "133" ~ "New York",
                                    mom_birthplace == "134" ~ "North Carolina",
                                    mom_birthplace == "135" ~ "North Dakota",
                                    mom_birthplace == "136" ~ "Ohio",
                                    mom_birthplace == "137" ~ "Oklahoma",
                                    mom_birthplace == "138" ~ "Oregon",
                                    mom_birthplace == "139" ~ "Pennsylvania",
                                    mom_birthplace == "140" ~ "Rhode Island",
                                    mom_birthplace == "141" ~ "South Carolina",
                                    mom_birthplace == "142" ~ "South Dakota",
                                    mom_birthplace == "143" ~ "Tennessee",
                                    mom_birthplace == "144" ~ "Texas",
                                    mom_birthplace == "145" ~ "Utah",
                                    mom_birthplace == "146" ~ "Vermont",
                                    mom_birthplace == "147" ~ "Virginia",
                                    mom_birthplace == "148" ~ "Washington",
                                    mom_birthplace == "149" ~ "West Virginia",
                                    mom_birthplace == "150" ~ "Wisconsin",
                                    mom_birthplace == "151" ~ "Wyoming",
                                    mom_birthplace == "AA" ~ "Rest of the World",
                                    mom_birthplace == "ABC" ~ "Canada",
                                    mom_birthplace == "AE" ~ "Rest of the World",
                                    mom_birthplace == "AF" ~ "Rest of the World",
                                    mom_birthplace == "AG" ~ "Rest of the World",
                                    mom_birthplace == "AI" ~ "Rest of the World",
                                    mom_birthplace == "AJ" ~ "Rest of the World",
                                    mom_birthplace == "AM" ~ "Rest of the World",
                                    mom_birthplace == "AN" ~ "Rest of the World",
                                    mom_birthplace == "AO" ~ "Rest of the World",
                                    mom_birthplace == "AQ" ~ "Rest of the World",
                                    mom_birthplace == "AS" ~ "Rest of the World",
                                    mom_birthplace == "AT" ~ "Rest of the World",
                                    mom_birthplace == "AU" ~ "Rest of the World",
                                    mom_birthplace == "AW" ~ "Rest of the World",
                                    mom_birthplace == "BA" ~ "Rest of the World",
                                    mom_birthplace == "BB" ~ "Rest of the World",
                                    mom_birthplace == "BCC" ~ "Canada",
                                    mom_birthplace == "BD" ~ "Rest of the World",
                                    mom_birthplace == "BE" ~ "Rest of the World",
                                    mom_birthplace == "BF" ~ "Rest of the World",
                                    mom_birthplace == "BG" ~ "Rest of the World",
                                    mom_birthplace == "BH" ~ "Rest of the World",
                                    mom_birthplace == "BL" ~ "Rest of the World",
                                    mom_birthplace == "BM" ~ "Rest of the World",
                                    mom_birthplace == "BN" ~ "Rest of the World",
                                    mom_birthplace == "BO" ~ "Rest of the World",
                                    mom_birthplace == "BP" ~ "Rest of the World",
                                    mom_birthplace == "BR" ~ "Rest of the World",
                                    mom_birthplace == "BS" ~ "Rest of the World",
                                    mom_birthplace == "BT" ~ "Rest of the World",
                                    mom_birthplace == "BU" ~ "Rest of the World",
                                    mom_birthplace == "BW" ~ "Rest of the World",
                                    mom_birthplace == "BX" ~ "Rest of the World",
                                    mom_birthplace == "CB" ~ "Rest of the World",
                                    mom_birthplace == "CC" ~ "China",
                                    mom_birthplace == "CD" ~ "Rest of the World",
                                    mom_birthplace == "CE" ~ "Rest of the World",
                                    mom_birthplace == "CF" ~ "Rest of the World",
                                    mom_birthplace == "CG" ~ "Rest of the World",
                                    mom_birthplace == "CI" ~ "Rest of the World",
                                    mom_birthplace == "CJ" ~ "Rest of the World",
                                    mom_birthplace == "CK" ~ "Rest of the World",
                                    mom_birthplace == "CL" ~ "Rest of the World",
                                    mom_birthplace == "CM" ~ "Rest of the World",
                                    mom_birthplace == "CQ" ~ "Rest of the World",
                                    mom_birthplace == "CR" ~ "Rest of the World",
                                    mom_birthplace == "CV" ~ "Rest of the World",
                                    mom_birthplace == "CW" ~ "Rest of the World",
                                    mom_birthplace == "CX" ~ "Rest of the World",
                                    mom_birthplace == "CY" ~ "Rest of the World",
                                    mom_birthplace == "DK" ~ "Rest of the World",
                                    mom_birthplace == "DM" ~ "Rest of the World",
                                    mom_birthplace == "DQ" ~ "Rest of the World",
                                    mom_birthplace == "DR" ~ "Rest of the World",
                                    mom_birthplace == "EA" ~ "Rest of the World",
                                    mom_birthplace == "EC" ~ "Rest of the World",
                                    mom_birthplace == "EG" ~ "Rest of the World",
                                    mom_birthplace == "ENK" ~ "Rest of the World",
                                    mom_birthplace == "ER" ~ "Rest of the World",
                                    mom_birthplace == "ES" ~ "Rest of the World",
                                    mom_birthplace == "ET" ~ "Rest of the World",
                                    mom_birthplace == "FA" ~ "Rest of the World",
                                    mom_birthplace == "FG" ~ "Rest of the World",
                                    mom_birthplace == "FI" ~ "Rest of the World",
                                    mom_birthplace == "FJ" ~ "Rest of the World",
                                    mom_birthplace == "FK" ~ "Rest of the World",
                                    mom_birthplace == "FM" ~ "Rest of the World",
                                    mom_birthplace == "FP" ~ "Rest of the World",
                                    mom_birthplace == "FR" ~ "Rest of the World",
                                    mom_birthplace == "FT" ~ "Rest of the World",
                                    mom_birthplace == "GB" ~ "Rest of the World",
                                    mom_birthplace == "GD" ~ "Rest of the World",
                                    mom_birthplace == "GH" ~ "Rest of the World",
                                    mom_birthplace == "GI" ~ "Rest of the World",
                                    mom_birthplace == "GM" ~ "Rest of the World",
                                    mom_birthplace == "GO" ~ "Rest of the World",
                                    mom_birthplace == "GP" ~ "Rest of the World",
                                    mom_birthplace == "GR" ~ "Rest of the World",
                                    mom_birthplace == "GS" ~ "Rest of the World",
                                    mom_birthplace == "GT" ~ "Rest of the World",
                                    mom_birthplace == "GV" ~ "Rest of the World",
                                    mom_birthplace == "GW" ~ "Rest of the World",
                                    mom_birthplace == "GY" ~ "Rest of the World",
                                    mom_birthplace == "GZ" ~ "Rest of the World",
                                    mom_birthplace == "HK" ~ "Rest of the World",
                                    mom_birthplace == "HM" ~ "Rest of the World",
                                    mom_birthplace == "HO" ~ "Rest of the World",
                                    mom_birthplace == "HT" ~ "Rest of the World",
                                    mom_birthplace == "HU" ~ "Rest of the World",
                                    mom_birthplace == "IC" ~ "Rest of the World",
                                    mom_birthplace == "IE" ~ "Rest of the World",
                                    mom_birthplace == "II" ~ "Rest of the World",
                                    mom_birthplace == "IO" ~ "Rest of the World",
                                    mom_birthplace == "IQ" ~ "Rest of the World",
                                    mom_birthplace == "IR" ~ "Rest of the World",
                                    mom_birthplace == "IS" ~ "Rest of the World",
                                    mom_birthplace == "IT" ~ "Rest of the World",
                                    mom_birthplace == "IV" ~ "Rest of the World",
                                    mom_birthplace == "JM" ~ "Rest of the World",
                                    mom_birthplace == "JO" ~ "Rest of the World",
                                    mom_birthplace == "KE" ~ "Rest of the World",
                                    mom_birthplace == "KG" ~ "Rest of the World",
                                    mom_birthplace == "KN" ~ "Rest of the World",
                                    mom_birthplace == "KO" ~ "Rest of the World",
                                    mom_birthplace == "KU" ~ "Rest of the World",
                                    mom_birthplace == "KZ" ~ "Rest of the World",
                                    mom_birthplace == "LB" ~ "Rest of the World",
                                    mom_birthplace == "LE" ~ "Rest of the World",
                                    mom_birthplace == "LI" ~ "Rest of the World",
                                    mom_birthplace == "LO" ~ "Rest of the World",
                                    mom_birthplace == "LS" ~ "Rest of the World",
                                    mom_birthplace == "LU" ~ "Rest of the World",
                                    mom_birthplace == "LV" ~ "Rest of the World",
                                    mom_birthplace == "LY" ~ "Rest of the World",
                                    mom_birthplace == "MBC" ~ "Canada",
                                    mom_birthplace == "MC" ~ "Rest of the World",
                                    mom_birthplace == "MF" ~ "Rest of the World",
                                    mom_birthplace == "MG" ~ "Rest of the World",
                                    mom_birthplace == "MH" ~ "Rest of the World",
                                    mom_birthplace == "MJ" ~ "Rest of the World",
                                    mom_birthplace == "MK" ~ "Rest of the World",
                                    mom_birthplace == "ML" ~ "Rest of the World",
                                    mom_birthplace == "MM" ~ "Rest of the World",
                                    mom_birthplace == "MP" ~ "Rest of the World",
                                    mom_birthplace == "MQ" ~ "Rest of the World",
                                    mom_birthplace == "MR" ~ "Rest of the World",
                                    mom_birthplace == "MU" ~ "Rest of the World",
                                    mom_birthplace == "MV" ~ "Rest of the World",
                                    mom_birthplace == "MW" ~ "Rest of the World",
                                    mom_birthplace == "MY" ~ "Rest of the World",
                                    mom_birthplace == "MZ" ~ "Rest of the World",
                                    mom_birthplace == "NFC" ~ "Canada",
                                    mom_birthplace == "NG" ~ "Rest of the World",
                                    mom_birthplace == "NIK" ~ "Rest of the World",
                                    mom_birthplace == "NKC" ~ "Canada",
                                    mom_birthplace == "NL" ~ "Rest of the World",
                                    mom_birthplace == "NN" ~ NA,
                                    mom_birthplace == "NO" ~ "Rest of the World",
                                    mom_birthplace == "NP" ~ "Rest of the World",
                                    mom_birthplace == "NQ" ~ "Rest of the World",
                                    mom_birthplace == "NR" ~ "Rest of the World",
                                    mom_birthplace == "NSC" ~ "Canada",
                                    mom_birthplace == "NTC" ~ "Canada",
                                    mom_birthplace == "NU" ~ "Rest of the World",
                                    mom_birthplace == "NW" ~ "Rest of the World",
                                    mom_birthplace == "NZ" ~ "Rest of the World",
                                    mom_birthplace == "ONC" ~ "Canada",
                                    mom_birthplace == "PE" ~ "Rest of the World",
                                    mom_birthplace == "PG" ~ "Rest of the World",
                                    mom_birthplace == "PH" ~ "Philipines",
                                    mom_birthplace == "PIC" ~ "Canada",
                                    mom_birthplace == "PK" ~ "Rest of the World",
                                    mom_birthplace == "PL" ~ "Rest of the World",
                                    mom_birthplace == "PN" ~ "Rest of the World",
                                    mom_birthplace == "PO" ~ "Rest of the World",
                                    mom_birthplace == "PP" ~ "Rest of the World",
                                    mom_birthplace == "PW" ~ "Rest of the World",
                                    mom_birthplace == "PY" ~ "Rest of the World",
                                    mom_birthplace == "QA" ~ "Rest of the World",
                                    mom_birthplace == "QUC" ~ "Canada",
                                    mom_birthplace == "RB" ~ "Rest of the World",
                                    mom_birthplace == "RH" ~ "Rest of the World",
                                    mom_birthplace == "RM" ~ "Rest of the World",
                                    mom_birthplace == "RU" ~ "Rest of the World",
                                    mom_birthplace == "RW" ~ "Rest of the World",
                                    mom_birthplace == "SA" ~ "Rest of the World",
                                    mom_birthplace == "SE" ~ "Rest of the World",
                                    mom_birthplace == "SF" ~ "Rest of the World",
                                    mom_birthplace == "SG" ~ "Rest of the World",
                                    mom_birthplace == "SI" ~ "Rest of the World",
                                    mom_birthplace == "SJ" ~ "Rest of the World",
                                    mom_birthplace == "SL" ~ "Rest of the World",
                                    mom_birthplace == "SN" ~ NA,
                                    mom_birthplace == "SNC" ~ "Canada",
                                    mom_birthplace == "SO" ~ "Rest of the World",
                                    mom_birthplace == "SG" ~ "Rest of the World",
                                    mom_birthplace == "SP" ~ "Rest of the World",
                                    mom_birthplace == "SQ" ~ "Rest of the World",
                                    mom_birthplace == "SR" ~ "Rest of the World",
                                    mom_birthplace == "STK" ~ "Rest of the World",
                                    mom_birthplace == "SU" ~ "Rest of the World",
                                    mom_birthplace == "SW" ~ "Rest of the World",
                                    mom_birthplace == "SX" ~ "Rest of the World",
                                    mom_birthplace == "SY" ~ "Rest of the World",
                                    mom_birthplace == "SZ" ~ "Rest of the World",
                                    mom_birthplace == "TA" ~ "Rest of the World",
                                    mom_birthplace == "TC" ~ "Rest of the World",
                                    mom_birthplace == "TG" ~ "Rest of the World",
                                    mom_birthplace == "TH" ~ "Rest of the World",
                                    mom_birthplace == "TI" ~ "Rest of the World",
                                    mom_birthplace == "TK" ~ "Rest of the World",
                                    mom_birthplace == "TO" ~ "Rest of the World",
                                    mom_birthplace == "TR" ~ "Rest of the World",
                                    mom_birthplace == "TS" ~ "Rest of the World",
                                    mom_birthplace == "TU" ~ "Rest of the World",
                                    mom_birthplace == "TV" ~ "Rest of the World",
                                    mom_birthplace == "TZ" ~ "Rest of the World",
                                    mom_birthplace == "UA" ~ "Rest of the World",
                                    mom_birthplace == "UG" ~ "Rest of the World",
                                    mom_birthplace == "UIK" ~ "Rest of the World",
                                    mom_birthplace == "UK" ~ "Rest of the World",
                                    mom_birthplace == "UN" ~ "Rest of the World",
                                    mom_birthplace == "UP" ~ "Rest of the World",
                                    mom_birthplace == "US" ~ NA,
                                    mom_birthplace == "UV" ~ "Rest of the World",
                                    mom_birthplace == "UY" ~ "Rest of the World",
                                    mom_birthplace == "UZ" ~ "Rest of the World",
                                    mom_birthplace == "VB" ~ "Virgin Islands",
                                    mom_birthplace == "VE" ~ "Rest of the World",
                                    mom_birthplace == "VM" ~ "Vietnam",
                                    mom_birthplace == "WJ" ~ "Rest of the World",
                                    mom_birthplace == "WK" ~ "Rest of the World",
                                    mom_birthplace == "WLK" ~ "Rest of the World",
                                    mom_birthplace == "WS" ~ "Rest of the World",
                                    mom_birthplace == "XC" ~ "Rest of the World",
                                    mom_birthplace == "XD" ~ "Rest of the World",
                                    mom_birthplace == "XE" ~ "Rest of the World",
                                    mom_birthplace == "XF" ~ "Rest of the World",
                                    mom_birthplace == "XJ" ~ "Rest of the World",
                                    mom_birthplace == "XK" ~ "Rest of the World",
                                    mom_birthplace == "XM" ~ "Rest of the World",
                                    mom_birthplace == "XN" ~ "Rest of the World",
                                    mom_birthplace == "XO" ~ "Rest of the World",
                                    mom_birthplace == "XR" ~ "Rest of the World",
                                    mom_birthplace == "XV" ~ "Rest of the World",
                                    mom_birthplace == "XX" ~ NA,
                                    mom_birthplace == "YE" ~ "Rest of the World",
                                    mom_birthplace == "YKC" ~ "Canada",
                                    mom_birthplace == "YU" ~ "Rest of the World",
                                    mom_birthplace == "ZA" ~ "Rest of the World",
                                    TRUE ~ .$mom_birthplace))

#1c. make comparable
df <- df %>%
  mutate(mom_years_edu = if_else(mom_years_edu < 2 & !is.na(mom_years_edu), 1,
                                 if_else(mom_years_edu > 3 & mom_years_edu < 9 & !is.na(mom_years_edu), 3, 
                                         if_else(mom_years_edu == 9, NA, 2))),
         fath_years_edu = if_else(fath_years_edu < 2 & !is.na(fath_years_edu), 1,
                                 if_else(fath_years_edu > 3 & fath_years_edu < 9 & !is.na(fath_years_edu), 3, 
                                         if_else(fath_years_edu == 9, NA, 2))),
         fath_hisp = if_else(fath_hisp == 8 | fath_hisp == 5 & !is.na(fath_hisp), 6, fath_hisp), 
         mom_hisp = if_else(mom_hisp == 8 | mom_hisp == 5 & !is.na(mom_hisp), 6, mom_hisp))

#Note: need to collapse mom_years_edu and fath_years_edu to be comparable with 2006 and after data. 
#Created 8th grade or below, high school, or above high school levels of education

#1d. select variables that are used across years
df <- df %>%
  select(-c(X, X.1, apgar1, apgar5, apgar10, num_cigarettes, num_cigarettes_tri1, 
            num_cigarettes_tri2, num_cigarettes_tri3, mom_height_in, mom_weight_prepreg,
            wic_received, plural_ord, mon_prenatal, diabetes_gest_p, fibroids_p, asthma_p, 
            mult_preg_p, rest_interut_grow_p, oth_poor_preg_out_p, success_ext_cephalic_ver_p,
            fail_ext_cephalic_ver_p, expert_consult_high_risk_p, fertil_drug_insemin_p, 
            assist_repro_tech_p, chlamydia_p, gonorrhea_p, groupb_strep_p, hepc_p, 
            syphilis_p, cytomegalovirus_p, listeria_p, parvovirus_p, toxoplasmosis_p,
            chlamydia_screen_p, gonorrhea_screen_p, groupb_strep_screen_p, hepb_screen_p,
            hiv_screen_p, syphilis_screen_p, nonvertex_pres_l, steroids_fetal_lung_l, 
            antibiotics_l, chorioamnionitis_l, fetal_intol_l, epidural_anes_l, 
            mem_rupture_l, placenta_insuf_l, perineal_lacer_l, rupture_uterus_l,
            unplan_hysterect_l, admiss_icu_l, unplanned_operat_l, mom_city, preg_complic,
            labor_complic))

write.csv2(df, file= here("data","intermediate","bsmf_2007_2011_int2.csv"))