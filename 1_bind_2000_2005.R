# File: Data cleaning - 2000-2005
# Date: 11/24/2023
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","here","haven")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
# Note: changes format of columns in 2005 and slightly in 2003. Positioning is the same 
# throughout 2000-2005 though so can bind together with the same code.

#1a. read in columns that using
bsmf_2000 <- read.csv2(here("data", "intermediate", "bsmf_2000_int.csv"), header = T, sep = ",")

bsmf_2000 <- bsmf_2000 %>%
  mutate(prev_livebirth_liv = as.numeric(as.character(prev_livebirth_liv)),
         prev_livebirth_dead = as.numeric(as.character(prev_livebirth_dead)),
         last_liv_date = as.character(last_liv_date),
         birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"), #consistent length
         birth_countyplace = str_remove(birth_countyplace, "^0"), 
         mom_res_county = str_remove(mom_res_county, "^0")) %>% #get rid of excess 0s at beginning
  select(-c(preg_complic,labor_complic))

bsmf_2001 <- read.csv2(here("data", "intermediate", "bsmf_2001_int.csv"), header = T, sep = ",")

bsmf_2001 <- bsmf_2001 %>%
  add_column(year = 2001) %>%
  mutate(mom_zipcode = as.numeric(as.character(mom_zipcode)),
         prev_livebirth_liv = as.numeric(as.character(prev_livebirth_liv)),
         prev_livebirth_dead = as.numeric(as.character(prev_livebirth_dead)),
         last_liv_date = as.character(last_liv_date),
         birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"), #consistent length
         birth_countyplace = str_remove(birth_countyplace, "^0"), 
         mom_res_county = str_remove(mom_res_county, "^0")) %>% #get rid of excess 0s at beginning
  select(-c(preg_complic,labor_complic))

bsmf_2002 <- read.csv2(here("data", "intermediate", "bsmf_2002_int.csv"), header = T, sep = ",")

bsmf_2002 <- bsmf_2002 %>%
  add_column(year = 2002) %>% 
  mutate(mom_zipcode = as.numeric(mom_zipcode),
         prev_livebirth_liv = as.numeric(as.character(prev_livebirth_liv)),
         prev_livebirth_dead = as.numeric(as.character(prev_livebirth_dead)),
         last_liv_date = as.character(last_liv_date),
         birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"), #consistent length
         birth_countyplace = str_remove(birth_countyplace, "^0"), 
         mom_res_county = str_remove(mom_res_county, "^0")) %>% #get rid of excess 0s at beginning
  select(-c(preg_complic,labor_complic))

bsmf_2003 <- read.csv2(here("data", "intermediate", "bsmf_2003_int.csv"), header = T, sep = ",")

bsmf_2003 <- bsmf_2003 %>%
  add_column(year = 2003) %>%
  mutate(prev_livebirth_liv = as.numeric(as.character(prev_livebirth_liv)),
         prev_livebirth_dead = as.numeric(as.character(prev_livebirth_dead)),
         last_liv_date = as.character(last_liv_date),
         birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"), #consistent length
         birth_countyplace = str_remove(birth_countyplace, "^0"), 
         mom_res_county = str_remove(mom_res_county, "^0")) %>% #get rid of excess 0s at beginning
  select(-c(preg_complic,labor_complic))

bsmf_2004 <- read.csv2(here("data", "intermediate", "bsmf_2004_int.csv"), header = T, sep = ",")

bsmf_2004 <- bsmf_2004 %>%
  add_column(year = 2004) %>%
  mutate(mom_zipcode = as.numeric(mom_zipcode),
         leng_gestation = as.numeric(as.character(leng_gestation)),
         last_liv_date = as.character(last_liv_date),
         prev_livebirth_liv = as.numeric(as.character(prev_livebirth_liv)),
         prev_livebirth_dead = as.numeric(as.character(prev_livebirth_dead)),
         birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"), #consistent length
         birth_countyplace = str_remove(birth_countyplace, "^0"), 
         mom_res_county = str_remove(mom_res_county, "^0")) %>% #get rid of excess 0s at beginning
  select(-c(preg_complic,labor_complic))

bsmf_2005 <- read.csv2(here("data", "intermediate", "bsmf_2005_int.csv"), header = T, sep = ",")

bsmf_2005 <- bsmf_2005 %>%
  add_column(year = 2005) %>%
  mutate(mom_zipcode = as.numeric(mom_zipcode),
         prev_livebirth_liv = as.numeric(as.character(prev_livebirth_liv)),
         prev_livebirth_dead = as.numeric(as.character(prev_livebirth_dead)),
         last_liv_date = as.character(last_liv_date),
         census_tract = as.character(census_tract),
         birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"), #consistent length
         birth_countyplace = str_remove(birth_countyplace, "^0"), 
         mom_res_county = str_remove(mom_res_county, "^0")) %>% #get rid of excess 0s at beginning
  select(-c(preg_complic,labor_complic))
         
bsmf <- bind_rows(bsmf_2000,bsmf_2001,bsmf_2002,bsmf_2003,bsmf_2004,bsmf_2005)

write.csv(bsmf, file= here("data","intermediate","bsmf_2000_2005.csv"))
