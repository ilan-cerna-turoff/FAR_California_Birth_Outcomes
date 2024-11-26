# File: Data cleaning - 2007-2008
# Date: 11/24/2023
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","here","haven")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
#1a. read in columns that using #
bsmf_2007 <- read.csv2(here("data", "intermediate", "bsmf_2007_int.csv"), header = T, sep = ",")

bsmf_2007 <- bsmf_2007 %>%
  add_column(year = 2007) %>%  
  mutate(prev_livebirth_liv = as.numeric(prev_livebirth_liv),
         prev_livebirth_dead = as.numeric(prev_livebirth_dead),
         birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"),
         birth_countyplace = str_remove(birth_countyplace, "^0"),
         mom_res_county = str_remove(mom_res_county, "^0")) #get rid of excess 0s at beginning

bsmf_2008 <- read.csv2(here("data", "intermediate", "bsmf_2008_int.csv"), header = T, sep = ",")

bsmf_2008 <- bsmf_2008 %>%
  mutate(birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"),
         birth_countyplace = str_remove(birth_countyplace, "^0"),
         mom_res_county = str_remove(mom_res_county, "^0")) #get rid of excess 0s at beginning

bsmf <- bind_rows(bsmf_2007,bsmf_2008)

write.csv(bsmf, file= here("data","intermediate","bsmf_2007_2008.csv"))
