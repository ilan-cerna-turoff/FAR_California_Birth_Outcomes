# File: Data cleaning - 2011-2017
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
bsmf_2011 <- read.csv2(here("data", "intermediate", "bsmf_2011_int.csv"), header = T, sep = ",")

bsmf_2011 <- bsmf_2011 %>%
  add_column(year = 2011) %>%  
  mutate(prev_livebirth_liv = as.numeric(prev_livebirth_liv),
         prev_livebirth_dead = as.numeric(prev_livebirth_dead),
         birthdate = str_pad(string = birthdate, width = 7, pad = "0", side = "left"),
         birthdate = str_pad(string = birthdate, width = 8, pad = "2", side = "left"),
         birth_countyplace = str_remove(birth_countyplace, "^0"),
         mom_res_county = str_remove(mom_res_county, "^0")) #get rid of excess 0s at beginning

write.csv(bsmf_2011, file= here("data","intermediate","bsmf_2011.csv"))
