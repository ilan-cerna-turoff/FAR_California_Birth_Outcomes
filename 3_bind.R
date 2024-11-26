# File: Final bind
# Date: 12/01/2023
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","purrr","here","readr","usethis")

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
#1a. read in files
int_dir <- here("data", "intermediate")
fin_dir <- here("data", "final")
int_filepaths <- list.files(int_dir, pattern = "\\int2.csv$")
         
df <- int_filepaths %>%
  map(~ read_delim(file.path(int_dir, .), delim = ";")) 

iwalk( #convert to individual dataframes
  .x = df,
  .f = function(x, y) {
    x <- as.data.frame(x)
    y <- paste0('df', y)
    assign(y, x, envir = globalenv())
  }
)

rm(df, fin_dir, int_dir, int_filepaths)

#usethis::edit_r_environ() #increase operating memory
#cut/paste R_MAX_VSIZE=100Gb in the new pop up window and then restart R 

#1b. ensure variables have comparable formats
df1 <- df1 %>%
  select(-starts_with("...")) %>%
  mutate(mom_zipcode = as.character(mom_zipcode))

df2 <- df2 %>%
  select(-starts_with("...")) %>%
  mutate(mom_zipcode = as.character(mom_zipcode))

df3 <- df3 %>%
  select(-starts_with("...")) %>%
  mutate(mom_zipcode = as.character(mom_zipcode))

#1c. combine dataframes
final_df <- bind_rows(df1, df2, df3) %>%
  mutate(mom_zipcode = str_extract(mom_zipcode, "[^-]+")) #strip to five digit zipcode

write.csv2(final_df, file= here("data","final","california_2000_2011.csv"))