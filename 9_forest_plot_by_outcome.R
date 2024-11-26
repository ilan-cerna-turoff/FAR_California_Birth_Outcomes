# Forest Plot
# Date: 2/7/2024
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","here","forestploter","grid") 

# Preterm
table_final <- matrix(c('Infant sex', NA, NA, NA,
                        'Female','-0.42','-0.48', '-0.37',
                        'Male','-0.45','-0.53', '-0.38',
                        'Birth parent ethnicity', NA, NA, NA,
                        'Hispanic', '-0.59','-0.67','-0.51',
                        'Non-Hispanic', '-0.26','-0.32','-0.20',
                        'Birth parent race', NA, NA, NA,
                        'White','-0.49','-0.55','-0.42',
                        'Asian','-0.35','-0.42','-0.28',
                        'Black','-0.27','-0.43','-0.12',
                        'American Indian/Alaskan Native','-0.41','-0.71','-0.12',
                        'Hawaiian/Pacific Islander','-0.65','-0.95','-0.36',
                        '≥ Two racial categories','-0.07','-0.23','0.09',
                        'Principal source of payment for prenatal care', NA, NA, NA,
                        'Private insurance/self-pay','-0.21','-0.26','-0.15',
                        'Public insurance/assistance','-0.61','-0.69','-0.54',
                        'Birth parent education level', NA, NA, NA,
                        'High school and below','-0.28','-0.51','-0.05',
                        'Some college and above','-0.53','-0.78','-0.28',
                        'Overall','-0.44','-0.50','-0.38'), ncol=4, byrow=TRUE) #add a blank column for some space

colnames(table_final) <- c('Subgroup','Cases per month','Low','High')
tab1 <- as.data.frame(table_final) 

tab1 <- tab1 %>%
  mutate(across(c("Cases per month","Low","High"), as.numeric))

tab1 <- tab1 %>%
  mutate(se = (tab1$High - tab1$Low)/3.92)

tab1$Subgroup <- ifelse(is.na(tab1$'Cases per month'),
                       tab1$Subgroup,
                       paste0("", tab1$Subgroup)) #indent column

tab1$`Preterm birth (95% CI)` <- ifelse(is.na(tab1$se), "",
                                                  sprintf("%.1f (%.1f to %.1f)", #how CI appears, 1 decimal point
                                                          tab1$'Cases per month', tab1$Low, tab1$High))

tab1$` ` <- paste(rep("  ", 10), collapse = " ") #space for CI

tm <- forest_theme(base_size = 12,
                   base_family = "serif",
                   ci_fill = "darkgreen",
                   refline_col = "gray25")


p <- forest(tab1[,c(1, 6:7)], est = tab1$'Cases per month', lower = tab1$Low, upper = tab1$High, 
            ref_line = 0, ci_column = 3, xlim = c(-1, 1),
            arrow_lab = c("Reduction", "Increase"),
            theme = tm, 
            boxsize = 4000) 

p
g <- edit_plot(p, row = c(1,4,7,14,17,20), 
               gp = gpar(fontface = "bold")) #bold specific rows

plot1 <- add_border(g, 
                part = "header", 
                row = 1,
                gp = gpar(lwd = 1)) #add line below heading

plot1

# png('preterm_forestplot_with_subgroups.png', res = 300, width = 10, height = 6, unit = 'in')
# plot1
# dev.off()

#-----------------------------------------------------------
# Low birthweight
table_final <- matrix(c('Infant sex', NA, NA, NA,
                        'Female','-0.16','-0.20', '-0.11',
                        'Male','-0.14','-0.19', '-0.10',
                        'Birth parent ethnicity', NA, NA, NA,
                        'Hispanic', '-0.22','-0.28','-0.15',
                        'Non-Hispanic', '-0.07','-0.10','-0.04',
                        'Birth parent race', NA, NA, NA,
                        'White','-0.18','-0.22','-0.13',
                        'Asian','-0.09','-0.15','-0.04',
                        'Black','-0.14','-0.10','-0.04',
                        'American Indian/Alaskan Native','-0.19','-0.36','-0.01',
                        'Hawaiian/Pacific Islander','-0.22','-0.41','-0.03',
                        '≥ Two racial categories','0.13','0.02','0.23',
                        'Principal source of payment for prenatal care', NA, NA, NA,
                        'Private insurance/self-pay','-0.07','-0.10','-0.03',
                        'Public insurance/assistance','-0.20','-0.26','-0.14',
                        'Birth parent education level', NA, NA, NA,
                        'High school and below','-0.06','-0.21','0.09',
                        'Some college and above','-0.21','-0.35','-0.08',
                        'Overall','-0.15','-0.19','-0.10'), ncol=4, byrow=TRUE) #add a blank column for some space

colnames(table_final) <- c('Subgroup','Cases per month','Low','High')
tab1 <- as.data.frame(table_final) 

tab1 <- tab1 %>%
  mutate(across(c("Cases per month","Low","High"), as.numeric))

tab1 <- tab1 %>%
  mutate(se = (tab1$High - tab1$Low)/3.92)

tab1$Subgroup <- ifelse(is.na(tab1$'Cases per month'),
                        tab1$Subgroup,
                        paste0("  ", tab1$Subgroup)) #indent column

tab1$`Low birth weight (95% CI)` <- ifelse(is.na(tab1$se), "",
                                           sprintf("%.1f (%.1f to %.1f)", #how CI appears, 1 decimal point
                                                   tab1$'Cases per month', tab1$Low, tab1$High))

tab1$` ` <- paste(rep("  ", 10), collapse = " ") #space for CI

tm <- forest_theme(base_size = 12,
                   base_family = "serif",
                   ci_fill = "darkgreen",
                   refline_col = "gray25")

p <- forest(tab1[,c(1, 6:7)], est = tab1$'Cases per month', lower = tab1$Low, upper = tab1$High, 
            ref_line = 0, ci_column = 3, xlim = c(-1, 1),
            arrow_lab = c("Reduction", "Increase"),
            theme = tm,
            boxsize = 4000) 

p
g <- edit_plot(p, row = c(1,4,7,14,17,20), 
               gp = gpar(fontface = "bold")) #bold specific rows

plot2 <- add_border(g, 
                part = "header", 
                row = 1,
                gp = gpar(lwd = 1)) #add line below heading

plot2

# png('lowbirthweight_forestplot_with_subgroups.png', res = 300, width = 10, height = 6, unit = 'in')
# plot2
# dev.off()

#-----------------------------------------------------------
# Very low birthweight
table_final <- matrix(c('Infant sex', NA, NA, NA,
                        'Female','-0.02','-0.03', '-0.02',
                        'Male','-0.02','-0.03', '-0.01',
                        'Birth parent ethnicity', NA, NA, NA,
                        'Hispanic', '-0.03','-0.04','-0.02',
                        'Non-Hispanic', '-0.01','-0.02','0.00',
                        'Birth parent race', NA, NA, NA,
                        'White','-0.03','-0.03','-0.02',
                        'Asian','-0.02','-0.03','-0.00',
                        'Black','-0.02','-0.06','0.03',
                        'American Indian/Alaskan Native','-0.04','-0.12','0.04',
                        'Hawaiian/Pacific Islander','-0.03','-0.12','0.06',
                        '≥ Two racial categories','0.05','0.00','0.09',
                        'Principal source of payment for prenatal care', NA, NA, NA,
                        'Private insurance/self-pay','-0.01','-0.02','-0.00',
                        'Public insurance/assistance','-0.03','-0.04','-0.01',
                        'Birth parent education level', NA, NA, NA,
                        'High school and below','-0.01','-0.03','0.01',
                        'Some college and above','-0.03','-0.05','-0.01',
                        'Overall','-0.02','-0.03','-0.01'), ncol=4, byrow=TRUE) #add a blank column for some space

colnames(table_final) <- c('Subgroup','Cases per month','Low','High')
tab1 <- as.data.frame(table_final) 

tab1 <- tab1 %>%
  mutate(across(c("Cases per month","Low","High"), as.numeric))

tab1 <- tab1 %>%
  mutate(se = (tab1$High - tab1$Low)/3.92)

tab1$Subgroup <- ifelse(is.na(tab1$'Cases per month'),
                        tab1$Subgroup,
                        paste0("", tab1$Subgroup)) #indent column

tab1$`Very low birth weight (95% CI)` <- ifelse(is.na(tab1$se), "",
                                                sprintf("%.1f (%.1f to %.1f)", #how CI appears, 1 decimal point
                                                        tab1$'Cases per month', tab1$Low, tab1$High))

tab1$` ` <- paste(rep("  ", 10), collapse = " ") #space for CI

tm <- forest_theme(base_size = 12,
                   base_family = "serif",
                   ci_fill = "darkgreen",
                   refline_col = "gray25")

p <- forest(tab1[,c(1, 6:7)], est = tab1$'Cases per month', lower = tab1$Low, upper = tab1$High, 
            ref_line = 0, ci_column = 3, xlim = c(-1, 1),
            arrow_lab = c("Reduction", "Increase"),
            theme = tm,
            boxsize = 4000) 

p
g <- edit_plot(p, row = c(1,4,7,14,17,20), 
               gp = gpar(fontface = "bold")) #bold specific rows

plot3 <- add_border(g, 
                part = "header", 
                row = 1,
                gp = gpar(lwd = 1)) #add line below heading

plot3

# png('verylowbirthweight_forestplot_with_subgroups.png', res = 300, width = 10, height = 6, unit = 'in')
# plot3
# dev.off()

#-----------------------------------------------------------
# Small for gestational age
table_final <- matrix(c('Infant sex', NA, NA, NA,
                        'Female','-0.26','-0.39', '-0.12',
                        'Male','-0.29','-0.41', '-0.17',
                        'Birth parent ethnicity', NA, NA, NA,
                        'Hispanic', '-0.45','-0.63','-0.27',
                        'Non-Hispanic', '-0.08','-0.16','0.01',
                        'Birth parent race', NA, NA, NA,
                        'White','-0.35','-0.48','-0.22',
                        'Asian','-0.18','-0.32','-0.03',
                        'Black','-0.03','-0.23','0.17',
                        'American Indian/Alaskan Native','-0.50','-0.81','-0.20',
                        'Hawaiian/Pacific Islander','-0.45','-0.72','-0.17',
                        '≥ Two racial categories','0.48','0.30','0.66',
                        'Principal source of payment for prenatal care', NA, NA, NA,
                        'Private insurance/self-pay','-0.08','-0.17','0.01',
                        'Public insurance/assistance','-0.42','-0.61','-0.22',
                        'Birth parent education level', NA, NA, NA,
                        'High school and below','-0.10','-0.44','0.23',
                        'Some college and above','-0.40','-0.70','-0.10',
                        'Overall','-0.27','-0.40','-0.15'), ncol=4, byrow=TRUE) #add a blank column for some space

colnames(table_final) <- c('Subgroup','Cases per month','Low','High')
tab1 <- as.data.frame(table_final) 

tab1 <- tab1 %>%
  mutate(across(c("Cases per month","Low","High"), as.numeric))

tab1 <- tab1 %>%
  mutate(se = (tab1$High - tab1$Low)/3.92)

tab1$Subgroup <- ifelse(is.na(tab1$'Cases per month'),
                        tab1$Subgroup,
                        paste0("", tab1$Subgroup)) #indent column

tab1$`Small-for-gestational age (95% CI)` <- ifelse(is.na(tab1$se), "",
                                                    sprintf("%.1f (%.1f to %.1f)", #how CI appears, 1 decimal point
                                                            tab1$'Cases per month', tab1$Low, tab1$High))

tab1$` ` <- paste(rep("  ", 10), collapse = " ") #space for CI

tm <- forest_theme(base_size = 12,
                   base_family = "serif",
                   ci_fill = "darkgreen",
                   refline_col = "gray25")

p <- forest(tab1[,c(1, 6:7)], est = tab1$'Cases per month', lower = tab1$Low, upper = tab1$High, 
            ref_line = 0, ci_column = 3, xlim = c(-1, 1),
            arrow_lab = c("Reduction", "Increase"),
            theme = tm,
            boxsize = 4000) 

p
g <- edit_plot(p, row = c(1,4,7,14,17,20), 
               gp = gpar(fontface = "bold")) #bold specific rows

plot4 <- add_border(g, 
                part = "header", 
                row = 1,
                gp = gpar(lwd = 1)) #add line below heading

plot4

# png('smallforgestational_forestplot_with_subgroups.png', res = 300, width = 10, height = 6, unit = 'in')
# plot4
# dev.off()

#-----------------------------------------------------------
#combine
box1 <- rbind(plot1,plot3)
box2 <- rbind(plot2,plot4)

box3 <- cbind(box1,box2)

# png('Figure2.png', res = 300, width = 16, height = 11.5, unit = 'in')
# box3
# dev.off()
