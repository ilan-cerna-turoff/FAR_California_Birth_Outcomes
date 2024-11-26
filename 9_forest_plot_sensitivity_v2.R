# Forest Plot
# Date: 2/7/2024
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
pacman::p_load("tidyverse","here","forestploter","grid") 

table_final <- matrix(c('Non-meaningful outcome',NA,NA,NA,
                        'Multiple births', '-0.08', '-1.25', '1.09',
                        'Non-meaningful intervention',NA,NA,NA,
                        'Preterm','-0.41','-0.86', '0.05',
                        'Low birth weight','-0.35','-0.72', '0.02',
                        'Very low birth weight', '-0.04','-0.10','0.01',
                        'Small-for-gestational age', '-1.03','-2.06','0.01',
                        'Pre-intervention linear slope',NA,NA,NA,
                        'Preterm','0.00','0.00', '0.01',
                        'Low birth weight','0.00','0.00', '0.00',
                        'Very low birth weight', '0.00','0.00','0.00',
                        'Small-for-gestational age', '0.01','0.00','0.01',
                        'ZCTA-level water arsenic reduction by magnitude',NA,NA,NA,
                        '≥ 3 μg/L',NA,NA,NA,
                        'Preterm','-0.91','-1.36', '-0.47',
                        '≥ 2 μg/L',NA,NA,NA,
                        'Preterm','-0.50','-0.64', '-0.37',
                        '≥ 1 μg/L',NA,NA,NA,
                        'Preterm','-0.34','-0.40', '-0.28',
                        'No reduction',NA,NA,NA,
                        'Preterm','-0.52','-0.59', '-0.44',
                        'ZCTA-level water arsenic reduction by threshold',NA,NA,NA,
                        '≤ 5 μg/L',NA,NA,NA,
                        'Preterm','-0.72','-0.89', '-0.55',
                        'Low birth weight','-0.27','-0.41', '-0.13',
                        'Very low birth weight', '-0.08','-0.14','-0.02',
                        'Small-for-gestational age', '-0.48','-0.69','-0.27',
                        '≤ 2 μg/L',NA,NA,NA,
                        'Preterm','-0.41','-0.49', '-0.33',
                        'Low birth weight','-0.13','-0.18', '-0.07',
                        'Very low birth weight', '-0.01','-0.03','-0.00',
                        'Small-for-gestational age', '-0.34','-0.49','-0.18',
                        '≤ 1 μg/L',NA,NA,NA,
                        'Preterm','-0.14','-0.18', '-0.10',
                        'Low birth weight','-0.14','-0.18', '-0.10',
                        'Very low birth weight', '-0.02','-0.03','-0.01',
                        'Small-for-gestational age', '-0.02','-0.03','-0.01'), ncol=4, byrow=TRUE) #add a blank column for some space

colnames(table_final) <- c('Subgroup','Cases per month','Low','High')
tab1 <- as.data.frame(table_final) 

tab1 <- tab1 %>%
  mutate(across(c("Cases per month","Low","High"), as.numeric))

tab1 <- tab1 %>%
  mutate(se = (tab1$High - tab1$Low)/3.92)

tab1$Subgroup <- ifelse(is.na(tab1$'Cases per month'),
                        tab1$Subgroup,
                        paste0("   ", tab1$Subgroup)) #indent column

tab1$`Monthly rate (95% CI)` <- ifelse(is.na(tab1$se), "",
                                                           sprintf("%.1f (%.1f to %.1f)", #how CI appears, 1 decimal point
                                                                   tab1$'Cases per month', tab1$Low, tab1$High))

tab1$` ` <- paste(rep(" ", 25), collapse = " ") #space for CI

tm <- forest_theme(base_size = 12,
                   base_family = "serif",
                   ci_fill = "darkgreen",
                   refline_col = "gray25")

p <- forest(tab1[,c(1, 6:7)], est = tab1$'Cases per month', lower = tab1$Low, upper = tab1$High, 
            ref_line = 0, ci_column = 3, xlim = c(-1,1),
            arrow_lab = c("Reduction", "Increase"),
            theme = tm,
            boxsize = 4000) 

p
g <- edit_plot(p, row = c(1,3,8,13,22), 
               gp = gpar(fontface = "bold")) #bold specific rows

plot1 <- add_border(g, 
                    part = "header", 
                    row = 1,
                    gp = gpar(lwd = 1)) #add line below heading

plot1

# png('sensitivity_forestplot.png', res = 300, width = 8, height = 9.5, unit = 'in')
# plot1
# dev.off()
