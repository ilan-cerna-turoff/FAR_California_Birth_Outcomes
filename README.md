# FAR\_California\_Birth\_Outcomes

## Overview 
This is a repository to prepare and analyze California electronic birth records for analyzing changes in trends in adverse birth outcomes before and after the implementation of the 2006 Fair Arsenic Rule (FAR). The FAR reduced the national regulated level of arsenic in drinking water from 50 to 10 μg/L. The repository also includes mapping and analysis of these data to ZIP Code areas that reduced arsenic by set magnitudes and below thresholds.

The R scripts follow this pattern by number series:

  * `0_`: converts raw electronic medical records into a usable form and cleans/standards scripts.
  * `1_`: conducts an initial bind of scripts across comparable years.
  * `2_`: cleans scripts within blocks of comparable years.
  * `3_`: binds scripts together.
  * `4_`: fuses electronic medical records with water arsenic data and includes a last round of cleaning/preparation for data analysis. 
  * `5_`: conducts an interrupted time series analysis for the four main adverse birth outcomes for all births and sociodemographic subgroups.
  * `6_`: prepares visualizations for descriptives and analyses.
  * `7_`: maps adverse birth outcomes by ZIP Codes.
  * `8_`: conducts sensitivity analyses.
  * `9_`: prepares forest plot visuals for the main adverse birth outcomes and sensitivity analyses.

## Additional Notes
  * The outcomes are preterm, low birth weight, very low birth weight, and small-for-gestational age births, 9 months after the implementation of the FAR in 2006.
  * Small-for-gestational age is calculated separately by sex and uses Aris et al. (2019) referencing of US national percentiles for birth gestation.
  * The analysis uses Prais-Winsten models for trends before and after the 2006 FAR. Prais-Winsten uses a linear model with a first order autocorrelation. For more information on models, refer to Bottomley et al. (2023).
  * For description of how the water arsenic files were created, refer to Spaur et al. (2024).
  * Written permission is required for the use of the electronic birth records data from California. Contact the California Department of Public Health (CDPH).

## Citation
**Please include the below citation if this code is used, or article is cited (in progress). Questions and comments can be addressed to Dr. Ilan Cerna-Turoff at it2208@caa.columbia.edu.** 

Cerna-Turoff I, Stoms M, Casey JA, Goin DE, Goldsmith J, Herbstman J, Parks RM, Nigra AE. The United States Environmental Protection Agency’s regulation of arsenic in California’s public drinking water and reductions in adverse birth outcomes: an interrupted time series design.  