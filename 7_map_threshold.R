## California arsenic maps
# Date: 05/02/24
# Author: Annie Nigra (adapted by Ilan Cerna-Turoff for current study)

###-- setup --#####
# You need an API key from the Census Bureau to use "tidycensus". A key can be
# acquired at http://api.census.gov/data/key_signup.html. 
# To store your API key across all of your .Renviron, run: 
readRenviron("~/.Renviron")

#Note: allows you not to have to reload every time you open R and applies this key across
#all scripts in the background. You would do this when you store an API key for the 
#first time.

# Install:
#tidycensus::census_api_key("5b9b8d64167328f8bf307a9c09457af0271e0721", install = TRUE)

# After storing the key this way, CENSUS_API_KEY in the subsequent code will call your personal key
Sys.getenv("CENSUS_API_KEY")


###-- Load packages --#####
pacman::p_load("openxlsx","tidyverse","here","raster","tigris","sf","tidycensus",
               "writexl","readxl","Hmisc","janitor")
options(tigris_use_cache = TRUE)

####--- Load data, clean for statistical analysis #####
zctas_full <- read_excel(here("data", "final","ca_zcta_as_change_syr2syr3_27Feb2024.xlsx")) %>% #pre-intervention arsenic levels
  clean_names()

describe(zctas_full$zcta5ce10) # n= 1905 overall
describe(zctas_full$final_wtd_as_avg20092011)  # n=1605 available
describe(zctas_full$wtd_as_avg20002005) #n= 1743 available

zctas_final <- zctas_full %>%
  filter(!is.na(final_wtd_as_avg20092011) & !is.na(wtd_as_avg20002005)) %>%
  mutate(lowered_as_1 = ifelse(wtd_as_avg20002005 > 1 & final_wtd_as_avg20092011 <= 1, 1, 0),
         lowered_as_2 = ifelse(wtd_as_avg20002005 > 2 & final_wtd_as_avg20092011 <= 2, 1, 0),
         lowered_as_5 = ifelse(wtd_as_avg20002005 > 5 & final_wtd_as_avg20092011 <= 5, 1, 0),
         compiled_lower = case_when(lowered_as_1 == 1 ~ 1,
                                    lowered_as_2 == 1 ~ 2,
                                    lowered_as_5 == 1 ~ 3,
                                    c(lowered_as_1 == 0 | lowered_as_2 == 0 | lowered_as_5 == 0) ~ 0)) %>%
  dplyr::select(c(zcta5ce10,compiled_lower,wtd_as_avg20002005,final_wtd_as_avg20092011)) 

####--- A. Map set up #####
#Set unique theme
raincloud_theme = theme(
  text = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text = element_blank(),
  axis.text.x = element_blank(),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(hjust=0.1,lineheight=.8, face="bold", size = 18),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
  axis.line.y = element_blank())

### read in and merge US census map
Zips<-zctas(year=2010) %>% #based upon the 2010 census
  shift_geometry(preserve_area = FALSE, position="below")

colnames(zctas_final)[colnames(zctas_final) == "zcta5ce10"] ="ZCTA5CE10" #capitalize to match with census

Zips_ca <-merge(Zips,zctas_final,by="ZCTA5CE10",all=T) %>% #merges census data with zcta estimates
  filter(!is.na(final_wtd_as_avg20092011) & !is.na(wtd_as_avg20002005)) 

mypal3<-c("white","#fef0d9","#fdbb84","#b30000")

state_outline3<-states(year=2010, cb=TRUE, resolution="20m") %>%
  shift_geometry(preserve_area = FALSE, position="below") 

####---- Figure 2. ZCTA maps for publication -----####
# Sample sizes:
describe(Zips_ca$ZCTA5CE10) #n=1582 zipcodes

state_outline4<-state_outline3[which(state_outline3$STATE=="06"),] #CA outline

## 1. map1: these estimates have diff weighting schemes for tier1 cwss
# Arsenic
plot_as_ca <-ggplot() + 
  geom_sf(data=Zips_ca, aes(fill=as.factor(compiled_lower)), colour= NA )+
  scale_fill_manual(values=mypal3,
                    name="Arsenic (μg/L) reductions",
                    breaks=c(0,1,2,3),
                    labels=c("N/A", "≤ 1", "≤ 2", "≤ 5"))+
  geom_sf(data = state_outline4[which(state_outline3$NAME!="Puerto Rico"),], aes(), colour = "black", fill = NA)+
  theme(legend.position = "none")+
  theme_bw() +
  theme(axis.line = element_line(), panel.border = element_blank())+
  coord_sf(crs = 5070 ) +
  raincloud_theme+
  theme(legend.key.size = unit(1, 'cm'))+
  theme(legend.title = element_text(size=12, family = "Times New Roman"))+
  theme(legend.text = element_text(size=12, family = "Times New Roman"))+
  theme(plot.title = element_text(size = 12))+
  theme(axis.line=element_blank())+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  theme(legend.key=element_rect(colour="black"))

#ggsave("map_threshold.png", plot = last_plot(), dpi = 300)





