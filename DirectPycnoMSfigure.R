#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno density figure for Direct MS                                             ##
# Data are current as of 2022-05-05                                              ##
# Data source: Sarah Gravem/IUCN                                                 ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2022-05-05                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Script to show range of historic and current pycnopodia densities across the
# West Coast as inset for manuscript model figure. 


# Required Files (check that script is loading latest version):
# PycnoDensity.csv

# Associated Scripts:
# NONE

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2022-05-05  Script Created
# 2022-05-09  Started creating additional feeding preference figure

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(maps)
library(scatterpie)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PycnoDensity <- read_csv("~/Git/pycno-urchin/Data/DirectMS/PycnoDensity.csv", 
                         col_types = cols(meanDensityKM2 = col_number(), 
                                          SDDensityKM2 = col_number(), SEDensityKM2 = col_number()))

PycnoDensity[is.na(PycnoDensity)] <- 0

PycnoDiet <- read_csv("Data/DirectMS/PycnoDiet.csv")

dietWide <- PycnoDiet %>%
  select(-Notes) %>%
  pivot_wider(names_from = "Item", values_from = "Value")
dietWide[is.na(dietWide)] <- 0

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DENSITY FIGURE                                                               ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# make a shiny app that shows the expected state for historic and current values
# with a slider that changes urchin recruitment

PycnoDensity %>%
  mutate(across(populationPhase, factor, levels=c("Historic", "Current"))) %>% 
  mutate(across(Region, factor, levels=c("Aleutians",
                                         "West Gulf of Alaska", 
                                         "East Gulf of Alaska",
                                         "Southeast Alaska",
                                         "British Columbia",
                                         "Salish Sea",
                                         "Washington Outer Coast",
                                         "Oregon",
                                         "Northern California",
                                         "Central California",
                                         "Southern California",
                                         "Baja California"))) %>% 
  ggplot(aes(Region, meanDensityM2, color = populationPhase)) +
  geom_pointrange(size = 1, aes(ymin = meanDensityM2-SEDensityM2, ymax = meanDensityM2+SEDensityM2), 
                  position=position_jitter(width=0.2), 
                  linetype='solid') +
  geom_hline(yintercept = 0.06, color = "red", linetype = "dotted", size = 1) +
  scale_color_viridis(name = "Population Phase", discrete = TRUE, begin = 0.3, end = 0.7, option = "F") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, hjust=0, vjust = 0)) +
  labs(y = "Mean Pycnopodia Density (m^-1)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DIET FIGURE                                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

world <- map_data("world")
NorthAm <- world %>%
  filter(region %in% c("USA", "Canada", "Mexico")) %>%
  filter(-155 < long & long < -110) %>%
  filter(25 < lat & lat < 75)
ggplot(NorthAm, aes(long, lat)) +
  geom_map(map=NorthAm, aes(map_id=region), fill="grey97", color="grey") +
  ylim(c(30,70)) +
  xlim(c(-150, -115)) +
  coord_fixed() +
  theme_bw() +
  geom_scatterpie(data = filter(dietWide, Depth == "Subtidal"),
                  aes(Lon, Lat, r = 1.5),
                  cols = c("Urchins", "Gastropods", "Bivalves", "Crustaceans", "Other Echinoderms", "Echinoderms")) +
  scale_fill_viridis(discrete = TRUE, option = "H", begin = 0.2)

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####