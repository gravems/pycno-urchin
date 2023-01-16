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
library(data.table)
library(ggpubr)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# read, filter old groupings out, add zeros to NA
PycnoDensity <- read_csv("~/Git/pycno-urchin/Data/DirectMS/PycnoDensity.csv", 
                         col_types = cols(meanDensityKM2 = col_number(), 
                                          SDDensityKM2 = col_number(), SEDensityKM2 = col_number()))
PycnoDensity <- PycnoDensity %>%
  filter(populationPhase != "Current" | Range != "full")

PycnoDensity[is.na(PycnoDensity)] <- 0

PycnoDiet <- read_csv("Data/DirectMS/PycnoDiet.csv")
# create less groupings
PycnoDiet_step1 <- PycnoDiet %>%
  mutate(item = case_when(Item == "Balanus spp." ~ "Barnacles",
                          Item == "Barnacles" ~ "Barnacles",
                          Item == "Bivalves" ~ "Bivalves",
                          Item == "Bryozoans" ~ "Other",
                          Item == "Chitons" ~ "Other Molluscs",
                          Item == "Crabs" ~ "Crustaceans",
                          Item == "Crustaceans" ~ "Crustaceans",
                          Item == "Echinoderms" ~ "Other Echinoderms",
                          Item == "Fishes" ~ "Other",
                          Item == "Gastropods" ~ "Gastropods",
                          Item == "Holothurians" ~ "Other Echinoderms",
                          Item == "Hydroids" ~ "Other",
                          Item == "Molluscs Cephalopods" ~ "Other Molluscs",
                          Item == "Molluscs Gastropods" ~ "Gastropods",
                          Item == "Molluscs Pelecypoda" ~ "Bivalves",
                          Item == "Mytilus edulis" ~ "Bivalves",
                          Item == "Ophiuroids" ~ "Other Echinoderms",
                          Item == "Other" ~ "Other",
                          Item == "Other Echinoderms" ~ "Other Echinoderms",
                          Item == "Polychaetes" ~ "Other",
                          Item == "Salps" ~ "Other",
                          Item == "Sipunculids" ~ "Other",
                          Item == "Sponges" ~ "Other",
                          Item == "Unidentified" ~ "Other",
                          Item == "Urchins" ~ "Urchins"))
# create unique sample ID to parse sub- inter-tidal
PycnoDiet_step2 <- PycnoDiet_step1 %>%
  unite("sampleID", Location, Depth, sep = "_") %>%
  group_by(sampleID)
# add urchin in Bamfield samples to the exposed site
PycnoDiet_step2$sampleID <- PycnoDiet_step2$sampleID %>%
  recode("Bamfield_Subtidal" = "Bamfield_Subtidal Exposed")
# bring Bamfield values up to 100 percent with 'Other'
PycnoDiet_step2 <- ungroup(PycnoDiet_step2) %>%
  add_row(Region = c("British Columbia", "British Columbia", "British Columbia"),
          sampleID = c("Bamfield_Subtidal Exposed", "Bamfield_Subtidal Intermediate", "Bamfield_Subtidal Protected"),
          Lat = c(48.8262097, 48.8262097, 48.8262097),
          Lon = c(-125.1359127, -125.1359127, -125.1359127),
          item = c("Other", "Other", "Other"),
          value = c(27.2, 43.1, 42.3))
  # adjust lat long to plot pies seperately
PycnoDiet_step3 <- PycnoDiet_step2 %>%
  mutate(new_long = case_when(sampleID == "Torch Bay_Subtidal" ~ Lon - 1.5,
                              sampleID == "Torch Bay_Intertidal" ~ Lon + 1.2,
                              sampleID == "Pacific Grove_Subtidal" ~ Lon - 3,
                              sampleID == "Bamfield_Subtidal Exposed" ~ Lon - 5,
                              sampleID == "Bamfield_Subtidal Intermediate" ~ Lon - 4.5,
                              sampleID == "Bamfield_Subtidal Protected" ~ Lon - 4,
                              sampleID == "Gabriola Island_Subtidal" ~ Lon - 1,
                              sampleID == "Prince William Sound_Intertidal" ~ Lon + 1.5,
                              sampleID == "Prince William Sound_Subtidal" ~ Lon - 1.5,
                              sampleID == "San Juan Islands_Subtidal Vadas" ~ Lon + 2,
                              sampleID == "San Juan Islands_Subtidal Mauzey" ~ Lon + 2.5,
                              sampleID == "Outer Coast_Intertidal Paine" ~ Lon - 3,
                              sampleID == "Outer Coast_Intertidal Dayton" ~ Lon - 1.5))
PycnoDiet_step4 <- PycnoDiet_step3 %>%
  mutate(new_lat = case_when(sampleID == "Torch Bay_Subtidal" ~ Lat - 2,
                             sampleID == "Torch Bay_Intertidal" ~ Lat - 3.5,
                             sampleID == "Pacific Grove_Subtidal" ~ Lat,
                             sampleID == "Bamfield_Subtidal Exposed" ~ Lat + 3.5,
                             sampleID == "Bamfield_Subtidal Intermediate" ~ Lat + 0.5,
                             sampleID == "Bamfield_Subtidal Protected" ~ Lat - 2.5,
                             sampleID == "Gabriola Island_Subtidal" ~ Lat + 2,
                             sampleID == "Prince William Sound_Intertidal" ~ Lat - 2,
                             sampleID == "Prince William Sound_Subtidal" ~ Lat - 2,
                             sampleID == "San Juan Islands_Subtidal Vadas" ~ Lat + 1.5,
                             sampleID == "San Juan Islands_Subtidal Mauzey" ~ Lat - 1.5,
                             sampleID == "Outer Coast_Intertidal Paine" ~ Lat - 5,
                             sampleID == "Outer Coast_Intertidal Dayton" ~ Lat - 7.5))
# summarise totals of each prey type per site 
PycnoDiet_step5 <- PycnoDiet_step4 %>%
  group_by(sampleID, item, new_lat, new_long, Lat, Lon, N) %>%
  summarise(value = sum(value))
# standardize all prey values to percent of total
PycnoDiet_step6 <- PycnoDiet_step5 %>%
  group_by(sampleID) %>%
  summarise(total = sum(value))
PycnoDiet_step7 <- PycnoDiet_step5 %>%
  left_join(PycnoDiet_step6, by = "sampleID")
PycnoDiet_step8 <- PycnoDiet_step7 %>%
  mutate(value = 100 * (value / total))
# round value column to 2 digits
PycnoDiet_step8$value <- round(PycnoDiet_step8$value, digits = 2)
# create subtidal intertidal column
PycnoDiet_step9 <- PycnoDiet_step8 %>%
  mutate(Depth = ifelse(sampleID %like% "Subtidal", "Subtidal", "Intertidal"))


  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DENSITY FIGURE                                                               ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# make a shiny app that shows the expected state for historic and current values
# with a slider that changes urchin recruitment

# calculate mean of means (SE = SD of the dist? Right?)

PycnoDensity_step1 <- PycnoDensity %>%
  group_by(populationPhase) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(where(is.character), ~"Overall Mean"))) %>%
  ungroup()
PycnoDensity_step1[PycnoDensity_step1 == "0"] <- 0.000005 
PycnoDensity_step1[24:25, 9] <- 0
PycnoDensity_step1[14,9] <- 0.0000001
PycnoDensity_step1[18, 9] <- 0.0000001
PycnoDensity_step1[22, 9] <- 0.0000001
PycnoDensity_step1[13, 9] <- 0.0000001


PycnoHistCurrPlot <- PycnoDensity_step1 %>%
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
                                         "Baja California",
                                         "Overall Mean"))) %>% 
  ggplot(aes(Region, meanDensityM2*10000, color = populationPhase)) +
  geom_rect(xmin = 12.5, xmax = 13.5, ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_hline(yintercept = 0.05, color = "dark grey", linetype = "solid", size = 1) +
  geom_pointrange(size = 1, aes(ymin = (meanDensityM2*10000)-(SEDensityM2*10000), ymax = (meanDensityM2*10000)+(SEDensityM2*10000)), 
                  position=position_jitter(width=0.2), 
                  linetype='solid') +
  geom_hline(yintercept = 6, color = "red", linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dotted", size = 1) +
  scale_color_viridis(name = "Population Phase", discrete = TRUE, begin = 0.3, end = 0.7, option = "F") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_y_continuous(trans= "log10", position = "right", breaks= c(seq(0.1,0.9, by = 0.1), seq(1,6, by = 1), seq(60,1400, by = 250)))+
  theme(axis.text.x = element_text(angle = 270, hjust=0, vjust = 0)) +
  ylab(expression(paste("Mean Pycnopodia Density (no. 100 ", m^-2, ")"))) +
  xlab(label = NULL) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))); PycnoHistCurrPlot 


#### DAN'S DENSITY FIGURE (2022-05-17)

library(fields)
library(metR)

### read in output

out <- read.csv("~/Git/Pycno_Purps/Output/initial_runs.csv")

### generate the figure
dens_plot <- ggplot(aes(urch_rec,pycno_dens*100, fill= urch_dens),data= out)+geom_tile()+
  scale_y_continuous(trans= "log10",expand= c(0,0), breaks= c(seq(0.1,0.9, by = 0.1), seq(1,6, by = 1)))+
  scale_fill_viridis(option = "H",trans= "sqrt",breaks= seq(0,30,by =5), 
                       name= expression(paste("Urchin Density (", m^-2, ")")))+
  ylab(expression(paste(italic("Pycnopodia"), " density (no. 100", m^-2, ")")))+
  theme_bw()+
  theme(legend.position= "top",
        legend.key.width = unit(2,"cm"),
        panel.background= element_blank(),
        plot.background= element_blank())+
  metR::geom_contour2(aes(z = urch_dens, label = stat(level)),breaks= seq(2,4,by =1),skip=0)+
  metR::geom_contour2(aes(z = urch_dens, label = stat(level)),breaks= seq(0,30,by =5),skip= 0)+
  guides(fill= guide_colourbar(title.position="top",title.hjust= 0.5))+
  xlab(expression(paste("Recruitment (no. ", yr^-1, m^-2,")")))+
  scale_x_continuous(expand= c(0,0))+
  annotation_logticks(sides = "l",base= 10,colour= "white") +
  geom_hline(yintercept = as.numeric(PycnoDensity_step1[16,7]*10000), color = "White", linetype = "dashed", size = 1) +
  geom_hline(yintercept = as.numeric(PycnoDensity_step1[23,7]*10000), color = "White", linetype = "dashed", size = 1) +
  annotate("text", x = 0.5, y = 5, color = "White", label = "WA Coast") +
  annotate("text", x = 1, y = 3, color = "white", label = "Central CA"); dens_plot



# PUT THEM TOGETHER

# create lines across plots:
insetlines <- tibble(y = c(0.2, 9.1, 2.2, 5.27, 6, 1.8, 7.5, 1.5),
                     x = c(0, 0, 5, 5, 2.7, 3.3, 1.5, 1.7), 
                     z = c("A", "B", "A", "B", "B", "A", "B", "A"))
lineplot <- insetlines %>%
  ggplot(aes(x, y, group = z)) +
  geom_point(size = 0.1, color = "white") +
  geom_smooth(method = 'lm', formula = y ~ poly(x, degree = 2), se = FALSE, 
              linetype = "dotted", color = "red", size = 1) +
  #geom_line(linetype = "dotted", color = "red", size = 1) +
  scale_x_continuous(limits = c(0,5)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_void(); lineplot

Figure1 <- ggarrange(dens_plot, lineplot, PycnoHistCurrPlot, 
                     nrow = 1, widths = c(1, 0.1, 1),
                     labels = c("a", "", "b"))
Figure1
# best size: ~1800x850

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DIET FIGURE                                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# set for 1600 x 1450

# create values of N to add to the plot
annotations <- data.frame (N  = c("72", "425", "311", "162", "67", "120", "45", "7", "24", "102", "51", "93", "41"),
                           Lat = c(56.1, 56.1, 53.8, 52.3, 52.5, 49, 46, 53.7, 50, 47, 43, 40.5, 36.8),
                           Lon = c(-149, -146, -138.5, -135.8, -132.8, -132.7, -132, -125, -118, -117.5, -130.1, -129, -127.8))


world <- map_data("world")
NorthAm <- world %>%
  filter(region %in% c("USA", "Canada", "Mexico")) %>%
  filter(-158 < long & long < -110) %>%
  filter(25 < lat & lat < 75)
ggplot(NorthAm, aes(long, lat)) +
  geom_map(map=NorthAm, aes(map_id=region), fill="grey80", color="grey25") +
  ylim(c(33,65)) +
  xlim(c(-153, -115)) +
  theme_bw() +
  geom_segment(data = PycnoDiet_step9,
               mapping = aes(x = new_long, xend = Lon, y = new_lat, yend = Lat),
               color = "black",
               size = 1) +
  geom_point(data = PycnoDiet_step9, aes(new_long, new_lat, color = Depth), size = 40) +
  guides(color = guide_legend(override.aes = list(size = 15))) +
  geom_scatterpie(data = PycnoDiet_step9,
                  aes(new_long, new_lat, r = 1.45),
                  cols = "item", long_format= TRUE) +
  scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.9, direction = -1) +
  scale_fill_viridis(discrete = TRUE, option = "H", begin = 0.2, name = "Prey Item") +
  theme(legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=16)) +
  geom_text(data = annotations, aes(x = Lon, y = Lat, label = N), size = 10) +
  annotate("text", x = -120, y = 63, label = "N", size = 8) +
  annotate("segment", x = -120, xend = -120, y = 59, yend = 62, arrow=arrow(length=unit(0.2,"cm"))) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(size=14),
        axis.title = element_text(size=16)) +
  coord_fixed()

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

test <- PycnoDiet_step8 %>%
  filter(sampleID %in% c("Prince William Sound_Subtidal", "Prince William Sound_Intertidal"))
