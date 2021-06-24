########## SURVEY COMPARISON PROJECT DENSITY COMPARISON ##########
########## 
##########
# This file creates a dataframe outlining the total number of sessions each 
# species observed in SVC and roving surveys was present in. Only a subset of 
# large, predatory reef fish species are extracted as these were the only group
# surveyed for by roving surveys. The dataframe created is used to compare 
# presence recordings between SVC and roving surveys across species in order to 
# explore if significant differences are present and how these differences 
# compare to differences in density across species explored previously. 
# A barplot of the number of sessions present for each species in SVC and roving
# surveys is also created. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-24
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(here)

# data
SVC_data <- read_csv(here("./clean_data/SVC_data.csv"))
prey_fish <- read_csv(here("./clean_data/prey_fish_data.csv"))
prey_meta <- read_csv(here("./clean_data/prey_metadata.csv"))


# Obtain Average Densities =====================================================

# The following compiles average density values for each species between SVC
# and transect surveys 

# filter for SVC focal species
SVC_data <- filter(SVC_data, species == "white grunt"|
                     species == "bluestriped grunt"|species == "hogfish"|
                     species == "mutton snapper"|species == "yellowtail snapper"|
                     species == "yellowtail snapper"|species == "red grouper"|
                     species == "black grouper")
prey_fish <- filter(prey_fish, species == "white grunt"|
                      species == "bluestriped grunt"|species == "hogfish"|
                      species == "mutton snapper"|species == "yellowtail snapper"|
                      species == "yellowtail snapper"|species == "red grouper"|
                      species == "black grouper")

# select transect species and session columns
prey_species <- prey_fish[,c(1,3)]

# aggregate species by session
prey_species <- prey_fish %>% group_by(session, species) %>% tally()

# rename abundance column
prey_species <- prey_species %>% rename(prey_abundance = n)

# select transect session and area columns
prey_area <- prey_meta[,c(1,15)]

# aggregate area by session
prey_area <- aggregate(.~session, prey_area, sum)

# join area to fish data
prey_density <- join(prey_species, prey_area, by = NULL, type = "full", 
                     match = "all")

# select SVC session, species, and abundance columns
SVC_density <- SVC_data[,c(1,12,37,38)]

# transect density calculation
prey_density$prey_density <- 
  (prey_density$prey_abundance)/(prey_density$prey_tran_area)

# SVC density calculation 
SVC_density$SVC_density <- 
  (SVC_density$SVC_abundance)/(SVC_density$SVC_cylinder_area)

# add survey column
prey_density$survey <- "transect"
SVC_density$survey <- "SVC"

# select session, survey, species, and density columns
prey_density <- prey_density[,c(1,6,2,5)]
SVC_density <- SVC_density[,c(1,6,3,5)]

# rename density columns
prey_density <- rename(prey_density, density = prey_density)
SVC_density <- rename(SVC_density, density = SVC_density)

# bind dataframes
SVCprey_density <- bind_rows(SVC_density, prey_density)

# remove NA values
SVCprey_density <- na.omit(SVCprey_density)


# ANOVA Across Species =========================================================

# The following performs an ANOVA across all SVC focal species to determine
# differences in their average densities between SVC and transect surveys. 

# ANOVA
SVCprey_density_anova <- aov(density~survey, SVCprey_density)

# Tukey Test 
TukeyHSD(SVCprey_density_anova)


# White Grunt ANOVA ============================================================

# The following performs an ANOVA on the average density of white grunts 
# observed between SVC and transect surveys. 

# filter for white grunts
white_grunt_density <- filter(SVCprey_density, species == "white grunt")

# ANOVA
white_grunt_anova <- aov(density~survey, white_grunt_density)

# Tukey Test
TukeyHSD(white_grunt_anova)


# Bluestriped Grunt ANOVA ======================================================

# The following performs an ANOVA on the average density of bluestriped grunts 
# observed between SVC and transect surveys. 

# filter for bluestriped grunts
bluestriped_grunt_density <- filter(SVCprey_density, 
                                    species == "bluestriped grunt")

# ANOVA
bluestriped_grunt_anova <- aov(density~survey, bluestriped_grunt_density)

# Tukey Test
TukeyHSD(bluestriped_grunt_anova)


# Hogfish ANOVA ================================================================

# The following performs an ANOVA on the average density of hogfish 
# observed between SVC and transect surveys. 

# filter for hogfish
hogfish_density <- filter(SVCprey_density, species == "hogfish")

# ANOVA
hogfish_anova <- aov(density~survey, hogfish_density)

# Tukey Test
TukeyHSD(hogfish_anova)


# Mutton Snapper ANOVA =========================================================

# The following performs an ANOVA on the average density of mutton snapper 
# observed between SVC and transect surveys. 

# filter for mutton snapper
mutton_snapper_density <- filter(SVCprey_density, species == "mutton snapper")

# ANOVA
mutton_snapper_anova <- aov(density~survey, mutton_snapper_density)

# Tukey Test
TukeyHSD(mutton_snapper_anova)


# Gray Snapper ANOVA ===========================================================

# The following performs an ANOVA on the average density of gray snapper 
# observed between SVC and transect surveys. 

# filter for gray snapper
gray_snapper_density <- filter(SVCprey_density, species == "gray snapper")

# ANOVA
gray_snapper_anova <- aov(density~survey, gray_snapper_density)

# Tukey Test
TukeyHSD(gray_snapper_anova)


# Yellowtail Snapper ANOVA =====================================================

# The following performs an ANOVA on the average density of yellowtail snapper 
# observed between SVC and transect surveys. 

# filter for yellowtail snapper
yellowtail_snapper_density <- filter(SVCprey_density, 
                                     species == "yellowtail snapper")

# ANOVA
yellowtail_snapper_anova <- aov(density~survey, yellowtail_snapper_density)

# Tukey Test
TukeyHSD(yellowtail_snapper_anova)


# Red Grouper ANOVA ============================================================

# The following performs an ANOVA on the average density of red grouper 
# observed between SVC and transect surveys. 

# filter for red grouper
red_grouper_density <- filter(SVCprey_density, species == "red grouper")

# ANOVA
red_grouper_anova <- aov(density~survey, red_grouper_density)

# Tukey Test
TukeyHSD(red_grouper_anova)


# Black Grouper ANOVA ==========================================================

# The following performs an ANOVA on the average density of black grouper 
# observed between SVC and transect surveys. 

# filter for black grouper
black_grouper_density <- filter(SVCprey_density, species == "black grouper")

# ANOVA
black_grouper_anova <- aov(density~survey, black_grouper_density)

# Tukey Test
TukeyHSD(black_grouper_anova)


# Density Barplot ==============================================================

# The following creates a barplot of the average density of each SVC focal 
# species between SVC and transect surveys.

# remove session column
SVCprey_density_bar <- SVCprey_density[,2:4]

# aggregate by species 
SVCprey_density_bar <- aggregate(.~species+survey, SVCprey_density_bar, mean)

# sort by species
SVCprey_density_bar <- SVCprey_density_bar[order(SVCprey_density_bar$species),]

# barplot
SVCprey_density_barplot <- ggplot(SVCprey_density_bar, aes(x = species, 
                           y = density, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() +
  xlab("Species") + 
  ylab("Average Density") +
  scale_fill_manual(values = c("lemonchiffon1", "navyblue")) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) 
ggsave(here("./visuals/SVCprey_density_bar.png"), SVCprey_density_barplot)