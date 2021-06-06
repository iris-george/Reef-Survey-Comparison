########## SURVEY COMPARISON PROJECT MODEL CREATION AND SELECTION ##########
########## 
##########
# This file creates a dataframe outlining the total number of sessions each 
# species observed in SVC and roving surveys was present in. Only a subset of 
# large, predatory reef fish species are extracted as these were the only group
# surveyed for by roving surveys. The dataframe created here will be used to 
# compare 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-04
##########
##########


# Set-Up =======================================================================

# packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(here)

# data
SVC_fish_data <- read_csv(here("./clean_data/SVC_data.csv"))
pred_fish_data <- read_csv(here("./clean_data/pred_fish_data.csv"))
traits <- read_csv(here("./clean_data/fish_traits.csv"))


# Obtain Presence Values =======================================================

# The following compiles all presence observations for each species.

# extract SVC observations
SVC_fish <- SVC_fish_data[,37]

# extract roving observations 
pred_fish <- pred_fish_data[,c(1,3)]

# remove duplicate rows 
pred_presence <- unique(pred_fish[,1:2])

# remove session column
pred_presence <- pred_presence[,2]

# add presence column
SVC_fish$presence <- 1
pred_presence$presence <- 1

# aggregate species by sum
SVC_presence <- aggregate(.~species, SVC_fish, sum)
pred_presence <- aggregate(.~species, pred_presence, sum)

# extract roving presence traits column
pred_fish_list <- traits[,c(4,7)]

# change column name
pred_fish_list <- rename(pred_fish_list, species = common_name)

# join roving presence to fish lists
SVC_presence <- join(SVC_presence, pred_fish_list, by = NULL, type = "full", 
                     match = "all")
pred_presence <- join(pred_presence, pred_fish_list, by = NULL, type = "full", 
                     match = "all")

# replace NA values with 0
SVC_presence[is.na(SVC_presence)] <- 0
pred_presence[is.na(pred_presence)] <- 0

# filter for roving survey species
SVC_presence <- filter(SVC_presence, predator_presence == 1) 
pred_presence <- filter(pred_presence, predator_presence == 1) 

# remove roving presence column
SVC_presence <- SVC_presence[,1:2]
pred_presence <- pred_presence[1:2]

# remove gray snapper 
SVC_presence <- SVC_presence[SVC_presence$species !="gray snapper",]
pred_presence <- pred_presence[pred_presence$species !="gray snapper",]

# add "survey" column
SVC_presence$survey <- "SVC"
pred_presence$survey <- "roving"

# bind SVC and roving presence values together 
SVCroving_presence_long <- bind_rows(SVC_presence, pred_presence)

# take from long to wide
SVCroving_presence_wide <- spread(SVCroving_presence, species, presence)

# change column names
SVC_presence <- rename(SVC_presence, SVC_presence = presence)
pred_presence <- rename(pred_presence, pred_presence = presence)

# remove survey columns
SVC_presence <- SVC_presence[,1:2]
pred_presence <- pred_presence[,1:2]

# join SVC and roving presence
SVCroving_presence_chi <- join(SVC_presence, pred_presence, by = NULL, 
                           type = "full", match = "all")

# convert to matrix
SVCroving_presence_chi <- matrix(SVCroving_presence_chi, 23, 3)


# Chi-Square Test ==============================================================

SVCroving_presence2 <- table(SVCroving_presence)
chisq <- chisq.test(SVCroving_presence)


# ANOVAs on Species' Presence ==================================================

for (species in SVCroving_presence$species) {
  aov(presence ~ survey, data = SVCroving_presence)
}

for (i in 1:nrow(SVCroving_presence_chi)) {
  print(i)
  print(chisq.test(c(SVCroving_presence_chi[i,2], SVCroving_presence_chi[i,3])))
}


# Barplot ======================================================================

ggplot(SVCroving_presence_long, aes(x = species, y = presence, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() + xlab("Species") + 
  ylab("Number of Sessions Present") +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 
