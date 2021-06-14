########## SURVEY COMPARISON PROJECT PRESENCE/ABSENCE COMPARISON ##########
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
# DATE OF CREATION: 2021-06-14
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

# The following compiles all presence observations across sessions for each 
# species.

# extract SVC observations
SVC_fish <- SVC_fish_data[,c(1,37)]

# extract roving observations 
pred_fish <- pred_fish_data[,c(1,3)]

# remove duplicate rows 
pred_presence <- unique(pred_fish[,1:2])

# remove session column
SVC_presence <- SVC_fish[,2]
pred_presence <- pred_presence[,2]

# add presence column
SVC_presence$presence <- 1
pred_presence$presence <- 1

# extract roving presence traits column
pred_fish_list <- traits[,c(4,7)]

# change column name
pred_fish_list <- rename(pred_fish_list, species = common_name)

# select species recorded on roving surveys
pred_fish_list <- filter(pred_fish_list, pred_fish_list$predator_presence == 1)

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
pred_presence <- pred_presence[,1:2]

# remove gray snapper 
SVC_presence <- SVC_presence[SVC_presence$species !="gray snapper",]
pred_presence <- pred_presence[pred_presence$species !="gray snapper",]

# add "survey" column
SVC_presence$survey <- "SVC"
pred_presence$survey <- "roving"

# bind SVC and roving presence values together 
SVCroving_presence <- bind_rows(SVC_presence, pred_presence)


# Chi-Square Test ==============================================================

# The following performs a Chi-Square Test on the number of sessions present
# across species between SVC and roving surveys to determine if an overall
# significant difference between survey types exists.

# convert dataframe to table
SVCroving_chi <- table(SVCroving_presence_long$species, 
                       SVCroving_presence_long$survey)

# Chi-Square Test
chisq.test(table(SVCroving_presence_long$species, 
                 SVCroving_presence_long$survey))


# Barplot ======================================================================

# The following creates a barplot of the number of sessions each species was 
# recorded in between SVC and roving surveys. 

# re-order species
SVCprey_model_data$colouration <- 
  factor(SVCprey_model_data$colouration, 
         levels = c("camouflage", "neutral", "silvering", "colourful"))
SVCroving_presence$species <- factor(SVCroving_presence$species, 
                              levels = c("goldentail moray", "green moray", 
                              "purplemouth moray", "spotted moray", 
                              "sharptail eel", "amberjack", "black grouper", 
                              "coney", "gag grouper", "graysby", 
                              "nassau grouper", "red grouper", "red hind", 
                              "rock hind", "greater soapfish", "scamp", 
                              "black margate", "cubera snapper", 
                              "mutton snapper", "barracuda", "lionfish", 
                              "spotted scorpionfish", "trumpetfish"))

# aggregate presence by sum
SVCroving_presence_bar <- aggregate(.~species+survey, SVCroving_presence, sum)

# significance stars
significance1 <- data.frame(Group = c("goldentail moray", "red hind", 
                                     "greater soapfish", "spotted scorpionfish"),
                           Value = c(10,10,10,12))
significance2 <- data.frame(Group = c("purplemouth moray"),
                            Value = c(18))
significance3 <- data.frame(Group = c("green moray", "spotted moray", 
                                      "black grouper", "gag grouper", 
                                      "nassau grouper", "red grouper",  
                                      "lionfish", "trumpetfish"),
                            Value = c(23,60,43,30,24,38,40,30))

# * = 0.05, ** = 0.01, *** = 0..001

# barplot
SVCpred_presabs_bar <- ggplot(SVCroving_presence_bar, aes(x = species, 
                       y = presence, fill = survey)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() +
  xlab("Species") + 
  ylab("Number of Sessions Present") +
  scale_fill_manual(values = c("lemonchiffon1", "navyblue")) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 

SVCpred_presabs_bar + 
  geom_text(data = significance1, label = "*") +
  geom_text(data = significance2, label = "**") +
  geom_text(data = significance3, label = "***")
ggsave(here("./visuals/SVCpred_presabs_barplot.png"), SVCpred_presabs_bar)