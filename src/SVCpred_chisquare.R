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

# remove sessions with un-matched dates between surveys
SVC_fish <- SVC_fish[SVC_fish$session !=178,]
SVC_fish <- SVC_fish[SVC_fish$session !=179,]
SVC_fish <- SVC_fish[SVC_fish$session !=180,]
pred_fish <- pred_fish[pred_fish$session !=178,]
pred_fish <- pred_fish[pred_fish$session !=179,]
pred_fish <- pred_fish[pred_fish$session !=180,]

# remove sessions with no roving data
SVC_fish <- SVC_fish[SVC_fish$session !=264,]
SVC_fish <- SVC_fish[SVC_fish$session !=265,]
SVC_fish <- SVC_fish[SVC_fish$session !=266,]
SVC_fish <- SVC_fish[SVC_fish$session !=267,]
SVC_fish <- SVC_fish[SVC_fish$session !=268,]
SVC_fish <- SVC_fish[SVC_fish$session !=269,]
SVC_fish <- SVC_fish[SVC_fish$session !=270,]
SVC_fish <- SVC_fish[SVC_fish$session !=271,]
SVC_fish <- SVC_fish[SVC_fish$session !=272,]
SVC_fish <- SVC_fish[SVC_fish$session !=273,]
SVC_fish <- SVC_fish[SVC_fish$session !=274,]

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

# take out 0 rows
SVCroving_presence <- SVCroving_presence[SVCroving_presence$presence !=0,]


# Chi-Square Test ==============================================================

# The following performs a Chi-Square Test on the number of sessions present
# across species between SVC and roving surveys to determine if an overall
# significant difference between survey types exists.

# convert dataframe to table
SVCroving_chi <- table(SVCroving_presence$species, 
                       SVCroving_presence$survey)

# Chi-Square Test
SVCroving_full_chi <- chisq.test(SVCroving_chi)

# save Chi-Square results
saveRDS(SVCroving_full_chi, here("./outputs/SVCpred_full_chi.rds"))


# Chi-Square Test: Species Subset ==============================================

# remove species present in <10% of sessions 
SVCroving_presence_subset <- SVCroving_presence[SVCroving_presence$species 
                                                !="goldentail moray",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="sharptail eel",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="amberjack",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="red hind",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="rock hind",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="greater soapfish",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="scamp",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="black margate",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="cubera snapper",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="barracuda",]
SVCroving_presence_subset <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                             !="spotted scorpionfish",]

# convert dataframe to table
SVCroving_chi_subset <- table(SVCroving_presence_subset$species, 
                       SVCroving_presence_subset$survey)

# Chi-Square Test
chisq.test(SVCroving_chi_subset)

# remove purplemouth moray (>10% of sessions but 0 presence on SVC)
SVCroving_presence_subset2 <- 
  SVCroving_presence_subset[SVCroving_presence_subset$species 
                            !="purplemouth moray",]

# convert dataframe to table
SVCroving_chi_subset2 <- table(SVCroving_presence_subset2$species, 
                              SVCroving_presence_subset2$survey)

# Chi-Square Test
SVCroving_chi_sub <- chisq.test(SVCroving_chi_subset2)

# save Chi-Square results
saveRDS(SVCroving_chi_sub, here("./outputs/SVCpred_chi_subset.rds"))


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

# sort by species
SVCroving_presence_bar <- 
  SVCroving_presence_bar[order(SVCroving_presence_bar$species),]

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
ggsave(here("./visuals/SVCpred_presabs_bar.png"), SVCpred_presabs_bar)