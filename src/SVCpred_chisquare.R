########## SURVEY COMPARISON PROJECT SVC VS. ROVING PRESENCE ANALYSIS ##########
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


# Full Chi-Square Test =========================================================

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


# Focal Chi-Square Test ========================================================

# filter for focal species
focal_SVCroving_pres <- filter(SVCroving_presence, species == "mutton snapper"|
                                 species == "red grouper"|
                                 species == "black grouper")

# convert dataframe to table
focal_SVCroving_chi <- table(focal_SVCroving_pres$species, 
                       focal_SVCroving_pres$survey)

# Chi-Square Test
SVCroving_focal_chi <- chisq.test(focal_SVCroving_chi)


# Chi-Square Test: Black Grouper ===============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# black grouper across sessions between SVC and roving surveys. 

# extract SVC observations
SVC_fish <- SVC_fish_data[,c(1,37)]

# extract roving observations 
pred_fish <- pred_fish_data[,c(1,3)]

# extract roving sessions
pred_sessions <- pred_fish[,1]

# remove duplicate sessions 
pred_sessions <- unique(pred_sessions[,1])

# remove un-matching dates
pred_sessions <- pred_sessions[pred_sessions$session !=178,]
pred_sessions <- pred_sessions[pred_sessions$session !=179,]
pred_sessions <- pred_sessions[pred_sessions$session !=180,]

# add species column for black grouper
black_grouper <- pred_sessions
black_grouper$species <- "black grouper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join black grouper presence values to each session
black_grouper_SVC <- join(black_grouper, SVC_fish, by = NULL, type = "left", 
                          match = "first")
black_grouper_pred <- join(black_grouper, pred_fish, by = NULL, type = "left", 
                           match = "first")

# add survey column
black_grouper_SVC$survey <- "SVC"
black_grouper_pred$survey <- "roving"

# rename columns
black_grouper_SVC <- rename(black_grouper_SVC, presence = SVC_presence)
black_grouper_pred <- rename(black_grouper_pred, presence = pred_presence)

# bind
black_grouper_chi <- bind_rows(black_grouper_SVC, black_grouper_pred)

# replace NA values with 0
black_grouper_chi[is.na(black_grouper_chi)] <- 0

# convert to table
black_grouper_chi <- table(black_grouper_chi$presence, black_grouper_chi$survey)

# chi-square test
black_grouper_result <- chisq.test(black_grouper_chi) 
# X-squared = 18.047, df = 1, p-value = 2.155e-05

# save chi-square results
saveRDS(black_grouper_result, here("./outputs/black_grouper_chi.rds"))


# Chi-Square Test: Red Grouper =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# red grouper across sessions between SVC and roving surveys. 

# add species column for red grouper
red_grouper <- pred_sessions
red_grouper$species <- "red grouper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join red grouper presence values to each session
red_grouper_SVC <- join(red_grouper, SVC_fish, by = NULL, type = "left", 
                        match = "first")
red_grouper_pred <- join(red_grouper, pred_fish, by = NULL, type = "left", 
                         match = "first")

# add survey column
red_grouper_SVC$survey <- "SVC"
red_grouper_pred$survey <- "roving"

# rename columns
red_grouper_SVC <- rename(red_grouper_SVC, presence = SVC_presence)
red_grouper_pred <- rename(red_grouper_pred, presence = pred_presence)

# bind
red_grouper_chi <- bind_rows(red_grouper_SVC, red_grouper_pred)

# replace NA values with 0
red_grouper_chi[is.na(red_grouper_chi)] <- 0

# convert to table
red_grouper_chi <- table(red_grouper_chi$presence, red_grouper_chi$survey)

# chi-square test
red_grouper_result <- chisq.test(red_grouper_chi) 
# X-squared = 12.712, df = 1, p-value = 0.0003634

# save chi-square results
saveRDS(red_grouper_result, here("./outputs/red_grouper_chi.rds"))


# Chi-Square Test: Mutton Snapper ==============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# mutton snapper across sessions between SVC and roving surveys. 

# add species column for mutton snapper
mutton_snapper <- pred_sessions
mutton_snapper$species <- "mutton snapper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join mutton snapper presence values to each session
mutton_snapper_SVC <- join(mutton_snapper, SVC_fish, by = NULL, type = "left", 
                           match = "first")
mutton_snapper_pred <- join(mutton_snapper, pred_fish, by = NULL, type = "left", 
                            match = "first")

# add survey column
mutton_snapper_SVC$survey <- "SVC"
mutton_snapper_pred$survey <- "roving"

# rename columns
mutton_snapper_SVC <- rename(mutton_snapper_SVC, presence = SVC_presence)
mutton_snapper_pred <- rename(mutton_snapper_pred, presence = pred_presence)

# bind
mutton_snapper_chi <- bind_rows(mutton_snapper_SVC, mutton_snapper_pred)

# replace NA values with 0
mutton_snapper_chi[is.na(mutton_snapper_chi)] <- 0

# convert to table
mutton_snapper_chi <- table(mutton_snapper_chi$presence, 
                            mutton_snapper_chi$survey)

# chi-square test
mutton_snapper_result <- chisq.test(mutton_snapper_chi) 
# X-squared = 0.83979, df = 1, p-value = 0.3595

# save chi-square results
saveRDS(mutton_snapper_result, here("./outputs/mutton_snapper_chi.rds"))


# Barplot ======================================================================

# The following creates a barplot of the number of sessions each species was 
# recorded in between SVC and roving surveys. 

# re-order species
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

# add 0 species
goldentail_SVC <- data.frame("goldentail moray", "SVC", 0)
names(goldentail_SVC) <- c("species", "survey", "presence")
purplemouth_SVC <- data.frame("purplemouth moray", "SVC", 0)
names(purplemouth_SVC) <- c("species", "survey", "presence")
sharptail_SVC <- data.frame("sharptail eel", "SVC", 0)
names(sharptail_SVC) <- c("species", "survey", "presence")
amberjack_SVC <- data.frame("amberjack", "SVC", 0)
names(amberjack_SVC) <- c("species", "survey", "presence")
nassau_SVC <- data.frame("nassau grouper", "SVC", 0)
names(nassau_SVC) <- c("species", "survey", "presence")
redhind_SVC <- data.frame("red hind", "SVC", 0)
names(redhind_SVC) <- c("species", "survey", "presence")
soapfish_SVC <- data.frame("greater soapfish", "SVC", 0)
names(soapfish_SVC) <- c("species", "survey", "presence")
scamp_SVC <- data.frame("scamp", "SVC", 0)
names(scamp_SVC) <- c("species", "survey", "presence")
cubera_SVC <- data.frame("cubera snapper", "SVC", 0)
names(cubera_SVC) <- c("species", "survey", "presence")
SVCroving_presence_bar <- rbind(SVCroving_presence_bar, goldentail_SVC, 
                                purplemouth_SVC, sharptail_SVC, amberjack_SVC, 
                                nassau_SVC, redhind_SVC, soapfish_SVC, 
                                scamp_SVC, cubera_SVC)

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
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 24)) 
ggsave(here("./visuals/SVCpred_presabs_bar.png"), SVCpred_presabs_bar)