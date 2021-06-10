########## SURVEY COMPARISON PROJECT PRESENCE/ABSENCE COMPARISON ##########
########## 
##########
# This file creates a dataframe outlining the total number of sessions each 
# species observed in SVC and roving surveys was present in. Only a subset of 
# large, predatory reef fish species are extracted as these were the only group
# surveyed for by roving surveys. The dataframe created is used to compare 
# presence recordings between SVC and roving surveys within and across species
# in order to explore if significant differences are present and how these
# differences compare to differences in density across species explored 
# previously. 
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

# aggregate species by sum
SVC_presence <- aggregate(.~species, SVC_presence, sum)
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
SVCroving_presence_wide <- spread(SVCroving_presence_long, species, presence)

# change column names
SVC_presence <- rename(SVC_presence, SVC_presence = presence)
pred_presence <- rename(pred_presence, pred_presence = presence)

# remove survey columns
SVC_presence <- SVC_presence[,1:2]
pred_presence <- pred_presence[,1:2]

# join SVC and roving presence
SVCroving_presence_chi <- join(SVC_presence, pred_presence, by = NULL, 
                           type = "full", match = "all")


# Barplot ======================================================================

# The following creates a barplot of the number of sessions each species was 
# recorded in between SVC and roving surveys. 

# barplot
SVCpred_presabs_bar <- ggplot(SVCroving_presence_long, aes(x = species, 
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
ggsave(here("./visuals/SVCpred_presabs_barplot.png"), SVCpred_presabs_bar)


# T-Test on Species' Presence ==================================================

# The following performs a two-sample t-test to test the effect of survey type
# on the number of sessions species were recorded in across all species.

# variance calculation
var(SVCroving_presence_chi$SVC_presence) # = 153.6957
var(SVCroving_presence_chi$pred_presence) # = 318.419

# F-test for equal variance
var.test(presence ~ survey, data = SVCroving_presence_long)
# p = 0.09472; no significant difference between variances 

# SVC presence normal distribution check
shapiro.test(SVCroving_presence_chi$SVC_presence) # p = 9.729e-07
hist(SVCroving_presence_chi$SVC_presence)
# non-normal

# roving presence normal distribution check
shapiro.test(SVCroving_presence_chi$pred_presence) # p = 0.001012
hist(SVCroving_presence_chi$pred_presence)
# non-normal

# t-test
t.test <- t.test(presence ~ survey, data = SVCroving_presence_long, 
                 var.equal = TRUE)
t.test
# t = 2.1688, df = 44, p-value = 0.03554


# Chi-Square Test: Red Grouper =================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# red grouper across sessions between SVC and roving surveys. 

# extract roving sessions
pred_sessions <- pred_fish[,1]

# remove duplicate sessions 
pred_sessions <- unique(pred_sessions[,1])

# add species column for red grouper
red_grouper <- pred_sessions
red_grouper$species <- "red grouper"

# add presence column
SVC_fish$SVC_presence <- 1
pred_fish$pred_presence <- 1

# join red grouper presence values to each session
red_grouper <- join(red_grouper, SVC_fish, by = NULL, type = "left", 
                    match = "first")
red_grouper <- join(red_grouper, pred_fish, by = NULL, type = "left", 
                    match = "first")

# replace NA values with 0
red_grouper[is.na(red_grouper)] <- 0

# convert to table
red_grouper_chi <- table(red_grouper$SVC_presence, red_grouper$pred_presence)

# chi-square test
chisq.test(red_grouper_chi)
# X-squared = 17.72, df = 1, p-value = 2.559e-05


# Chi-Square Test: Black Grouper ===============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# black grouper across sessions between SVC and roving surveys. 

# add species column for black grouper
black_grouper <- pred_sessions
black_grouper$species <- "black grouper"

# join black grouper presence values to each session
black_grouper <- join(black_grouper, SVC_fish, by = NULL, type = "left", 
                    match = "first")
black_grouper <- join(black_grouper, pred_fish, by = NULL, type = "left", 
                    match = "first")

# replace NA values with 0
black_grouper[is.na(black_grouper)] <- 0

# convert to table
black_grouper_chi <- table(black_grouper$SVC_presence, 
                           black_grouper$pred_presence)

# chi-square test
chisq.test(black_grouper_chi)
# X-squared = 5.33, df = 1, p-value = 0.02096


# Chi-Square Test: Mutton Snapper ==============================================

# The following performs a Chi-Square test on presence/absence recordings of 
# mutton snapper across sessions between SVC and roving surveys. 

# add species column for mutton snapper
mutton_snapper <- pred_sessions
mutton_snapper$species <- "mutton snapper"

# join mutton snapper presence values to each session
mutton_snapper <- join(mutton_snapper, SVC_fish, by = NULL, type = "left", 
                      match = "first")
mutton_snapper <- join(mutton_snapper, pred_fish, by = NULL, type = "left", 
                      match = "first")

# replace NA values with 0
mutton_snapper[is.na(mutton_snapper)] <- 0

# convert to table
mutton_snapper_chi <- table(mutton_snapper$SVC_presence, 
                           mutton_snapper$pred_presence)

# chi-square test
chisq.test(mutton_snapper_chi)
# X-squared = 15.174, df = 1, p-value = 9.806e-05


# Chi-Square Test: Lionfish ====================================================

# The following performs a Chi-Square test on presence/absence recordings of 
# lionfish across sessions between SVC and roving surveys. 

# add species column for lionfish
lionfish <- pred_sessions
lionfish$species <- "lionfish"

# join lionfish presence values to each session
lionfish <- join(lionfish, SVC_fish, by = NULL, type = "left", 
                       match = "first")
lionfish <- join(lionfish, pred_fish, by = NULL, type = "left", 
                       match = "first")

# replace NA values with 0
lionfish[is.na(lionfish)] <- 0

# convert to table
lionfish_chi <- table(lionfish$SVC_presence, 
                            lionfish$pred_presence)

# chi-square test
chisq.test(lionfish_chi)
# X-squared = 0.62845, df = 1, p-value = 0.4279 
