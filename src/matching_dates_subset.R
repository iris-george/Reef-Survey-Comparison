########## SURVEY COMPARISON PROJECT MATCHING DATE SUBSET ##########
########## 
##########
# This file subsets available reef survey data to only sessions of matching date
# across all three survey types: SVC, transect, and roving. Identical analyses
# are then run on this subset to determine if the inclusion of non-matching
# dates is impacting results. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-03
##########
##########


# Set-Up =======================================================================

# packages
library(tidyverse)
library(nlme)
library(car)
library(arm)
library(MuMIn)
library(here)

# data
SVC_date <- read_csv(here("./clean_data/SVC_date_time.csv"))
prey_date <- read_csv(here("./clean_data/prey_date_time.csv"))
pred_date <- read_csv(here("./clean_data/pred_date_time.csv"))
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_model_data.csv"))
SVCpred_model_data <- read_csv(here("./dataframes/SVCpred_model_data.csv"))


# Find Matching Dives ==========================================================

# aggregate SVC dates
SVC_surveys <- SVC_date %>% group_by(session, site, SVC_date) %>% 
  summarise_each(funs(mean))

# aggregate transect dates
prey_surveys <- prey_date %>% group_by(session, site, prey_date) %>% 
  summarise_each(funs(mean))

# aggregate roving dates
pred_surveys <- pred_date %>% group_by(session, site, pred_date) %>% 
  summarise_each(funs(mean))

# join dates and times 
SVCprey_dates <- join(SVC_surveys, prey_surveys, by = NULL, type = "full", 
                      match = "first")
survey_dates <- join(SVCprey_dates, pred_surveys, by = NULL, type = "full", 
                     match = "first")

# export csv
write_csv(survey_dates, here("./clean_data/survey_dates.csv"))


# Removing Un-Matched Dives from Dataframes ====================================

# removing 6 out of 118 total sessions

# remove session 178
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$session !=178,]
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$session !=178,]

# remove session 179
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$session !=179,]
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$session !=179,]

# remove session 180
SVCprey_model_data <- SVCprey_model_data[SVCprey_model_data$session !=180,]
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$session !=180,]

# remove session 189 from SVC vs. roving
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$session !=189,]

# remove session 190 from SVC vs. roving
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$session !=190,]

# remove session 243 from SVC vs. roving
SVCpred_model_data <- SVCpred_model_data[SVCpred_model_data$session !=243,]


# SVC vs. Transect Surveys: Global Linear Mixed Effects Model ==================

# global lme
SVCprey_subset_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                        size_bin*colouration2+nocturnal+position+max_length+
                        behavior+cryptic_behaviour+average_depth+size_bin*shape, 
                      random = list(~1|site, ~1|species_order), 
                      SVCprey_model_data) 

# model summary
summary(SVCprey_subset_global)
# AIC = 28516.79
# sig pos predictors: octocoral, stony, colourful, neutral, max length, 
# solitary, size_bin*elongated, size_bin*fusiform, size_bin*globiform
# sig neg predictors: habitat, cryptic behaviour, elongated, fusiform, globiform

# summary of all sessions model:
# AIC = 29598.41
# sig pos covariates = octocoral, stony, size bin, colourful, neutral, 
# silvering, max length, shoaling, size bin*elongated, size bin*fusiform
# sig neg covariates = habitat, cryptic behaviour, elongated, fusiform, 
# size bin*colourful, size bin*neutral, size bin*silvering

# changes: decrease in AIC with subset, size bin insignificant, silvering
# insignificant solitary significant and shoaling insignificant, globiform 
# significant, globiform*size bin significant, no size bin*colouration 
# significance 

# VIF values
vif(SVCprey_subset_global)
# behaviour GVIF = 5.463450


# SVC vs. Roving Surveys: Global Linear Mixed Effects Model ====================

# global lme
SVCpred_subset_global <- lme(log_difference~habitat+octocoral+stony+relief_cm+
                             size_bin+nocturnal+position+max_length+
                             colouration2+cryptic_behaviour+average_depth+shape, 
                             random = list(~1|site, ~1|species_order), 
                             na.action = na.omit, SVCpred_model_data) 

# model summary
summary(SVCpred_subset_global)
# AIC = 1572.68
# sig pos predictors: maximum length, elongated, fusiform, silvering
# sig neg predictors: size bin

# summary of all sessions model:
# AIC = 1683.789 --> better fit than previous model without shape
# sig pos covariates = silvering, fusiform
# sig neg covariates = stony, size bin

# changes: decrease in AIC with subset, max length significant, elongated
# significant, stony insignificant 

# VIF values
vif(SVCpred_subset_global)
# habitat GVIF = 6.989918
# position GVIF = 6.905598
# colouration GVIF = 22.841205
# average depth GVIF = 5.119935
# shape GVIF = 8.876307
