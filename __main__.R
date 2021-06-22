########## SURVEY COMPARISON PROJECT MAIN FILE ##########
########## 
##########
# This main file calls and runs all subsequent R files in this analysis.
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-07
##########
##########


# Set-Up =======================================================================
library(here)


# Fish Dataframe Creation ======================================================

# This script reads in cleaned survey data from SVC, transect, and roving 
# surveys and creates a constant format between the three types before joining
# them into a single dataframe with all fish observations, grouped by species
# and size bin for each session. 

source(here("./src/fish_dataframes.R"))


# Reclassifying Size Bins ======================================================

# This script reclassifies the size bins created in the initial fish dataframe
# to utilize the mean lengths in each of the six bins (0-5cm, 5-10cm, 10-15cm, 
# 15-20cm, 20-30cm, and >30cm) as opposed to arbitrary values for further 
# analyses. 

source(here("./src/sizebin_reclassification.R"))


# Model Dataframe Creation =====================================================

# This script joins survey metadata, habitat trait, and species' trait values 
# onto the cleaned fish dataframe before splitting it into the two dataframes
# used in further analyses. One dataframe examines fish observations within 
# SVC and transect surveys, while the other explores observations in SVC and 
# roving surveys. The script additionally calculates log-transformed density 
# differences between the survey types for each fish observation as well as 
# average depths between the surveys. 

source(here("./src/model_dataframes.R"))


# Model Selection ==============================================================

# This script runs linear mixed effects models on the dataframes comparing log 
# density differences between survey types which incorporate all habitat, trait, 
# and survey characteristics present in the dataframes. A global model is 
# created for each dataframe (SVC vs. transect surveys and SVC vs. roving 
# surveys) utilizing all appropriate traits and interactions, and a dredge is 
# performed on each to examine the models of the highest likelihood. Included 
# are tests of predictor collinearity and model fit. 

source(here("./src/model_selection.R"))


# Chi-Square Analysis: All Species =============================================

# This script creates a dataframe of presence/absence recordings of species
# across sessions for SVC and roving surveys in order to compare differences in 
# recordings. A Chi-Square test is performed to determine whether a difference
# between SVC and roving presence recordings exists. A barplot is created of 
# presence recordings in sessions across the surveys for every species. 

source(here("./src/SVCpred_chisquare.R"))


# Chi-Square Analysis: Individual Species ======================================

# This script creates a presence/absence recording for each species in each 
# session for SVC and roving surveys. A Chi-Square test is then performed on 
# each species in order to determine if a difference in their recordings exists
# between SVC and roving surveys. 

source(here("./src/SVCpred_species_presence.R"))


# Covariate Plots ==============================================================

# This script creates boxplots and scatterplots of significant covariates from
# the global linear mixed effects models and the model average performed on 
# the dredging results. 

source(here("./src/covariate_plots.R"))


# Density Barplots =============================================================

# This script creates and saves barplots of the density differences between 
# survey types averaged across families (for SVC compared to transect surveys) 
# and species (for SVC compared to roving surveys). 

source(here("./src/density_barplots.R"))